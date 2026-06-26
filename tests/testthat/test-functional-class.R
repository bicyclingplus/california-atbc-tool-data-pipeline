# Tests for the functional-class fix: `functional` (road hierarchy from OSM
# `highway`) is derived in process_osm_tags, carried through enrichment, added as
# a model predictor, and aggregated to nodes as the HIGHEST class of the touching
# links. Also covers the revised node aggregation of infra_type / strava /
# speed_limit / is_paved / crash_count_30m.

# --- process_osm_tags: highway -> functional -------------------------------

test_that("process_osm_tags derives functional from highway, keeping infra_type separate", {
  out <- process_osm_tags(fixture_osm())

  # functional is the road hierarchy, NOT bike infra.
  expect_true("functional" %in% names(out))
  expect_true("infra_type" %in% names(out))

  # fixture_osm highway: cycleway, residential, primary, footway, secondary
  expect_equal(
    out$functional,
    c("Local Road", "Local Road", "Major Road", "Local Road", "Minor Road")
  )

  # infra_type is a different axis (bike facility), unaffected by functional.
  expect_equal(out$infra_type[3], "bike_lane")     # primary + cycleway=lane
  expect_equal(out$infra_type[1], "separated_path")# cycleway highway

  # functional only ever takes the three road-hierarchy classes.
  expect_true(all(out$functional %in% c("Major Road", "Minor Road", "Local Road")))
})

test_that("process_osm_tags maps every highway tier to the right functional class", {
  osm <- st_sf(
    osm_id  = as.character(1:7),
    highway = c("motorway", "trunk", "primary", "secondary", "tertiary",
                "residential", "service"),
    surface = NA, maxspeed = NA, bicycle = NA, cycleway = NA,
    geometry = st_sfc(lapply(1:7, function(i)
      st_linestring(rbind(c(i, 0), c(i, 1)))), crs = 3310)
  )
  out <- process_osm_tags(osm)
  expect_equal(
    out$functional,
    c("Major Road", "Major Road", "Major Road",   # motorway/trunk/primary
      "Minor Road", "Minor Road",                  # secondary/tertiary
      "Local Road", "Local Road")                  # residential/service
  )
})

# --- calculate_model_features: functional survives the tibble rebuild ------
# calculate_model_features rebuilds a fresh tibble, so any column not explicitly
# named there is silently dropped. Confirm functional comes through as a factor.

test_that("calculate_model_features carries functional through as a factor", {
  links <- fixture_links() %>%
    mutate(
      infra_type = c("separated_path", "bike_lane", "shared_arterial"),
      functional = c("Local Road", "Minor Road", "Major Road"),
      is_paved = c(1, 1, 0),
      speed_limit = c(15, 25, 45)
    )
  out <- calculate_model_features(links)
  expect_true("functional" %in% names(out))
  expect_s3_class(out$functional, "factor")
  expect_equal(as.character(out$functional), c("Local Road", "Minor Road", "Major Road"))
})

test_that("calculate_model_features defaults missing functional to Local Road", {
  links <- fixture_links() %>%
    mutate(
      infra_type = c("separated_path", "bike_lane", "other"),
      functional = c("Major Road", NA, NA),   # two missing
      is_paved = 1, speed_limit = 25
    )
  out <- calculate_model_features(links)
  expect_equal(as.character(out$functional), c("Major Road", "Local Road", "Local Road"))
})

# --- modeling: functional is a predictor and one-hot encodes ---------------

test_that("functional is in the predictor sets and one-hot encodes", {
  expect_true("functional" %in% PREDICTORS_BASE)
  expect_true("functional" %in% PREDICTORS_A)
  expect_true("functional" %in% PREDICTORS_B)

  df <- data.frame(
    infra_type  = factor(c("separated_path", "shared_arterial")),
    functional  = factor(c("Local Road", "Major Road")),
    is_paved    = factor(c(1, 0)),
    speed_limit = c(15, 45),
    aadb        = c(10, 200)
  )
  m <- lgb_matrix(df, c("infra_type", "functional", "is_paved", "speed_limit"), "aadb")
  expect_true("functionalLocal Road" %in% m$cols)
  expect_true("functionalMajor Road" %in% m$cols)
})

# --- prep_network_topology: node aggregation -------------------------------
# Build a small network where two links meet at a shared node, with known
# attributes, so every aggregation rule can be checked by hand.
#
# Geometry: l1 = A--B, l2 = B--C. They share endpoint B (degree 2 at B).
#   l1: functional=Local, infra=quiet_street,   strava=10, speed=25, paved=1, crash=2
#   l2: functional=Major, infra=separated_path, strava=30, speed=45, paved=0, crash=5
# At shared node B both links meet (degree 2):
#   functional       -> highest         = Major Road
#   infra_type       -> most-protective = separated_path
#   strava_vol_total -> 2*sum/degree    = 2*(10+30)/2 = 40
#   speed_limit      -> max             = 45
#   is_paved         -> max             = 1
#   crash_count_30m  -> sum             = 7
#   area/context (e.g. emp_density)     -> mean

test_that("prep_network_topology aggregates link attributes to nodes correctly", {
  l1 <- st_linestring(rbind(c(0, 0), c(100, 0)))     # A--B
  l2 <- st_linestring(rbind(c(100, 0), c(200, 0)))   # B--C (shares B)
  links <- st_sf(
    edge_uid         = c("e1", "e2"),
    functional       = c("Local Road", "Major Road"),
    infra_type       = c("quiet_street", "separated_path"),
    strava_vol_total = c(10, 30),
    speed_limit      = c(25, 45),
    is_paved         = c(1, 0),
    crash_count_30m  = c(2, 5),
    emp_density      = c(100, 200),     # area/context -> mean
    geometry = st_sfc(l1, l2, crs = 3310)
  )

  topo <- prep_network_topology(links)
  nodes <- topo$nodes

  # The shared node B is at coordinate (100, 0). Find it by node degree: it is the
  # only node touched by both links. Its from/to id is the coord hash of (100,0).
  node_B_id <- sprintf("%s_%s", as.character(round(100, 6)), as.character(round(0, 6)))
  b <- nodes[nodes$node_id == node_B_id, ]
  expect_equal(nrow(b), 1)

  expect_equal(b$functional, "Major Road")            # highest class
  expect_equal(b$infra_type, "separated_path")        # most-protective facility
  expect_equal(b$strava_vol_total, 40)                # 2*sum/degree = 2*40/2
  expect_equal(b$speed_limit, 45)                     # max
  expect_equal(b$is_paved, 1)                         # max
  expect_equal(b$crash_count_30m, 7)                  # sum
  expect_equal(b$emp_density, 150)                    # mean(100,200)
})

test_that("node functional aggregation respects the Major>Minor>Local order", {
  # Three links meeting at a shared node, classes Local/Minor/Major in any order;
  # highest must win.
  l1 <- st_linestring(rbind(c(0, 0),   c(50, 0)))
  l2 <- st_linestring(rbind(c(50, 0),  c(100, 0)))
  l3 <- st_linestring(rbind(c(50, 0),  c(50, 50)))   # all share (50,0)
  links <- st_sf(
    edge_uid         = c("e1", "e2", "e3"),
    functional       = c("Minor Road", "Local Road", "Major Road"),
    infra_type       = c("bike_lane", "other", "shared_arterial"),
    strava_vol_total = c(1, 1, 1),
    speed_limit      = c(25, 25, 45),
    is_paved         = c(1, 1, 1),
    crash_count_30m  = c(0, 0, 0),
    emp_density      = c(0, 0, 0),
    geometry = st_sfc(l1, l2, l3, crs = 3310)
  )
  topo <- prep_network_topology(links)
  node_id <- sprintf("%s_%s", as.character(round(50, 6)), as.character(round(0, 6)))
  shared <- topo$nodes[topo$nodes$node_id == node_id, ]
  expect_equal(shared$functional, "Major Road")
  expect_equal(shared$infra_type, "bike_lane")   # most-protective of the three
})
