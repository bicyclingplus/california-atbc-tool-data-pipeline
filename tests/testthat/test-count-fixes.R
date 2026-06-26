# Tests for the count-data correctness fixes:
#  - axis-based bike snapping (direction_to_axis, axis_separation,
#    snap_counts_to_network)
#  - edge_uid dedup in match_strava_to_osm
#  - CAT/Caltrans dedup in load_catportal_counts
# Functions are sourced globally by helper-fixtures.R.

# --- direction_to_axis ------------------------------------------------------
test_that("direction_to_axis folds direction labels to road axes (mod 180)", {
  # N and S both -> N-S axis (0); E and W -> E-W axis (90): axis is invariant to
  # which way along the road, so it is also invariant to the CAT/Caltrans
  # travel-vs-approach label inversion.
  expect_equal(direction_to_axis(c("Northbound", "Southbound", "N", "S")), c(0, 0, 0, 0))
  expect_equal(direction_to_axis(c("Eastbound", "Westbound", "E", "W")), c(90, 90, 90, 90))
  expect_equal(direction_to_axis(c("Northeast", "Southwest")), c(45, 45))
  expect_equal(direction_to_axis(c("Northwest", "Southeast")), c(135, 135))
  expect_true(is.na(direction_to_axis("Up")))
  expect_true(is.na(direction_to_axis(NA)))
})

# --- axis_separation --------------------------------------------------------
test_that("axis_separation is the smallest angle between two axes", {
  expect_equal(axis_separation(0, 90), 90)
  expect_equal(axis_separation(45, 45), 0)
  expect_equal(axis_separation(10, 170), 20)   # wraps around 0/180
})

# --- snap_counts_to_network: axis assignment --------------------------------
test_that("snap_counts_to_network sums bike legs per axis and assigns to the matching-axis link", {
  ctr <- st_transform(st_sfc(st_point(c(-121.5, 38.5)), crs = 4326), 3310)
  cxy <- st_coordinates(ctr)[1, ]
  ns <- st_linestring(rbind(c(cxy[1], cxy[2] - 100), c(cxy[1], cxy[2] + 100)))  # N-S, axis 0
  ew <- st_linestring(rbind(c(cxy[1] - 100, cxy[2]), c(cxy[1] + 100, cxy[2])))  # E-W, axis 90
  links <- st_sf(edge_uid = c("ns", "ew"), from = c("a", "c"), to = c("b", "d"),
                 geometry = st_sfc(ns, ew, crs = 3310))
  nodes <- st_sf(node_id = "n1", geometry = st_sfc(st_point(cxy), crs = 3310))
  counts <- data.frame(
    mode = "bike", aadt = c(10, 20, 5, 15), lat = 38.5, lon = -121.5,
    direction = c("Northbound", "Southbound", "Eastbound", "Westbound"),
    source = "test", spatial_id = "loc1", year = 2023, stringsAsFactors = FALSE)

  res <- snap_counts_to_network(counts, list(links = links, nodes = nodes))
  lt <- res$links_train
  expect_equal(nrow(lt), 2)
  expect_equal(lt$aadb[lt$edge_uid == "ns"], 30)   # N + S onto the N-S link
  expect_equal(lt$aadb[lt$edge_uid == "ew"], 20)   # E + W onto the E-W link
})

test_that("snap_counts_to_network snaps directionless (UCB) bike counts to the nearest link", {
  ctr <- st_transform(st_sfc(st_point(c(-121.5, 38.5)), crs = 4326), 3310)
  cxy <- st_coordinates(ctr)[1, ]
  near <- st_linestring(rbind(c(cxy[1], cxy[2] - 5),   c(cxy[1], cxy[2] + 5)))    # ~at point
  far  <- st_linestring(rbind(c(cxy[1] + 500, cxy[2]), c(cxy[1] + 600, cxy[2])))  # 500 m away
  links <- st_sf(edge_uid = c("near", "far"), from = c("a", "c"), to = c("b", "d"),
                 geometry = st_sfc(near, far, crs = 3310))
  nodes <- st_sf(node_id = "n1", geometry = st_sfc(st_point(cxy), crs = 3310))
  counts <- data.frame(mode = "bike", aadt = 42, lat = 38.5, lon = -121.5,
                       direction = NA_character_, source = "ucb", spatial_id = "u1",
                       year = 2018, stringsAsFactors = FALSE)

  res <- snap_counts_to_network(counts, list(links = links, nodes = nodes))
  lt <- res$links_train
  expect_equal(nrow(lt), 1)
  expect_equal(lt$edge_uid, "near")
  expect_equal(lt$aadb, 42)
})

# --- match_strava_to_osm: edge_uid dedup ------------------------------------
test_that("match_strava_to_osm drops duplicate edge_uid rows (boundary overlaps)", {
  strava <- data.frame(edge_uid = c("e1", "e1", "e2"), osm_ref = c("1", "1", "2"),
                       strava_vol_total = c(10, 10, 20))
  osm <- data.frame(osm_id = c("1", "2"), infra_type = c("bike_lane", "quiet_street"),
                    is_paved = c(1, 1), speed_limit = c(25, 25))
  out <- match_strava_to_osm(strava, osm)
  expect_equal(nrow(out), 2)                    # duplicate e1 collapsed to one
  expect_setequal(out$edge_uid, c("e1", "e2"))
})

# --- load_catportal_counts: CAT/Caltrans dedup ------------------------------
test_that("load_catportal_counts drops CAT copies of Caltrans counters (agency_name)", {
  dir <- tempfile(); dir.create(dir)
  meta <- data.frame(
    Filename = c("cal.csv.gz", "other.csv.gz"), Year = 2023,
    `Agency ID` = c(1, 2), Agency = c("Caltrans", "Other"),
    Mode = 2, `Mode Label` = "Bicycle", Method = 3,
    `Method Label` = "Permanent automated counter", check.names = FALSE)
  readr::write_csv(meta, file.path(dir, "counts_zip_metadata.csv"))
  mk <- function(lat, lon, agency) do.call(rbind, lapply(1:20, function(d) data.frame(
    interval_start = sprintf("1/%d/2023, %d:00:00 %s", d, ((0:23) %% 12) + 1,
                             ifelse(0:23 < 12, "AM", "PM")),
    interval_length = 60, volume = 10, latitude = lat, longitude = lon,
    bearing_dir = "North", location_type = "Trail", agency_name = agency)))
  readr::write_csv(mk(38.0, -121.0, "Caltrans"), gzfile(file.path(dir, "cal.csv.gz")))
  readr::write_csv(mk(39.0, -122.0, "Other"),    gzfile(file.path(dir, "other.csv.gz")))

  out <- load_catportal_counts(dir, ucb_sites = NULL)
  expect_false(any(round(out$lat, 3) == 38.0))  # Caltrans copy dropped
  expect_true(any(round(out$lat, 3) == 39.0))   # non-Caltrans kept
})
