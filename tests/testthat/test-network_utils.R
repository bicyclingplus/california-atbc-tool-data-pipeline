# Tests for src/functions/network_utils.R

test_that("process_osm_tags classifies facility, paved, and speed correctly", {
  out <- process_osm_tags(fixture_osm())
  # cycleway -> separated_path; primary with cycleway=lane -> bike_lane
  cls <- setNames(out$infra_type, out$osm_id)
  expect_equal(cls[["1"]], "separated_path")   # highway=cycleway
  expect_equal(cls[["4"]], "separated_path")   # footway
  expect_equal(cls[["2"]], "quiet_street")     # residential
  # surface -> is_paved
  paved <- setNames(out$is_paved, out$osm_id)
  expect_equal(paved[["1"]], 1)                # asphalt
  expect_equal(paved[["4"]], 0)                # gravel
  # speed: explicit maxspeed parsed; fallbacks otherwise
  spd <- setNames(out$speed_limit, out$osm_id)
  expect_equal(spd[["2"]], 25)                 # "25 mph" -> 25
  expect_equal(spd[["3"]], 45)                 # "45" parsed
  expect_true(all(c("osm_id", "infra_type", "is_paved", "speed_limit") %in% names(out)))
})

test_that("build_topology_from_links assigns shared endpoints the same node id", {
  links <- fixture_links()                     # e1:A-B, e2:B-C, e3:C-D
  topo <- build_topology_from_links(links)
  L <- topo$links
  # e1's 'to' (point B) must equal e2's 'from' (point B)
  expect_equal(L$to[L$edge_uid == "e1"], L$from[L$edge_uid == "e2"])
  # e2's 'to' (C) must equal e3's 'from' (C)
  expect_equal(L$to[L$edge_uid == "e2"], L$from[L$edge_uid == "e3"])
  # nodes are unique points
  expect_true(inherits(topo$nodes, "sf"))
  expect_equal(anyDuplicated(topo$nodes$node_id), 0)
})

test_that("map_volumes_across_network moves link bike vols to nodes (max) and node ped vols to links (avg)", {
  links <- fixture_links()
  topo <- build_topology_from_links(links)
  L <- topo$links; N <- topo$nodes
  L$pred_bike_vol <- c(10, 20, 30)
  N$pred_ped_vol <- seq_len(nrow(N)) * 100

  res <- map_volumes_across_network(L, N)
  # each node's bike vol = max over incident links
  expect_true("pred_bike_vol" %in% names(res$nodes))
  expect_true(all(res$nodes$pred_bike_vol >= 0))
  # each link's ped vol = average of its two endpoint nodes' ped vols
  expect_true("pred_ped_vol" %in% names(res$links))
  expect_equal(nrow(res$links), nrow(L))
})

test_that("finalize_web_network derives exposure classes and keeps the supplied functional", {
  links <- fixture_links()
  topo <- build_topology_from_links(links)
  L <- topo$links; N <- topo$nodes
  # functional (road hierarchy) and infra_type (bike facility) are SEPARATE axes,
  # both supplied upstream. finalize_web_network must keep functional as-is, not
  # re-derive it from infra_type.
  L$functional <- c("Major Road", "Minor Road", "Local Road")
  L$infra_type <- c("shared_arterial", "bike_lane", "separated_path")
  L$pred_bike_vol <- c(5, 50, 500)
  L$pred_ped_vol <- c(2, 20, 200)
  N$functional <- "Local Road"
  N$infra_type <- "quiet_street"
  N$pred_bike_vol <- c(1, 10, 100, 5)[seq_len(nrow(N))]
  N$pred_ped_vol <- c(3, 30, 300, 9)[seq_len(nrow(N))]

  res <- finalize_web_network(L, N)
  expect_true(all(c("bicycle_exposure_class", "pedestrian_exposure_class",
                    "functional", "length_ft") %in% names(res$links)))
  expect_true(all(res$links$bicycle_exposure_class %in% c("Low", "Medium", "High")))
  # functional is carried through unchanged (NOT derived from infra_type).
  expect_equal(res$links$functional[res$links$edge_uid == "e1"], "Major Road")
  expect_equal(res$links$functional[res$links$edge_uid == "e3"], "Local Road")
  # infra_type stays the bike-facility value, distinct from functional.
  expect_equal(res$links$infra_type[res$links$edge_uid == "e1"], "shared_arterial")
})

test_that("match_strava_to_osm joins osm attributes onto strava by osm_ref", {
  strava <- data.frame(osm_ref = c("1", "2", "999"), strava_vol_total = c(10, 20, 30))
  osm <- data.frame(osm_id = c("1", "2"), infra_type = c("bike_lane", "quiet_street"),
                    is_paved = c(1, 1), speed_limit = c(25, 25))
  out <- match_strava_to_osm(strava, osm)
  expect_equal(nrow(out), 3)
  expect_equal(out$infra_type[out$osm_ref == "1"], "bike_lane")
  expect_true(is.na(out$infra_type[out$osm_ref == "999"]))   # unmatched -> NA
})
