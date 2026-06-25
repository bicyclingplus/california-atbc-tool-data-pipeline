# Tests for src/functions/enrichment.R (logic-only parts).

test_that("process_switrs_data drops missing coords and projects to 3310", {
  csv <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(
    POINT_X = c(-121.7, NA, -122.3),
    POINT_Y = c(38.5, 38.6, 37.8),
    BICYCLE_ACCIDENT = "Y"
  ), csv)
  out <- process_switrs_data(csv)
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2)                     # NA-coord row dropped
  expect_equal(sf::st_crs(out)$epsg, 3310L)      # projected
})

# --- PRISM climate extraction --------------------------------------------
# extract_prism() samples ppt/tmin/tmax from the PRISM 4km grid at point
# geometries. We avoid the network download (get_prism_climate) by writing tiny
# synthetic single-cell-resolution rasters in PRISM's native CRS (EPSG:4269) and
# checking the values land on the right cells.

# Build three synthetic PRISM-like rasters over a small CA-ish lon/lat window and
# return their .bil paths, mimicking get_prism_climate()'s named output.
fixture_prism <- function(dir = tempfile("prism")) {
  dir.create(dir)
  ext <- terra::ext(-122, -120, 37, 39)        # 2x2 degree window
  mk <- function(vals) {
    r <- terra::rast(ext, nrow = 2, ncol = 2, crs = "EPSG:4269")
    terra::values(r) <- vals                    # row-major: NW, NE, SW, SE
    r
  }
  paths <- c(
    ppt  = file.path(dir, "ppt.tif"),
    tmin = file.path(dir, "tmin.tif"),
    tmax = file.path(dir, "tmax.tif")
  )
  terra::writeRaster(mk(c(100, 200, 300, 400)), paths["ppt"])   # mm
  terra::writeRaster(mk(c(2,   4,   6,   8)),   paths["tmin"])  # degC
  terra::writeRaster(mk(c(12,  14,  16,  18)),  paths["tmax"])  # degC
  paths
}

test_that("extract_prism samples ppt/tmin/tmax at points in the correct cells", {
  paths <- fixture_prism()

  # One point in the NW cell, one in the SE cell (lon/lat, EPSG:4269).
  pts <- st_sf(
    id = c("nw", "se"),
    geometry = st_sfc(st_point(c(-121.5, 38.5)),   # NW cell
                      st_point(c(-120.5, 37.5)),   # SE cell
                      crs = 4269)
  )
  out <- extract_prism(pts, paths)

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("prcp_annua", "temp_min", "temp_max"))
  expect_equal(out$prcp_annua, c(100, 400))   # NW=100, SE=400
  expect_equal(out$temp_min,   c(2, 8))
  expect_equal(out$temp_max,   c(12, 18))
})

test_that("extract_prism reprojects non-4269 points before sampling", {
  paths <- fixture_prism()

  # Same NW point, but provided in CRS 3310 (projected metres). extract_prism
  # must reproject to 4269 internally and still hit the NW cell.
  pt_ll <- st_sfc(st_point(c(-121.5, 38.5)), crs = 4269)
  pt_3310 <- st_transform(pt_ll, 3310)
  pts <- st_sf(id = "nw", geometry = pt_3310)

  out <- extract_prism(pts, paths)
  expect_equal(out$prcp_annua, 100)
  expect_equal(out$temp_min, 2)
  expect_equal(out$temp_max, 12)
})

test_that("enrich_crashes counts crashes within the 30m buffer of each link", {
  # Two links; place a crash point ~5m from link 1 and none near link 2.
  l1 <- st_linestring(rbind(c(0, 0), c(100, 0)))
  l2 <- st_linestring(rbind(c(0, 500), c(100, 500)))
  links <- st_sf(edge_uid = c("e1", "e2"),
                 geometry = st_sfc(l1, l2, crs = 3310))
  crashes <- st_sf(
    BICYCLE_ACCIDENT = "Y",
    geometry = st_sfc(st_point(c(50, 5)), st_point(c(50, 6)), crs = 3310)
  )
  out <- enrich_crashes(links, crashes)
  expect_true("crash_count_30m" %in% names(out))
  expect_equal(out$crash_count_30m[out$edge_uid == "e1"], 2)   # both within 30m
  expect_equal(out$crash_count_30m[out$edge_uid == "e2"], 0)   # none near
})
