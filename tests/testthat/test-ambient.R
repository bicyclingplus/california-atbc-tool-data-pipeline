# Tests for src/functions/ambient.R -- raster ambient Strava.
# NOTE: focal windows must be small relative to the grid extent, so the fixtures
# here use a wide synthetic network and small test rings (the real pipeline grid
# is statewide). We use rings c(50, 100) on a ~600m network.

fixture_wide_links <- function() {
  # A small grid of segments spanning both x and y so the raster extent is 2-D.
  segs <- list()
  for (x in seq(0, 400, by = 100)) for (y in seq(0, 400, by = 100)) {
    segs[[length(segs) + 1]] <- st_linestring(rbind(c(x, y), c(x + 50, y + 50)))
  }
  st_sf(strava_vol_total = seq_along(segs) * 10,
        geometry = st_sfc(segs, crs = 3310))
}

test_that("build_strava_grid writes a multi-layer .tif", {
  out <- tempfile(fileext = ".tif")
  p <- build_strava_grid(fixture_wide_links(), out, res = 20, rings = c(50, 100))
  expect_true(file.exists(p))
  expect_equal(terra::nlyr(terra::rast(p)), 2)   # one layer per ring
})

test_that("extract_ambient returns one log1p annulus column per ring", {
  grid <- tempfile(fileext = ".tif")
  build_strava_grid(fixture_wide_links(), grid, res = 20, rings = c(50, 100))
  pts <- st_sf(geometry = st_sfc(st_point(c(200, 200)), crs = 3310))
  amb <- extract_ambient(grid, pts, radii = c(50, 100))
  expect_equal(names(amb), c("amb_strava_50m", "amb_strava_100m"))
  expect_equal(nrow(amb), 1)
  expect_false(any(is.na(amb)))
  expect_true(all(unlist(amb) >= 0))             # log1p of non-negative sums
})

test_that("inner ring captures less or equal total than the cumulative outer disk", {
  # The 100m disk contains the 50m disk, so before differencing disk_100 >= disk_50;
  # after differencing both annuli must be non-negative.
  grid <- tempfile(fileext = ".tif")
  build_strava_grid(fixture_wide_links(), grid, res = 20, rings = c(50, 100))
  pts <- st_sf(geometry = st_sfc(st_point(c(200, 200)), crs = 3310))
  amb <- extract_ambient(grid, pts, radii = c(50, 100))
  expect_true(all(unlist(amb) >= 0))
})
