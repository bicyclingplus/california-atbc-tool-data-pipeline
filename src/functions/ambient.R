library(terra)
library(sf)
library(dplyr)

# ============================================================================
# Ambient Strava demand field (raster focal-sum)
# ----------------------------------------------------------------------------
# A count site's (or a new path's) bicycling demand is predicted by the
# Strava activity of the surrounding network. Since there is no strava volume
# on entirely new facilities, this surrounding volumes is a key predictor
# for estimating path volume. The "ambient Strava" volume is a series of 
# summed volume over rings around each location.
#
# Computing ring sums point-by-point against the 8M-link network was infeasible
# (estimated 1,250 hours for the full prediction network). So this raterizes 
# Strava volume to a 100 m grid and use focal (moving-window) sums. The estimate
# was validated vs exact point-buffers for a sample:
# 250 m cor=0.99, 500 m=1.00, 1 km=1.00, 2 km=1.00
#
# RINGS are non-overlapping to reduce collinearity: 
# 0-250, 250-500, 500-1000, 1000-2000 m.
# ============================================================================

AMBIENT_RINGS <- c(250, 500, 1000, 2000)   # outer radii of the cumulative disks

#' Build the gridded Strava demand raster and write it to a multi-layer GeoTIFF.
#'
#' Returns the .tif path (for use as a targets `format = "file"` target). Each
#' layer is the cumulative-disk focal sum of `strava_vol_total` at one ring
#' radius; the rings are differenced at extraction time.
#'
#' @param master_network sf LINESTRING with `strava_vol_total` (any CRS; reprojected to 3310)
#' @param out_path       destination .tif
#' @param res            grid resolution in metres (default 100)
build_strava_grid <- function(master_network, out_path, res = 100,
                              rings = AMBIENT_RINGS) {
  message("...Building ambient Strava grid (rasterize + focal sums)")
  mn <- st_transform(master_network, 3310)

  xy <- st_coordinates(st_centroid(st_geometry(mn)))
  # extract point vector for network and store in terra spatial
  pv <- terra::vect(data.frame(x = xy[, 1], y = xy[, 2],
                               strava = mn$strava_vol_total),
                    geom = c("x", "y"), crs = "EPSG:3310")

  # create raster of 0s for extent of point vector
  r0 <- terra::rast(terra::ext(pv), resolution = res, crs = "EPSG:3310")
  # map point vector to the raster and fill with strava volume, leave cells without
  # points at 0.
  rstrava <- terra::rasterize(pv, r0, field = "strava", fun = "sum", background = 0)

  # Cumulative-disk focal sums (one layer per outer radius). Binary circular
  # window of each radius; focal sum = total Strava within that disk per cell.
  disks <- lapply(rings, function(rad) {
    w <- terra::focalMat(rstrava, rad, type = "circle") # ring weight matrix
    w[w > 0] <- 1                                       # convert weights to binary
    terra::focal(rstrava, w, fun = "sum", na.rm = TRUE) # focal sum
  })
  out <- terra::rast(disks)
  names(out) <- paste0("disk_", rings, "m")

  terra::writeRaster(out, out_path, overwrite = TRUE)
  out_path
}

#' Extract ambient Strava rings at points from the gridded .tif.
#'
#' @param grid_path path to the .tif written by build_strava_grid()
#' @param points    sf object (any geometry; centroids used) or sf POINTs
#' @param radii     outer radii (must match the grid layers)
#' @return tibble with columns amb_strava_<ring>m (one per ring), 
#' log1p-scaled for modeling
extract_ambient <- function(grid_path, points, radii = AMBIENT_RINGS) {
  grid <- terra::rast(grid_path)
  pts <- st_centroid(st_transform(st_geometry(points), 3310))
  v <- terra::vect(st_coordinates(pts), type = "points", crs = "EPSG:3310")

  disks <- terra::extract(grid, v)[, -1, drop = FALSE]   # drop ID col (-1), returns df
  disks[is.na(disks)] <- 0                               # make sure all else is 0

  # Cumulative disks -> non-overlapping rings. 
  # Each ring = disk at this radius minus disk at previous radius.
  rings <- disks
  for (k in length(radii):2){   # iterate from outer disk to 2 (disk 1 is already a ring)
    rings[[k]] <- disks[[k]] - disks[[k - 1]]
  } 
  rings <- as.data.frame(lapply(rings, function(x) log1p(x))) # log1 transform
  names(rings) <- paste0("amb_strava_", radii, "m")
  tibble::as_tibble(rings)
}
