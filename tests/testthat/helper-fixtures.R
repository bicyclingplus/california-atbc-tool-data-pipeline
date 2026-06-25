# Shared setup for all tests: source the pipeline function files and provide tiny
# synthetic fixtures. No Box data, no network, no pipeline.

suppressMessages({
  library(sf); library(dplyr); library(terra); library(lightgbm)
})

# Locate src/functions relative to the repo root (tests run from repo root).
.fn_dir <- "src/functions"
if (!dir.exists(.fn_dir)) {
  # fall back to absolute repo path
  .fn_dir <- "C:/Users/Dillon/projects/california-atbc-tool-data-pipeline/src/functions"
}
for (f in list.files(.fn_dir, pattern = "\\.R$", full.names = TRUE)) {
  suppressWarnings(suppressMessages(sys.source(f, envir = globalenv())))
}

# --- Fixtures --------------------------------------------------------------

# A tiny set of LINESTRING links in CRS 3310 (projected metres) with strava +
# from/to node geometry already shared between adjacent links.
fixture_links <- function() {
  l1 <- st_linestring(rbind(c(0, 0), c(100, 0)))      # A--B
  l2 <- st_linestring(rbind(c(100, 0), c(200, 0)))    # B--C (shares B)
  l3 <- st_linestring(rbind(c(200, 0), c(200, 100)))  # C--D (shares C)
  st_sf(
    edge_uid = c("e1", "e2", "e3"),
    strava_vol_total = c(10, 20, 30),
    strava_leisure   = c(5, 10, 15),
    geometry = st_sfc(l1, l2, l3, crs = 3310)
  )
}

# Tiny synthetic OSM-tag table for process_osm_tags().
fixture_osm <- function() {
  st_sf(
    osm_id   = as.character(1:5),
    highway  = c("cycleway", "residential", "primary", "footway", "secondary"),
    surface  = c("asphalt", NA, "concrete", "gravel", NA),
    maxspeed = c(NA, "25 mph", "45", NA, NA),
    bicycle  = c(NA, NA, NA, NA, NA),
    cycleway = c(NA, NA, "lane", NA, NA),
    geometry = st_sfc(lapply(1:5, function(i)
      st_linestring(rbind(c(i, 0), c(i, 1)))), crs = 3310)
  )
}
