library(sf)
library(tidyverse)
library(httr)
library(jsonlite)

#' Process SWITRS Crash Data
process_switrs_data <- function(file_path) {
  read_csv(file_path, show_col_types = FALSE) %>%
    filter(!is.na(POINT_X), !is.na(POINT_Y)) %>%
    st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 4326) %>%
    st_transform(3310) 
}

#' Download (cached) PRISM 4km annual climate normals for `year`.
#'
#' Sample every segment/block from the PRISM 4km gridded surface. 
#' Pulls three annual layers and returns their .bil paths:
#'   ppt  -> annual precipitation (mm)
#'   tmin -> annual mean of daily minimum temperature (deg C)
#'   tmax -> annual mean of daily maximum temperature (deg C)
#'
#' Returned as a named character vector to call by variable.
#' @param year       PRISM annual estimate
#' @param cache_dir  Folder for the PRISM download cache (created if needed).
get_prism_climate <- function(year = 2023, cache_dir = "data_raw/prism",
                              resolution = "4km") {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  prism::prism_set_dl_dir(cache_dir)

  # prism >= 0.3.0 requires `resolution` ("4km"/"800m") to disambiguate the
  # archive -- prism_archive_subset() defaults it to NULL and errors otherwise.
  vars <- c("ppt", "tmin", "tmax")
  paths <- vapply(vars, function(v) {
    prism::get_prism_annual(v, years = year, keepZip = FALSE,
                            resolution = resolution)
    prism::pd_to_file(prism::prism_archive_subset(
      v, "annual", years = year, resolution = resolution))
  }, character(1))

  names(paths) <- vars     # c(ppt = ..., tmin = ..., tmax = ...)
  paths
}

#' Sample the PRISM ppt/tmin/tmax rasters at point geometries.
#'
#' Leaves PRISM in its native CRS (NAD83 geographic, EPSG:4269) and reprojects
#' the points to match.
#' Returns a tibble (one row per input feature) with the three model columns.
#' @param points_sf   sf POINT geometries (e.g. segment centroids).
#' @param prism_paths Char vector from get_prism_climate().
extract_prism <- function(points_sf, prism_paths) {
  if (is.null(names(prism_paths)) ||
      !all(c("ppt", "tmin", "tmax") %in% names(prism_paths))) {
    stopifnot(length(prism_paths) == 3L)
    names(prism_paths) <- c("ppt", "tmin", "tmax")
  }
  r   <- terra::rast(unname(prism_paths[c("ppt", "tmin", "tmax")]))
  pts <- terra::vect(sf::st_transform(points_sf, 4269))   # PRISM CRS
  v   <- terra::extract(r, pts, ID = FALSE)
  tibble::tibble(
    prcp_annua = as.numeric(v[[1]]),
    temp_min   = as.numeric(v[[2]]),
    temp_max   = as.numeric(v[[3]])
  )
}

#' Enrich Bike Network Link
#' This updates EVERY link in the master network with covariates
#' Consolidates Strava, SLD, Walk Index, Crashes, and Weather into one SF object
#' Enrich Strava with OSM by table join (assuming osm_id is correct)
#' 
# --- BASE DATA ENRICHMENT ---
# Combines OSM, Weather, SLD, and Walk Index
enrich_base_network <- function(strava_sf, osm_sf, sld_sf, wi_sf, prism_paths) {
  message("...Joining OSM, Weather, SLD, and Walk Index")

  # Turn off S2 to avoid the "Loop 0" vertex errors during st_centroid
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(TRUE))

  # OSM Table Join
  osm_data <- osm_sf %>%
    st_drop_geometry() %>%
    mutate(osm_id = as.character(osm_id)) %>%
    select(osm_id, any_of(c("infra_type", "is_paved", "speed_limit", "functional")))

  strava_enriched <- strava_sf %>%
    mutate(osm_ref = as.character(osm_ref)) %>%
    left_join(osm_data, by = c("osm_ref" = "osm_id")) %>%
    st_transform(3310)

  # Compute centroids ONCE: reused for both the PRISM raster sampling and the
  # SLD/WI polygon joins below.
  centroids <- st_centroid(strava_enriched)

  # Weather Join (PRISM 4km grid sampled at segment centroids)
  clim <- extract_prism(centroids, prism_paths)
  strava_enriched$prcp_annua <- clim$prcp_annua
  strava_enriched$temp_min   <- clim$temp_min
  strava_enriched$temp_max   <- clim$temp_max

  # Polygon Joins (SLD & WI via Centroid)
  centroids_data <- centroids %>%
    st_join(st_transform(sld_sf, 3310) %>% select(D1B, D3b), join = st_intersects) %>%
    st_join(st_transform(wi_sf, 3310) %>% select(NatWalkInd), join = st_intersects) %>%
    st_drop_geometry() %>%
    select(edge_uid, D1B, D3b, NatWalkInd)

  strava_enriched %>%
    left_join(centroids_data, by = "edge_uid")
}

# --- CRASH ENRICHMENT ---
enrich_crashes <- function(strava_sf, crash_sf) {
  # Identifying which chunk this is by its row count
  chunk_id <- paste0("Chunk (", nrow(strava_sf), " rows)")
  
  message(paste(chunk_id, "--- Starting Crash Join ---"))
  
  # Get Bounding Box
  message(paste(chunk_id, ": Step 1/3 - Filtering Crashes by BBox..."))
  chunk_bbox <- st_as_sfc(st_bbox(strava_sf)) %>% st_buffer(100)
  crash_subset <- crash_sf[chunk_bbox, ]
  message(paste(chunk_id, ": Found", nrow(crash_subset), "crashes in vicinity."))
  
  # Buffer
  message(paste(chunk_id, ": Step 2/3 - Creating 30m road buffers..."))
  strava_buffer <- st_buffer(strava_sf, 30, endCapStyle = "SQUARE", joinStyle = "MITRE")
  
  # Intersect
  message(paste(chunk_id, ": Step 3/3 - Performing Point-in-Polygon check..."))
  intersect_list <- st_intersects(strava_buffer, crash_subset)
  
  strava_sf$crash_count_30m <- lengths(intersect_list)
  
  message(paste(chunk_id, "--- FINISHED ---"))
  return(strava_sf)
}

# --- CENSUS ENRICHMENT ---
enrich_census <- function(strava_sf, census_sf) {
  message("...Joining Census Block Data")
  
  # Crucial: S2 off for speed and to avoid vertex errors
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(TRUE))
  
  # Transform census to match network projection (CA Albers)
  census_proj <- st_transform(census_sf, 3310)
  
  # Use Centroids to join attributes
  # This ensures we don't get double-counting from edges crossing boundaries
  census_join <- strava_sf %>%
    st_centroid() %>%
    st_join(census_proj, join = st_intersects) %>%
    st_drop_geometry() %>%
    # Select all census columns except those that might conflict (like geometry)
    select(edge_uid, everything()) %>%
    # One census block per link. A centroid landing on a block boundary can
    # intersect two blocks -> duplicate edge_uid rows -> the final left_join
    # below becomes one-to-many and duplicates the link. distinct() keeps the
    # first block (matches the "avoid double-counting" intent above).
    distinct(edge_uid, .keep_all = TRUE)

  # Handle NAs
  # If a segment didn't hit a US census block, fill numeric cols with 0
  census_join <- census_join %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  
  # Join back to the main network linework
  strava_sf %>%
    left_join(census_join, by = "edge_uid")
}

# --- FINAL MATH ---
calculate_model_features <- function(strava_sf) {
  
  # UNWRAP list
  if (is.list(strava_sf) && !inherits(strava_sf, "sf")) {
    strava_sf <- strava_sf[[1]] 
  }
  
  # SEPARATE Geometry for Speed
  geom_backup <- st_geometry(strava_sf)
  df_clean <- st_drop_geometry(strava_sf)
  
  # accessibility coloumns to fill with zeros
  repair_cols <- c(
    "POP_LOW_ST", "POP_HIGH_S", "HOUSING10", "EMP_LOW_ST", "EMP_HIGH_S",
    "SCHOOLS_LO", "SCHOOLS_HI", "COLLEGES_L", "COLLEGES_H", "DOCTORS_LO", 
    "DOCTORS_HI", "PHARMAC_01", "PHARMAC_02", "RETAIL_LOW", "RETAIL_HIG", 
    "SUPERMA_01", "SUPERMA_02", "PARKS_LOW_", "PARKS_HIGH", "TRAILS_LOW", 
    "TRAILS_HIG", "COMMUNI_01", "COMMUNI_02", "TRANSIT_LO", "TRANSIT_HI"
  )
  
  df_clean <- df_clean %>%
    mutate(across(
      any_of(repair_cols), 
      ~as.numeric(as.character(.)) %>% replace_na(0)
    ))
  
  # HELPER: Clean names from prior joins (Strava, OSM, SLD, WI, Crashes)
  fast_coalesce <- function(target_df, base_name) {
    matched_cols <- grep(paste0("^", base_name, "(\\.[xy])*$"), names(target_df), value = TRUE)
    if (length(matched_cols) == 0) return(rep(NA, nrow(target_df)))
    if (length(matched_cols) == 1) return(target_df[[matched_cols]])
    exec(coalesce, !!!target_df[matched_cols])
  }
  
  # VARIABLE MAPPING & RENAMING
  df_final <- tibble(
    edge_uid         = fast_coalesce(df_clean, "edge_uid"),
    from             = fast_coalesce(df_clean, "from"),
    to               = fast_coalesce(df_clean, "to"),
    strava_vol_total = fast_coalesce(df_clean, "strava_vol_total"),
    
    # --- BUILT ENVIRONMENT (From Prior SLD/WI Joins) ---
    # assume NAs are 0 for density variables
    emp_density      = replace_na(fast_coalesce(df_clean, "D1B"), 0),
    int_density      = replace_na(fast_coalesce(df_clean, "D3b"), 0),
    walk_index       = fast_coalesce(df_clean, "NatWalkInd"),
    
    # --- CENSUS / BNA ACCESSIBILITY (Direct selection + Renaming) ---
    # Population & Housing
    pop_low          = df_clean$POP_LOW_ST,
    pop_high         = df_clean$POP_HIGH_S,
    housing_total    = df_clean$HOUSING10,
    
    # Employment
    emp_low          = df_clean$EMP_LOW_ST,
    emp_high         = df_clean$EMP_HIGH_S,
    
    # Schools & Education
    schools_low      = df_clean$SCHOOLS_LO,
    schools_high     = df_clean$SCHOOLS_HI,
    colleges_low     = df_clean$COLLEGES_L,
    colleges_high    = df_clean$COLLEGES_H,
    
    # Healthcare
    doctors_low      = df_clean$DOCTORS_LO,
    doctors_high     = df_clean$DOCTORS_HI,
    pharmacies_low   = df_clean$PHARMAC_02, 
    pharmacies_high  = df_clean$PHARMAC_01, 
    
    # Retail & Services
    retail_low       = df_clean$RETAIL_LOW,
    retail_high      = df_clean$RETAIL_HIG,
    supermarket_low  = df_clean$SUPERMA_02,
    supermarket_high = df_clean$SUPERMA_01,
    
    # Recreation & Community
    parks_low        = df_clean$PARKS_LOW_,
    parks_high       = df_clean$PARKS_HIGH,
    trails_low       = df_clean$TRAILS_LOW,
    trails_high      = df_clean$TRAILS_HIG,
    community_low    = df_clean$COMMUNI_02,
    community_high   = df_clean$COMMUNI_01,
    
    # Transit
    transit_low      = df_clean$TRANSIT_LO,
    transit_high     = df_clean$TRANSIT_HI,
    
    # --- NETWORK & INFRASTRUCTURE (Prior OSM Joins) ---
    infra_type       = fast_coalesce(df_clean, "infra_type"),
    is_paved         = fast_coalesce(df_clean, "is_paved"),
    speed_limit      = fast_coalesce(df_clean, "speed_limit"),
    functional       = fast_coalesce(df_clean, "functional"),  # road hierarchy
    
    # --- ENVIRONMENTAL/SAFETY (Prior Weather/Crash Joins) ---
    precip_annual    = fast_coalesce(df_clean, "prcp_annua"),
    temp_min         = fast_coalesce(df_clean, "temp_min"),
    temp_max         = fast_coalesce(df_clean, "temp_max"),
    crash_count_30m  = replace_na(fast_coalesce(df_clean, "crash_count_30m"), 0)
  ) %>%
    mutate(
      # Bias adjustment calculation
      stra_leisure = fast_coalesce(df_clean, "strava_leisure"),
      recr_prop    = replace_na(stra_leisure / pmax(strava_vol_total, 1), 0),
      
      # Miah et al. (2025) Weekend-Weekday Index (WWI) Calculation
      WWI = 0.55134 - 0.04631 * log10(pmax(emp_density, 1)) + 0.61717 * (recr_prop^2),
      
      # Convert categorical predictors to factors (one-hot at model time).
      infra_type = as.factor(infra_type),
      is_paved   = as.factor(is_paved),
      functional = as.factor(replace_na(functional, "Local Road"))
    )
  
  # 5. REATTACH Geometry
  final_sf <- st_set_geometry(df_final, geom_backup)
  
  return(final_sf)
}

# --- Builds the network topology over the full network (deployment="main"). ---
# Generates stable from/to link ids + node geometry (coordinate-hashed, so ids
# are reproducible across rebuilds), then aggregates link attributes to nodes
# (mean of numeric, first of character) for the ped model.
prep_network_topology <- function(enriched_links) {
  message("...Building Network Topology (This is the slow part...)")

  # Build Topology
  topo <- build_topology_from_links(enriched_links)
  links_final <- topo$links
  nodes_geom  <- topo$nodes

  if(!"edge_uid" %in% names(links_final)) stop("Error: 'edge_uid' column missing.")

  # Clean Attributes
  links_final <- links_final %>%
    mutate(
      across(matches("pharmacies|supermarket|schools|retail|parks|transit"), ~replace_na(as.numeric(.), 0)),
      infra_type = replace_na(as.character(infra_type), "other"),
      is_paved = as.numeric(replace_na(is_paved, 1)),
      functional = replace_na(as.character(functional), "Local Road")
    )

  # Aggregate link attributes to nodes (for the ped model): pivot links to their
  # endpoint node_ids, then summarise per node. After pivot_longer each link
  # contributes 2 rows (its two endpoints), so within a node group n() == the
  # node degree (number of link-endpoints meeting there).
  #
  # Aggregation is per-attribute, not a blanket mean/first:
  #   strava_vol_total : crossing volume without double-counting. Each link's
  #                      volume is counted at BOTH its endpoints, so raw sum
  #                      over-counts; sum / (0.5 * degree) == 2*sum/degree gives
  #                      the through/crossing volume at the node.
  #   speed_limit      : max  (intersection takes its fastest approaching road)
  #   is_paved         : max  (paved if any leg is paved)
  #   crash_count_30m  : sum  (all crashes near the meeting links)
  #   functional       : highest road class touching the node (Major>Minor>Local)
  #   infra_type       : most-protective facility touching the node
  #   everything else (area/context: densities, BNA buffers, weather, ...) : mean
  functional_rank <- c("Local Road" = 1, "Minor Road" = 2, "Major Road" = 3)
  infra_rank <- c("other" = 1, "shared_arterial" = 2, "shared_lane_marked" = 3,
                  "quiet_street" = 4, "bike_lane" = 5, "buffered_lane" = 6,
                  "separated_path" = 7)
  pick_max_rank <- function(x, ranks) {
    x <- x[!is.na(x) & x %in% names(ranks)]
    if (length(x) == 0) return(NA_character_)
    names(which.max(ranks[x]))
  }

  node_data <- links_final %>%
    st_drop_geometry() %>%
    pivot_longer(cols = c(from, to), values_to = "node_id") %>%
    group_by(node_id) %>%
    summarise(
      strava_vol_total = sum(strava_vol_total, na.rm = TRUE) / (0.5 * n()),
      speed_limit      = max(speed_limit, na.rm = TRUE),
      is_paved         = max(is_paved, na.rm = TRUE),
      crash_count_30m  = sum(crash_count_30m, na.rm = TRUE),
      across(
        where(is.numeric) &
          !any_of(c("strava_vol_total", "speed_limit", "is_paved", "crash_count_30m")),
        ~mean(.x, na.rm = TRUE)
      ),
      functional = pick_max_rank(functional, functional_rank),
      infra_type = pick_max_rank(infra_type, infra_rank),
      across(
        where(is.character) & !any_of(c("functional", "infra_type")),
        ~first(.x)
      ),
      .groups = "drop"
    )
  nodes_final <- inner_join(nodes_geom, node_data, by = "node_id")

  return(list(links = links_final, nodes = nodes_final))
}

# Fold a direction label to its road AXIS in [0,180): N/S->0, E/W->90,
# NE/SW->45, NW/SE->135. Axis (not travel direction) is what we assign on, so
# E and W both fold to the E-W axis. Returns NA for unparseable / 
# missing direction (e.g. UCB bike -> undirected snap).
direction_to_axis <- function(d) {
  d <- toupper(trimws(as.character(d)))
  brg <- dplyr::case_when(
    d %in% c("N","NB","NORTH","NORTHBOUND") ~ 0,   d %in% c("S","SB","SOUTH","SOUTHBOUND") ~ 180,
    d %in% c("E","EB","EAST","EASTBOUND")   ~ 90,  d %in% c("W","WB","WEST","WESTBOUND")   ~ 270,
    d %in% c("NE","NEB","NORTHEAST") ~ 45, d %in% c("SW","SWB","SOUTHWEST") ~ 225,
    d %in% c("NW","NWB","NORTHWEST") ~ 315, d %in% c("SE","SEB","SOUTHEAST") ~ 135,
    TRUE ~ NA_real_)
  brg %% 180
}

# Smallest separation between two axes (each in [0,180)).
axis_separation <- function(a, b) { d <- abs(a - b) %% 180; pmin(d, 180 - d) }

# Assign each point to the nearest link whose AXIS aligns with the point's
# count-axis (within `tol` 45deg default, among links <= `dist` 50m default).
# If no aligned link within `dist`, take the nearest link within `dist`;
# if no link at all within `dist`, take the global nearest link.
# Returns a link row index per point.
#
# Fully VECTORIZED (no per-point loop). The earlier per-point version called
# st_nearest_feature() inside the loop for every no-candidate point -- each a
# full nearest search over the ~6M-link network -- and subset the 6M-row sf per
# iteration, which ran for hours and accumulated memory to OOM. This does a fixed
# handful of spatial ops over bare geometry instead:
#   1) st_is_within_distance: candidate links per point (one call)
#   2) st_nearest_feature:    global-nearest fallback for ALL points (one call)
#   3) st_distance:           every (point, candidate) pair at once (one call)
# then picks, per point, the nearest ALIGNED candidate (else nearest candidate)
# via a vectorized order()/!duplicated() -- identical result to the loop, with
# ties resolved to an equidistant link.
snap_to_axis_link <- function(pts_sf, links_final, link_axis, count_axis,
                              dist = 50, tol = 45) {
  lg   <- sf::st_geometry(links_final)                 # bare sfc: cheap to index
  cand <- sf::st_is_within_distance(pts_sf, lg, dist)  # candidate link idx per point
  chosen <- sf::st_nearest_feature(pts_sf, lg)         # default = global nearest (no-candidate pts)

  pii <- rep(seq_along(cand), lengths(cand))           # point index, one row per pair
  lii <- unlist(cand)                                  # candidate link index
  if (length(lii)) {
    dpair <- as.numeric(sf::st_distance(pts_sf[pii, ], lg[lii], by_element = TRUE))
    alg   <- axis_separation(link_axis[lii], count_axis[pii]) <= tol
    # within each point: aligned candidates first, then by distance -> first row
    # per point is the chosen link (nearest aligned, else nearest candidate).
    ord <- order(pii, !alg, dpair)
    sel <- ord[!duplicated(pii[ord])]
    chosen[pii[sel]] <- lii[sel]
  }
  chosen
}

# --- Snaps counts to the pre-built network ---
snap_counts_to_network <- function(counts_df, network_list) {
  message("...Snapping Counts to Pre-Built Network")

  links_final <- network_list$links
  nodes_final <- network_list$nodes

  # --- SNAP BIKE COUNTS (axis-based) ---
  # Bikes that carry a `direction` (Caltrans / CAT, possibly multiple legs per
  # site) are aggregated by AXIS: sum the legs on each axis (N+S, E+W, NE+SW,
  # NW+SE) and assign that both-directions sum to the matching-bearing link.
  # This matches the non-directional model (strava_vol_total = both directions on
  # a link) and, at true intersections, gives each road its own volume instead of
  # lumping the cross-street's volume onto one link. Bikes without a direction
  # (UCB) snap undirected to the nearest link.
  all_bike_matches_list <- list()

  bike_raw <- counts_df %>%
    filter(mode == "bike", !is.na(aadt)) %>%
    rename(raw_count = aadt)
  bike_raw$direction <- if ("direction" %in% names(bike_raw)) bike_raw$direction else NA_character_
  bike_raw$axis      <- direction_to_axis(bike_raw$direction)

  if(nrow(bike_raw) > 0) {
    # UNDIRECTED (no parseable direction, e.g. UCB) -> nearest link, as-is.
    undirected <- bike_raw %>% filter(is.na(axis))
    if(nrow(undirected) > 0) {
      u_sf <- undirected %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(st_crs(links_final))
      nearest_rows <- st_nearest_feature(u_sf, links_final)
      all_bike_matches_list[[length(all_bike_matches_list) + 1]] <-
        links_final[nearest_rows, ] %>%
        bind_cols(st_drop_geometry(u_sf) %>% select(aadb = raw_count, source, spatial_id, year))
    }

    # DIRECTIONAL -> sum legs per (site, year, axis), assign to nearest
    #    axis-aligned link. spatial_id = location (axis-independent) so both axes
    #    of one intersection stay in the same spatial-CV fold.
    directed <- bike_raw %>% filter(!is.na(axis))
    if(nrow(directed) > 0) {
      axis_sums <- directed %>%
        mutate(loc_id = paste0(round(lat, 6), "_", round(lon, 6))) %>%
        group_by(loc_id, year, axis) %>%
        summarize(aadb = sum(raw_count, na.rm = TRUE),
                  lat = first(lat), lon = first(lon),
                  source = first(source), .groups = "drop") %>%
        mutate(spatial_id = loc_id)

      d_sf <- axis_sums %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(st_crs(links_final))

      # Link axes (bearing mod 180), aligned to links_final row order.
      lco <- st_coordinates(links_final)
      lc  <- as.data.frame(lco) %>%
        group_by(L1) %>%
        summarize(sx = first(X), sy = first(Y), ex = last(X), ey = last(Y), .groups = "drop")
      link_axis <- ((atan2(lc$ex - lc$sx, lc$ey - lc$sy) * 180 / pi) %% 360) %% 180

      pick <- snap_to_axis_link(d_sf, links_final, link_axis, axis_sums$axis,
                                dist = 50, tol = 45)
      all_bike_matches_list[[length(all_bike_matches_list) + 1]] <-
        links_final[pick, ] %>%
        bind_cols(st_drop_geometry(d_sf) %>% select(aadb, source, spatial_id, year))
    }
  }

  # Final binding of matches
  links_train <- bind_rows(all_bike_matches_list)

  
  # --- SNAP PED COUNTS  ---
  # all to nodes, much simpler than bike counts.
  ped_counts <- counts_df %>% 
    filter(mode == "ped", !is.na(aadt)) %>%
    rename(raw_count = aadt)
  
  nodes_train <- tibble()
  
  if(nrow(ped_counts) > 0) {
    ped_sf <- ped_counts %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(st_crs(nodes_final))
    
    node_idx <- st_nearest_feature(ped_sf, nodes_final)
    
    nodes_train <- nodes_final[node_idx, ] %>%
      bind_cols(st_drop_geometry(ped_sf) %>% 
                  select(aadp=raw_count, source, spatial_id, year))
  }
  
  return(list(
    links_train   = links_train,
    links_predict = links_final,
    nodes_train   = nodes_train,
    nodes_predict = nodes_final
  ))
}

# --- Prepare Census Blocks for Web Tool in chunks---
enrich_census_chunk <- function(census_chunk, sld_sf, wi_sf, crash_sf, prism_paths) {

  # Setup & CRS Definition
  # Ensure the chunk is valid and ready
  blocks_proj <- census_chunk %>% st_make_valid()
  target_crs  <- st_crs(blocks_proj)

  # Fast CRS check: If mismatched, project inputs to match the chunk
  if (st_crs(crash_sf)   != target_crs) crash_sf   <- st_transform(crash_sf, target_crs)
  if (st_crs(sld_sf)     != target_crs) sld_sf     <- st_transform(sld_sf, target_crs)
  if (st_crs(wi_sf)      != target_crs) wi_sf      <- st_transform(wi_sf, target_crs)
  
  # Crash Aggregation
  # Calculate area first
  blocks_proj <- blocks_proj %>% 
    mutate(
      area_sqm  = as.numeric(st_area(.)), 
      area_sqkm = area_sqm / 1e6
    )
  
  # Fast Count (st_intersects is much faster than st_join for pure counts)
  crash_hits <- st_intersects(blocks_proj, crash_sf)
  blocks_proj$crash_count <- lengths(crash_hits)
  
  # Environment (SLD/WI) - Centroid Sampling
  # Using centroids avoids "edge" issues where a block touches 2 zones
  block_centroids <- st_centroid(blocks_proj)
  
  sld_wi_data <- block_centroids %>%
    st_join(sld_sf %>% select(D1B, D3b), join = st_intersects) %>%
    st_join(wi_sf  %>% select(NatWalkInd), join = st_intersects) %>%
    st_drop_geometry() %>%
    select(BLOCKID10, D1B, D3b, NatWalkInd)
  
  # Weather (PRISM 4km grid sampled at block centroids)
  clim <- extract_prism(block_centroids, prism_paths)
  
  # Merge & Format Columns
  enriched_chunk <- blocks_proj %>%
    left_join(sld_wi_data, by = "BLOCKID10") %>%
    mutate(across(
      .cols = matches("(_01|_02|COMMUNI|PHARMAC|RETAIL|SUPERMA|UNIVERS|HOSPITA|SOCIAL|COLLEG|DOCTOR|DENTIST)"),
      .fns  = ~as.numeric(as.character(.))
    )) %>%
    # Replace NAs with 0 now that types are corrected
    mutate(across(where(is.numeric), ~tidyr::replace_na(., 0))) %>%
    mutate(
      precip_annual = clim$prcp_annua,
      temp_min      = clim$temp_min,
      temp_max      = clim$temp_max,

      # --- ID & Model Predictors ---
      GEOID10     = BLOCKID10,
      emp_density = D1B,
      int_density = D3b,
      walk_index  = NatWalkInd,
      
      # --- Native Census Columns (Renaming) ---
      pop_low = POP_LOW_ST, pop_high = POP_HIGH_S, 
      housing_total = HOUSING10,
      emp_low = EMP_LOW_ST, emp_high = EMP_HIGH_S,
      
      schools_low = SCHOOLS_LO, schools_high = SCHOOLS_HI,
      colleges_low = COLLEGES_L, colleges_high = COLLEGES_H,
      
      doctors_low = DOCTORS_LO, doctors_high = DOCTORS_HI,
      pharmacies_low = PHARMAC_02, pharmacies_high = PHARMAC_01,
      
      retail_low = RETAIL_LOW, retail_high = RETAIL_HIG,
      supermarket_low = SUPERMA_02, supermarket_high = SUPERMA_01,
      
      parks_low = PARKS_LOW_, parks_high = PARKS_HIGH,
      trails_low = TRAILS_LOW, trails_high = TRAILS_HIG,
      
      community_low = COMMUNI_02, community_high = COMMUNI_01,
      transit_low = TRANSIT_LO, transit_high = TRANSIT_HI,
      
      # --- Density Calculation ---
      crash_density = crash_count / pmax(area_sqkm, 0.001)
    ) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  
  return(enriched_chunk)
}

# --- Combine census chunks and prepare web blocks---
prepare_web_blocks <- function(enriched_chunks, min_sqm_threshold = 1000) {
  
  message("...Finalizing Web Map & Fixing Slivers")
  
  # Ensure SF format and Re-Calculate Area
  if (is.list(enriched_chunks) && !inherits(enriched_chunks, "sf")) {
    # Use do.call(rbind) to safely merge the list of SF objects
    full_blocks <- do.call(rbind, enriched_chunks) %>% st_as_sf()
  } else {
    full_blocks <- st_as_sf(enriched_chunks)
  }
  
  full_blocks <- full_blocks %>% 
    mutate(area_sqm = as.numeric(st_area(.)))
  
  # Identify Slivers vs Keepers
  slivers <- full_blocks %>% filter(area_sqm < min_sqm_threshold)
  keepers <- full_blocks %>% filter(area_sqm >= min_sqm_threshold)
  
  if (nrow(slivers) > 0) {
    # Find nearest "Keeper" for every "Sliver"
    nearest_keeper_idx <- st_nearest_feature(slivers, keepers)
    
    repaired_slivers <- slivers
    
    # --- DETECT GEOMETRY COLUMN NAME ---
    geo_col <- attr(repaired_slivers, "sf_column") # Likely "geom" or "geometry"
    
    cols_to_skip <- c("GEOID10", "BLOCKID10", geo_col, "area_sqm", "area_sqkm")
    cols_to_fix  <- setdiff(names(repaired_slivers), cols_to_skip)
    
    # Overwrite sliver data with neighbor data
    st_geometry(repaired_slivers) <- NULL 
    source_data <- st_drop_geometry(keepers[nearest_keeper_idx, ])
    
    # Copy attributes
    repaired_slivers[, cols_to_fix] <- source_data[, cols_to_fix]
    
    # Instead of st_sf(), manually assign the geometry to the original column name
    repaired_slivers[[geo_col]] <- st_geometry(slivers)
    repaired_slivers <- st_as_sf(repaired_slivers) # Re-activate SF status
    
    # Now names match perfectly ("geom" == "geom")
    final_output <- rbind(keepers, repaired_slivers)
    message("...Fixed ", nrow(slivers), " sliver blocks.")
    
  } else {
    final_output <- keepers
    message("...No slivers found.")
  }
  
  # 3. Final Export Selection & Projection
  final_output %>%
    select(
      GEOID10,
      emp_density, int_density, walk_index,
      pop_low, pop_high, housing_total, emp_low, emp_high,
      schools_low, schools_high, colleges_low, colleges_high,
      doctors_low, doctors_high, pharmacies_low, pharmacies_high,
      retail_low, retail_high, supermarket_low, supermarket_high,
      parks_low, parks_high, trails_low, trails_high,
      community_low, community_high, transit_low, transit_high,
      precip_annual, temp_min, temp_max, crash_density
    ) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    st_transform(4326)
}

