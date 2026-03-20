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

#' Fetch Weather for Multiple Stations and return SF
#' @param station_ids Vector of IDs (e.g., c("SFO", "LAX", "SAC"))
#' @param year The year to fetch
get_spatial_weather <- function(station_ids, year = 2023) {
  library(httr)
  library(jsonlite)
  library(sf)
  library(purrr)
  
  weather_list <- map_dfr(station_ids, function(id) {
    url <- "https://data.rcc-acis.org/StnData"
    
    payload <- list(
      sid = id,
      sdate = as.character(year),
      edate = as.character(year),
      elems = list(list(
        name = "pcpn",
        interval = list(1), 
        duration = 1,
        reduce = "sum"
      ))
    )
    
    res <- POST(url, body = toJSON(payload, auto_unbox = TRUE), 
                add_headers("Content-Type" = "application/json"))
    
    if (status_code(res) == 200) {
      out <- fromJSON(content(res, "text"))
      
      # The API returns data as [["Year", "Value"]]
      # We need the first row (1) and the second column (2)
      raw_val <- out$data[1, 2] 
      
      if (is.null(raw_val) || raw_val == "M") return(NULL)
      
      return(tibble(
        station_id = id,
        prcp_annua = as.numeric(raw_val) * 25.4, # to mm
        lon = as.numeric(out$meta$ll[1]),
        lat = as.numeric(out$meta$ll[2])
      ))
    }
    return(NULL)
  })
  
  # Ensure we have data, then convert to SF
  if (nrow(weather_list) == 0) return(NULL)
  
  weather_list %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(3310)
}

#' Enrich Bike Network Links (The "Spine" Function)
#' This updates EVERY link in the master network with covariates
#' Consolidates Strava, SLD, Walk Index, Crashes, and Weather into one SF object
#' Enrich Strava Spine with OSM by table join (assuming osm_id is correct)
#' 
# --- 1. STABLE BASE ENRICHMENT ---
# Combines OSM, Weather, SLD, and Walk Index
enrich_base_network <- function(strava_sf, osm_sf, sld_sf, wi_sf, weather_sf) {
  message("...Joining OSM, Weather, SLD, and Walk Index")
  
  # Crucial: Turn off S2 to avoid the "Loop 0" vertex errors during st_centroid
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(TRUE)) 
  
  # OSM Table Join
  osm_data <- osm_sf %>%
    st_drop_geometry() %>%
    mutate(osm_id = as.character(osm_id)) %>%
    select(osm_id, any_of(c("infra_type", "is_paved", "speed_limit")))
  
  strava_enriched <- strava_sf %>%
    mutate(osm_ref = as.character(osm_ref)) %>%
    left_join(osm_data, by = c("osm_ref" = "osm_id")) %>%
    st_transform(3310)
  
  # Weather Join (Nearest Station)
  weather_proj <- st_transform(weather_sf, 3310)
  nearest_w_idx <- st_nearest_feature(strava_enriched, weather_proj)
  strava_enriched$prcp_annua <- weather_proj$prcp_annua[nearest_w_idx]
  
  # Polygon Joins (SLD & WI via Centroid)
  centroids_data <- strava_enriched %>% 
    st_centroid() %>%
    st_join(st_transform(sld_sf, 3310) %>% select(D1B, D3b), join = st_intersects) %>%
    st_join(st_transform(wi_sf, 3310) %>% select(NatWalkInd), join = st_intersects) %>%
    st_drop_geometry() %>%
    select(edge_uid, D1B, D3b, NatWalkInd)
  
  strava_enriched %>%
    left_join(centroids_data, by = "edge_uid")
}

# --- 2. CRASH ENRICHMENT (The High-Speed Modular Step) ---
enrich_crashes <- function(strava_sf, crash_sf) {
  # Identifying which chunk this is by its row count
  chunk_id <- paste0("Chunk (", nrow(strava_sf), " rows)")
  
  message(paste(chunk_id, "--- Starting Crash Join ---"))
  
  # Step 1: Bounding Box
  message(paste(chunk_id, ": Step 1/3 - Filtering Crashes by BBox..."))
  chunk_bbox <- st_as_sfc(st_bbox(strava_sf)) %>% st_buffer(100)
  crash_subset <- crash_sf[chunk_bbox, ]
  message(paste(chunk_id, ": Found", nrow(crash_subset), "crashes in vicinity."))
  
  # Step 2: Buffering
  message(paste(chunk_id, ": Step 2/3 - Creating 30m road buffers..."))
  strava_buffer <- st_buffer(strava_sf, 30, endCapStyle = "SQUARE", joinStyle = "MITRE")
  
  # Step 3: Intersection
  message(paste(chunk_id, ": Step 3/3 - Performing Point-in-Polygon check..."))
  intersect_list <- st_intersects(strava_buffer, crash_subset)
  
  strava_sf$crash_count_30m <- lengths(intersect_list)
  
  message(paste(chunk_id, "--- FINISHED ---"))
  return(strava_sf)
}

# --- 3. CENSUS ENRICHMENT (The missing blocks_optimized link)
enrich_census <- function(strava_sf, census_sf) {
  message("...Joining Census Block Data")
  
  # Crucial: S2 off for speed and to avoid vertex errors
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(TRUE))
  
  # 1. Transform census to match network projection (CA Albers)
  census_proj <- st_transform(census_sf, 3310)
  
  # 2. Use Centroids to join attributes
  # This ensures we don't get double-counting from edges crossing boundaries
  census_join <- strava_sf %>%
    st_centroid() %>%
    st_join(census_proj, join = st_intersects) %>%
    st_drop_geometry() %>%
    # Select all census columns except those that might conflict (like geometry)
    select(edge_uid, everything()) 
  
  # 3. Handle NAs (The 'Mexico/Rural' Gap Fix)
  # If a segment didn't hit a US census block, fill numeric cols with 0
  census_join <- census_join %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  
  # 4. Join back to the main network linework
  strava_sf %>%
    left_join(census_join, by = "edge_uid")
}

# --- 4. FINAL MATH ---
calculate_model_features <- function(strava_sf) {
  
  # 1. UNWRAP list
  if (is.list(strava_sf) && !inherits(strava_sf, "sf")) {
    strava_sf <- strava_sf[[1]] 
  }
  
  # 2. SEPARATE Geometry for Speed
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
  
  # 3. HELPER: Clean names for prior joins (Strava, OSM, SLD, WI, Crashes)
  fast_coalesce <- function(target_df, base_name) {
    matched_cols <- grep(paste0("^", base_name, "(\\.[xy])*$"), names(target_df), value = TRUE)
    if (length(matched_cols) == 0) return(rep(NA, nrow(target_df)))
    if (length(matched_cols) == 1) return(target_df[[matched_cols]])
    exec(coalesce, !!!target_df[matched_cols])
  }
  
  # 4. VARIABLE MAPPING & RENAMING
  df_final <- tibble(
    edge_uid         = fast_coalesce(df_clean, "edge_uid"),
    from             = fast_coalesce(df_clean, "from"),
    to               = fast_coalesce(df_clean, "to"),
    strava_vol_total = fast_coalesce(df_clean, "strava_vol_total"),
    
    # --- BUILT ENVIRONMENT (From Prior SLD/WI Joins) ---
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
    
    # --- ENVIRONMENTAL/SAFETY (Prior Weather/Crash Joins) ---
    precip_annual    = fast_coalesce(df_clean, "prcp_annua"),
    crash_count_30m  = replace_na(fast_coalesce(df_clean, "crash_count_30m"), 0)
  ) %>%
    mutate(
      # Bias adjustment calculation
      stra_leisure = fast_coalesce(df_clean, "strava_leisure"),
      recr_prop    = replace_na(stra_leisure / pmax(strava_vol_total, 1), 0),
      
      # Mintu et al. Weekend-Weekday Index (WWI) Calculation
      WWI = 0.55134 - 0.04631 * log10(pmax(emp_density, 1)) + 0.61717 * (recr_prop^2),
      
      # Convert infrastructure to factors for the Random Forest
      infra_type = as.factor(infra_type),
      is_paved   = as.factor(is_paved)
    )
  
  # 5. REATTACH Geometry
  final_sf <- st_set_geometry(df_final, geom_backup)
  
  return(final_sf)
}

# --- Builds the network once (over 1 hr runtime) ---
prep_network_topology <- function(enriched_links) {
  message("...Building Network Topology (This is the slow part...)")
  
  # 1. Build Topology
  topo <- build_topology_from_links(enriched_links)
  links_final <- topo$links
  nodes_geom  <- topo$nodes
  
  if(!"edge_uid" %in% names(links_final)) stop("Error: 'edge_uid' column missing.")
  
  # 2. Clean Attributes
  links_final <- links_final %>%
    mutate(
      across(matches("pharmacies|supermarket|schools|retail|parks|transit"), ~replace_na(as.numeric(.), 0)),
      infra_type = replace_na(as.character(infra_type), "other"),
      is_paved = as.numeric(replace_na(is_paved, 1))
    )
  
  # 3. Aggregate Node Data (for Peds)
  node_data <- links_final %>%
    st_drop_geometry() %>%
    pivot_longer(cols = c(from, to), values_to = "node_id") %>%
    group_by(node_id) %>%
    summarise(
      across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
      across(where(is.character), ~first(.x)),
      .groups = "drop"
    )
  
  nodes_final <- inner_join(nodes_geom, node_data, by = "node_id")
  
  # Return a list containing the Cleaned Network
  return(list(links = links_final, nodes = nodes_final))
}

# --- Snaps counts to the pre-built network ---
snap_counts_to_network <- function(counts_df, network_list) {
  message("...Snapping Counts to Pre-Built Network")
  
  links_final <- network_list$links
  nodes_final <- network_list$nodes
  
  # --- SNAP BIKE COUNTS ---
  
  # Initialize a list to collect results (prevents crash)
  all_bike_matches_list <- list()
  
  # 1. Prepare Raw Data
  bike_raw <- counts_df %>% 
    filter(mode == "bike", !is.na(aadt)) %>%
    rename(raw_count = aadt)
  
  if(nrow(bike_raw) > 0) {
    bike_sf <- bike_raw %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(st_crs(links_final)) %>%
      mutate(
        direction = if("direction" %in% names(.)) direction else NA,
        count_angle = case_when(
          grepl("^N", direction, ignore.case=T) ~ 0,
          grepl("^S", direction, ignore.case=T) ~ 180,
          grepl("^E", direction, ignore.case=T) ~ 90,
          grepl("^W", direction, ignore.case=T) ~ 270,
          TRUE ~ NA_real_
        )
      )
    
    # 2. Undirected Snapping
    undirected_bikes <- bike_sf %>% filter(is.na(count_angle))
    if(nrow(undirected_bikes) > 0) {
      nearest_rows <- st_nearest_feature(undirected_bikes, links_final)
      
      # Save matched undirected
      bike_matches_undirected <- links_final[nearest_rows, ] %>%
        bind_cols(st_drop_geometry(undirected_bikes) %>% 
                    select(aadb=raw_count, source, spatial_id, year))
      
      # Add to our collection list
      all_bike_matches_list[[length(all_bike_matches_list) + 1]] <- bike_matches_undirected
    }
    
    # 3. Directed Snapping
    directed_bikes <- bike_sf %>% filter(!is.na(count_angle))
    if(nrow(directed_bikes) > 0) {
      matches <- st_join(directed_bikes, links_final, join = st_is_within_distance, dist = 50)
      
      unique_uids <- unique(matches$edge_uid)
      relevant_links <- links_final %>% filter(edge_uid %in% unique_uids)
      
      # Vectorized Bearing Calc
      coords <- st_coordinates(relevant_links)
      link_coords <- as.data.frame(coords) %>%
        group_by(L1) %>%
        summarize(start_x=first(X), start_y=first(Y), end_x=last(X), end_y=last(Y), .groups="drop")
      
      rad2deg <- function(rad) {(rad * 180 / (pi)) %% 360}
      calculated_bearings <- rad2deg(atan2(link_coords$end_x - link_coords$start_x, 
                                           link_coords$end_y - link_coords$start_y))
      
      bearing_lookup <- tibble(edge_uid = relevant_links$edge_uid, link_bearing = calculated_bearings)
      
      best_directed <- matches %>%
        st_drop_geometry() %>% 
        left_join(bearing_lookup, by = "edge_uid") %>%
        mutate(
          diff = abs(count_angle - link_bearing),
          diff = pmin(diff, 360 - diff),
          is_aligned = diff < 45 | abs(diff - 180) < 45
        ) %>%
        filter(is_aligned) %>%
        group_by(spatial_id, year) %>%
        slice(1) %>%
        ungroup()
      
      # Bike directed matched
      bike_matches_directed <- links_final %>%
        right_join(best_directed %>% select(edge_uid, aadb=raw_count, source, spatial_id, year), 
                   by = "edge_uid")
      
      # Add to our collection list
      all_bike_matches_list[[length(all_bike_matches_list) + 1]] <- bike_matches_directed
    }
  }
  
  # Final binding of matches
  links_train <- bind_rows(all_bike_matches_list)

  
  # --- SNAP PED COUNTS  ---
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
enrich_census_chunk <- function(census_chunk, sld_sf, wi_sf, crash_sf, weather_sf) {
  
  # 1. Setup & CRS Safety
  # Ensure the chunk is valid and ready
  blocks_proj <- census_chunk %>% st_make_valid()
  target_crs  <- st_crs(blocks_proj)
  
  # Fast CRS check: If mismatched, project inputs to match the chunk
  if (st_crs(crash_sf)   != target_crs) crash_sf   <- st_transform(crash_sf, target_crs)
  if (st_crs(sld_sf)     != target_crs) sld_sf     <- st_transform(sld_sf, target_crs)
  if (st_crs(wi_sf)      != target_crs) wi_sf      <- st_transform(wi_sf, target_crs)
  if (st_crs(weather_sf) != target_crs) weather_sf <- st_transform(weather_sf, target_crs)
  
  # 2. Crash Aggregation (Optimized Fast Path)
  # Calculate area first
  blocks_proj <- blocks_proj %>% 
    mutate(
      area_sqm  = as.numeric(st_area(.)), 
      area_sqkm = area_sqm / 1e6
    )
  
  # Fast Count (st_intersects is much faster than st_join for pure counts)
  crash_hits <- st_intersects(blocks_proj, crash_sf)
  blocks_proj$crash_count <- lengths(crash_hits)
  
  # 3. Environment (SLD/WI) - Centroid Sampling
  # Using centroids avoids "edge" issues where a block touches 2 zones
  block_centroids <- st_centroid(blocks_proj)
  
  sld_wi_data <- block_centroids %>%
    st_join(sld_sf %>% select(D1B, D3b), join = st_intersects) %>%
    st_join(wi_sf  %>% select(NatWalkInd), join = st_intersects) %>%
    st_drop_geometry() %>%
    select(BLOCKID10, D1B, D3b, NatWalkInd)
  
  # 4. Weather (Nearest Neighbor)
  nearest_idx  <- st_nearest_feature(block_centroids, weather_sf)
  weather_vals <- weather_sf$prcp_annua[nearest_idx]
  
  # 5. Merge & Format Columns
  enriched_chunk <- blocks_proj %>%
    left_join(sld_wi_data, by = "BLOCKID10") %>%
    mutate(across(
      .cols = matches("(_01|_02|COMMUNI|PHARMAC|RETAIL|SUPERMA|UNIVERS|HOSPITA|SOCIAL|COLLEG|DOCTOR|DENTIST)"),
      .fns  = ~as.numeric(as.character(.))
    )) %>%
    # Replace NAs with 0 now that types are corrected
    mutate(across(where(is.numeric), ~tidyr::replace_na(., 0))) %>%
    mutate(
      precip_annual = weather_vals,
      
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
  
  # 1. Ensure SF format and Re-Calculate Area
  if (is.list(enriched_chunks) && !inherits(enriched_chunks, "sf")) {
    # Use do.call(rbind) to safely merge the list of SF objects
    full_blocks <- do.call(rbind, enriched_chunks) %>% st_as_sf()
  } else {
    full_blocks <- st_as_sf(enriched_chunks)
  }
  
  full_blocks <- full_blocks %>% 
    mutate(area_sqm = as.numeric(st_area(.)))
  
  # 2. Identify Slivers vs Keepers
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
    
    # Instead of st_sf(), we manually assign the geometry to the original column name
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
      precip_annual, crash_density
    ) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    st_transform(4326)
}

