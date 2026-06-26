library(sf)
library(dplyr)
library(readr)
library(osmextract)
library(purrr)


#' Load Strava Network (Parallelized)
#' Handles hashed filenames, truncated columns, and Case Sensitivity.
load_strava_network <- function(zip_files) {
  
  if (length(zip_files) == 0) {
    stop("No zip files found! Check your data_raw folder.")
  }
  
  message("Processing ", length(zip_files), " Strava zip files in PARALLEL...")
  
  # --- Reading strava in parallel ---
  all_districts <- future_map(zip_files, function(zip_path) {
    
    # Need to explicitly load packages inside parallel workers
    library(sf)
    library(tidyverse)
    library(zip)
    
    # Create a unique temp directory
    # Use a random string to ensure no conflicts between parallel workers
    temp_dir <- file.path(tempdir(), paste0("str_", paste(sample(letters, 8), collapse="")))
    dir.create(temp_dir, showWarnings = FALSE)
    
    # Extract ALL files
    unzip(zip_path, exdir = temp_dir)
    
    # Identify and RENAME files
    shp_raw <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
    
    if (is.na(shp_raw)) {
      unlink(temp_dir, recursive = TRUE)
      return(NULL)
    }
    
    base_hash <- tools::file_path_sans_ext(basename(shp_raw))
    all_assoc_files <- list.files(temp_dir, pattern = paste0("^", base_hash), full.names = TRUE)
    sapply(all_assoc_files, function(f) {
      ext <- tools::file_ext(f)
      file.rename(f, file.path(temp_dir, paste0("edges.", ext)))
    })
    
    # Read Geometry
    geo_path <- file.path(temp_dir, "edges.shp")
    geo <- st_read(geo_path, quiet = TRUE)
    
    # Column name consistency
    orig_names <- names(geo)
    lower_names <- tolower(orig_names)
    
    id_idx <- grep("uid|edge_id|edgeid", lower_names)[1]
    if (is.na(id_idx)) id_idx <- 1
    id_col_real_name <- orig_names[id_idx]
    
    osm_idx <- grep("osm|reference", lower_names)[1]
    
    if (!is.na(osm_idx)) {
      osm_col_real_name <- orig_names[osm_idx]
      geo <- geo %>% select(edge_uid = all_of(id_col_real_name), osm_ref = all_of(osm_col_real_name))
    } else {
      geo <- geo %>% select(edge_uid = all_of(id_col_real_name)) %>% mutate(osm_ref = NA_character_)
    }
    
    geo <- geo %>% st_transform(4326)
    
    # Read CSV and Join
    csv_path <- file.path(temp_dir, "edges.csv")
    
    if (file.exists(csv_path)) {
      counts <- read_csv(csv_path, show_col_types = FALSE) %>%
        select(edge_uid, 
               total_trip_count, 
               forward_commute_trip_count, reverse_commute_trip_count, 
               forward_leisure_trip_count, reverse_leisure_trip_count) %>%
        group_by(edge_uid) %>%
        summarize(
          strava_vol_total = sum(total_trip_count, na.rm = TRUE),
          strava_commute = sum(forward_commute_trip_count + reverse_commute_trip_count, na.rm = TRUE),
          strava_leisure = sum(forward_leisure_trip_count + reverse_leisure_trip_count, na.rm = TRUE),
          .groups = "drop"
        )
      
      geo$edge_uid <- as.character(geo$edge_uid)
      counts$edge_uid <- as.character(counts$edge_uid)
      
      geo <- geo %>%
        left_join(counts, by = "edge_uid") %>%
        mutate(
          strava_vol_total = replace_na(strava_vol_total, 0),
          strava_commute = replace_na(strava_commute, 0),
          strava_leisure = replace_na(strava_leisure, 0)
        )
    } else {
      geo <- geo %>% mutate(strava_vol_total = 0, strava_commute = 0, strava_leisure = 0)
    }
    
    unlink(temp_dir, recursive = TRUE)
    return(geo)
    
  }, .options = furrr_options(seed = TRUE)) # Enable random seed for safety
  
  all_districts <- all_districts[!sapply(all_districts, is.null)]
  
  message("Merging ", length(all_districts), " districts...")
  bind_rows(all_districts)
}

#' Download OSM Reference Data
#' We download OSM just to get the attributes (Speed, Facility Type, etc.).
get_statewide_osm <- function() {
  library(osmextract)
  library(sf)
  
  # Downloads the 1.2GB California file once, then reads it.
  dir.create("data_raw/osm", showWarnings = FALSE, recursive = TRUE)
  # We select only the columns needed for joining (osm_id) and plotting (highway).
  ca_osm <- oe_get(
    place = "California",
    layer = "lines",
    download_directory = "data_raw/osm",
    extra_tags = c("surface", "maxspeed", "bicycle", "cycleway"),
    query = "SELECT osm_id, highway, surface, maxspeed, bicycle, cycleway, geometry FROM lines WHERE highway IS NOT NULL",
    quiet = FALSE
  )
  
  return(ca_osm)
}

#' Process osm tags
process_osm_tags <- function(osm_raw) {
  message("...Performing reclass of OSM tags")
  
  osm_raw %>%
    st_drop_geometry() %>%
    mutate(
      osm_id = as.character(osm_id),
      
      # INFRASTRUCTURE TYPE (Hierarchical: Best feature wins)
      infra_type = case_when(
        # High Comfort: Separated from cars
        highway %in% c("cycleway", "path", "footway", "pedestrian") | 
          cycleway %in% c("track", "separate", "sidepath") ~ "separated_path",
        
        # Medium-High: On-street but buffered
        cycleway %in% c("buffered_lane") ~ "buffered_lane",
        
        # Medium: Standard dedicated lanes
        cycleway %in% c("lane", "left", "right", "both") ~ "bike_lane",
        
        # Medium-Low: Shared but designated/marked
        cycleway == "shared_lane" | bicycle == "shared_lane" ~ "shared_lane_marked",
        
        # Low Stress: No markings, but low car volume/speed
        highway %in% c("residential", "living_street") ~ "quiet_street",
        
        # High Stress: Mixing with high-speed traffic
        highway %in% c("primary", "secondary", "tertiary", "trunk") ~ "shared_arterial",
        
        TRUE ~ "other"
      ),
      
      # SURFACE QUALITY (Binary: paved or not)
      # This handles your 160+ types by looking for "dirt-like" keywords
      is_paved = case_when(
        str_detect(surface, "asphalt|paved|concrete|paving_stones|sett|bricks|cement") ~ 1,
        str_detect(surface, "dirt|gravel|ground|unpaved|sand|earth|mud|grass|rock|wood") ~ 0,
        is.na(surface) & !highway %in% c("path", "track", "bridleway") ~ 1, # Assume roads are paved
        TRUE ~ 0
      ),
      
      # SPEED LIMIT (With functional class fallbacks)
      speed_extracted = as.numeric(str_extract(maxspeed, "\\d+")),
      speed_limit = case_when(
        !is.na(speed_extracted) ~ speed_extracted,
        highway == "motorway" ~ 65,
        highway %in% c("trunk", "primary") ~ 45,
        highway == "secondary" ~ 35,
        highway == "tertiary" ~ 25,
        highway %in% c("residential", "living_street", "service") ~ 25,
        infra_type == "separated_path" ~ 15,
        TRUE ~ 25
      ),

      # FUNCTIONAL CLASS (road hierarchy, from the raw OSM `highway` tag -- a
      # DIFFERENT axis than infra_type, which describes bike facility. Major =
      # high-speed arterials/highways; Minor = secondary/tertiary collectors;
      # Local = residential / paths / everything else.)
      functional = case_when(
        highway %in% c("motorway", "trunk", "primary") ~ "Major Road",
        highway %in% c("secondary", "tertiary")        ~ "Minor Road",
        TRUE                                           ~ "Local Road"
      )
    ) %>%
    # Keep both axes: infra_type (bike facility) AND functional (road hierarchy).
    select(osm_id, infra_type, is_paved, speed_limit, functional)
}

#' Match Strava with OSM Attributes
#' This performs the Spatial Join: Strava (Target) + OSM (Attributes)
match_strava_to_osm <- function(strava_base, osm_processed) {
  options(scipen = 999) # Keep scientific notation off
  
  # Ensure character types
  strava_base <- strava_base %>% mutate(osm_ref = as.character(osm_ref))
  osm_processed <- osm_processed %>% mutate(osm_id = as.character(osm_id))
  
  # The Join
  result <- left_join(strava_base, osm_processed, by = c("osm_ref" = "osm_id"))

  # Deduplicate edge_uid. The per-district Strava clips overlap at Caltrans
  # district boundaries, so the same physical edge (identical geometry AND
  # strava volume) appears in two files -> duplicate edge_uid rows. Verified as
  # true duplicates.
  #
  # Keep the first occurrence per edge_uid; geometry/volume are identical so this
  # is lossless. Guards against OSM-join multiplication too.
  if ("edge_uid" %in% names(result)) {
    n_before <- nrow(result)
    result   <- result[!duplicated(result$edge_uid), ]
    if (n_before > nrow(result)) {
      message(paste0("--- Dropped ", n_before - nrow(result),
                     " duplicate edge_uid rows (boundary overlaps) ---"))
    }
  }

  # diagnostic check
  total_rows <- nrow(result)
  matched_rows <- sum(!is.na(result$infra_type)) # Check infra_type, not highway!
  match_rate <- round((matched_rows / total_rows) * 100, 2)

  message(paste0("--- Join Success: ", match_rate, "% ---"))

  return(result)
}

#' Build Network Topology (from and to)
#' Generates stable IDs for links and creates matching node geometries.
#' Returns a list: list(links = sf_object, nodes = sf_object)
build_topology_from_links <- function(links_sf) {
  library(sf)
  library(dplyr)

  message("...Building Topology: Generating stable IDs and Nodes")

  # GENERATE LINK IDs based on coordinates. If coordinates change we get a new
  # ID; otherwise IDs are stable across rebuilds (the key is the rounded
  # coordinate, not row order).
  coords <- st_coordinates(links_sf)
  L1 <- coords[, "L1"]
  n  <- nrow(coords)

  # Start index of each line = first row where L1 changes; end index = last row
  # before the next line. Vectorized: no per-line match()/rev() scan.
  is_start <- c(TRUE, L1[-1] != L1[-n])
  start_idx <- which(is_start)
  end_idx   <- c(start_idx[-1] - 1L, n)

  # Coordinate key "x_y" rounded to 6 dp -- STRING key, stable across rebuilds
  # (same coordinate always yields the same id). Only the endpoints
  # are stringified, not every vertex.
  hash_xy <- function(idx) {
    sprintf("%s_%s", as.character(round(coords[idx, 1], 6)),
                     as.character(round(coords[idx, 2], 6)))
  }
  links_sf$from <- hash_xy(start_idx)
  links_sf$to   <- hash_xy(end_idx)

  # GENERATE NODES -- one geometry per unique node_id.
  # Interleave start/end so node_ids_ordered[i] matches endpoint row i below.
  node_ids_ordered <- c(rbind(links_sf$from, links_sf$to))
  endpoint_rows    <- c(rbind(start_idx, end_idx))   # coord rows, same interleave

  # Deduplicate on node_id: take the coord rows at the line endpoints, keep the 
  # first occurrence of each node_id.
  keep <- !duplicated(node_ids_ordered)
  nodes_sf <- st_as_sf(
    data.frame(
      node_id = node_ids_ordered[keep],
      X = coords[endpoint_rows[keep], 1],
      Y = coords[endpoint_rows[keep], 2]
    ),
    coords = c("X", "Y"), crs = st_crs(links_sf)
  )

  return(list(links = links_sf, nodes = nodes_sf))
}


#' Map volumes from links to nodes and nodes to links
map_volumes_across_network <- function(links, nodes) {
  require(sf)
  require(dplyr)
  require(tidyr)
  
  # Assign Link Bike Volume to Nodes ---
  # Identify all links connected to a node and take the MAX volume
  
  link_bike_to_node <- links %>%
    st_drop_geometry() %>%
    select(from, to, pred_bike_vol) %>%
    # Pivot so we have a list of all node IDs and the volumes touching them
    pivot_longer(cols = c(from, to), values_to = "node_id") %>%
    group_by(node_id) %>%
    summarise(pred_bike_vol = max(pred_bike_vol, na.rm = TRUE), .groups = "drop")
  
  nodes_updated <- nodes %>%
    left_join(link_bike_to_node, by = "node_id") %>%
    mutate(pred_bike_vol = replace_na(pred_bike_vol, 0))
  
  # Assign Node Ped Volume to Links ---
  # Aaverage pedestrian volume of the 'source' and 'target' nodes
  node_ped_lookup <- nodes %>%
    st_drop_geometry() %>%
    select(node_id, pred_ped_vol)
  
  links_updated <- links %>%
    # Join start node volume
    left_join(node_ped_lookup, by = c("from" = "node_id")) %>%
    rename(ped_vol_start = pred_ped_vol) %>%
    # Join end node volume
    left_join(node_ped_lookup, by = c("to" = "node_id")) %>%
    rename(ped_vol_end = pred_ped_vol) %>%
    # Calculate average
    mutate(
      pred_ped_vol = (replace_na(ped_vol_start, 0) + replace_na(ped_vol_end, 0)) / 2
    ) %>%
    select(-ped_vol_start, -ped_vol_end)
  
  return(list(links = links_updated, nodes = nodes_updated))
}

#' Setup network for web with only necessary fields
finalize_web_network <- function(links, nodes) {
  require(dplyr)
  require(sf)

  # `functional` (road hierarchy: Major/Minor/Local Road) is carried through
  # from OSM `highway` via process_osm_tags -- it is NOT derived from infra_type
  # (bike facility). Links keep their own functional; nodes inherit the highest
  # functional of their touching links (set in prep_network_topology).

  # --- Helper: Exposure Class Calculation (Low/Medium/High) ---
  get_exposure_class <- function(vec) {
    # Default to Low if all values are 0 or NA
    if(all(vec == 0, na.rm = TRUE)) return(rep("Low", length(vec)))
    
    # Calculate quantiles based only on non-zero volumes
    vals <- vec[vec > 0 & !is.na(vec)]
    if(length(vals) < 3) return(rep("Low", length(vec)))
    
    breaks <- quantile(vals, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
    
    # Cut into 3 categories
    classes <- as.character(cut(vec, 
                                breaks = c(-Inf, breaks[2], breaks[3], Inf), 
                                labels = c("Low", "Medium", "High"), 
                                include.lowest = TRUE))
    
    # Catch any NAs and default them to Low
    classes[is.na(classes)] <- "Low"
    return(classes)
  }
  
  # --- Process Links ---
  # `functional` is already present (carried from OSM); just compute length and
  # the exposure classes.
  links_final <- links %>%
    mutate(
      length_ft = as.numeric(st_length(.)) * 3.28084,
      bicycle_exposure_class = get_exposure_class(pred_bike_vol),
      pedestrian_exposure_class = get_exposure_class(pred_ped_vol)
    ) %>%
    select(
      edge_uid, pred_bike_vol, length_ft, bicycle_exposure_class,
      functional, infra_type, pred_ped_vol, pedestrian_exposure_class,
      source = from, target = to
    )
  
  # --- Process Nodes ---
  # `functional` is already present (highest class of touching links, set in
  # prep_network_topology); just compute the exposure classes.
  nodes_final <- nodes %>%
    mutate(
      bicycle_exposure_class = get_exposure_class(pred_bike_vol),
      pedestrian_exposure_class = get_exposure_class(pred_ped_vol)
    ) %>%
    select(
      node_id, pred_ped_vol, functional, infra_type, 
      pedestrian_exposure_class, pred_bike_vol, bicycle_exposure_class
    )
  
  return(list(links = links_final, nodes = nodes_final))
}