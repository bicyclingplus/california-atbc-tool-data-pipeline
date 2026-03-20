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
  
  # --- CHANGE: Use future_map instead of map ---
  all_districts <- future_map(zip_files, function(zip_path) {
    
    # Need to explicitly load packages inside parallel workers
    library(sf)
    library(tidyverse)
    library(zip)
    
    # A. Create a unique temp directory
    # Use a random string to ensure no conflicts between parallel workers
    temp_dir <- file.path(tempdir(), paste0("str_", paste(sample(letters, 8), collapse="")))
    dir.create(temp_dir, showWarnings = FALSE)
    
    # Extract ALL files
    unzip(zip_path, exdir = temp_dir)
    
    # B. Identify and RENAME files
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
    
    # C. Read Geometry
    geo_path <- file.path(temp_dir, "edges.shp")
    geo <- st_read(geo_path, quiet = TRUE)
    
    # Robust Column Selection
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
    
    # D. Read CSV and Join
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
#' We download OSM just to get the attributes (Speed, Facility Type).
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
      
      # 1. INFRASTRUCTURE TYPE (Hierarchical: Best feature wins)
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
      
      # 2. SURFACE QUALITY (Binary: Is it rideable for a road bike?)
      # This handles your 160+ types by looking for "dirt-like" keywords
      is_paved = case_when(
        str_detect(surface, "asphalt|paved|concrete|paving_stones|sett|bricks|cement") ~ 1,
        str_detect(surface, "dirt|gravel|ground|unpaved|sand|earth|mud|grass|rock|wood") ~ 0,
        is.na(surface) & !highway %in% c("path", "track", "bridleway") ~ 1, # Assume roads are paved
        TRUE ~ 0
      ),
      
      # 3. SPEED LIMIT (With functional fallbacks)
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
      )
    ) %>%
    # Rename variables to match your bike_features list
    select(osm_id, infra_type, is_paved, speed_limit)
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
  
  # The CORRECT diagnostic check
  total_rows <- nrow(result)
  matched_rows <- sum(!is.na(result$infra_type)) # Check infra_type, not highway!
  match_rate <- round((matched_rows / total_rows) * 100, 2)
  
  message(paste0("--- Join Success: ", match_rate, "% ---"))
  
  return(result)
}

#' Build Network Topology (from and to)
#' Generates stable IDs for links and creates perfectly matching node geometries.
#' Returns a list: list(links = sf_object, nodes = sf_object)
build_topology_from_links <- function(links_sf) {
  library(sf)
  library(dplyr)
  library(digest)
  
  message("...Building Topology: Generating stable IDs and Nodes")
  
  # 1. GENERATE LINK IDs
  # We use MD5 hashing of coordinates to ensure ID stability
  coords <- st_coordinates(links_sf)
  
  # Identify start/end indices for each line
  start_idx <- match(unique(coords[,"L1"]), coords[,"L1"])
  end_idx   <- nrow(coords) - match(unique(coords[,"L1"]), rev(coords[,"L1"])) + 1
  
  # Helper to make hash
  get_hash <- function(idx) {
    # Format: "X_Y" rounded to 6 decimals
    txt <- paste(round(coords[idx, 1], 6), round(coords[idx, 2], 6), sep="_")
    purrr::map_chr(txt, ~digest::digest(.x, algo="md5", serialize=FALSE))
  }
  
  # Assign IDs to the links
  links_sf$from <- get_hash(start_idx)
  links_sf$to   <- get_hash(end_idx)
  
  # 2. GENERATE NODES
  # We extract the geometry immediately using the same IDs
  node_ids_ordered <- c(rbind(links_sf$from, links_sf$to)) # Interleave Start, End
  
  # Create simple node SF
  nodes_sf <- coords %>%
    as_tibble() %>%
    group_by(L1) %>%
    slice(c(1, n())) %>% # Keep first and last points
    ungroup() %>%
    mutate(node_id = node_ids_ordered) %>%
    distinct(node_id, .keep_all = TRUE) %>% # Remove duplicates
    st_as_sf(coords = c("X", "Y"), crs = st_crs(links_sf))
  
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
  
  # --- Helper 1: Functional Class Mapping ---
  map_func <- function(type) {
    case_when(
      type %in% c("motorway", "trunk", "primary", "boulevard") ~ "Major Road",
      type %in% c("secondary", "tertiary", "shared_arterial") ~ "Minor Road",
      TRUE ~ "Local Road"
    )
  }
  
  # --- Helper 2: Exposure Class Calculation (Low/Medium/High) ---
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
    
    # Catch any NAs that slipped through and default them to Low
    classes[is.na(classes)] <- "Low"
    return(classes)
  }
  
  # --- Process Links ---
  links_final <- links %>%
    mutate(
      length_ft = as.numeric(st_length(.)) * 3.28084,
      functional = map_func(infra_type),
      # Calculate the missing exposure classes!
      bicycle_exposure_class = get_exposure_class(pred_bike_vol),
      pedestrian_exposure_class = get_exposure_class(pred_ped_vol)
    ) %>%
    select(
      edge_uid, pred_bike_vol, length_ft, bicycle_exposure_class,
      functional, infra_type, pred_ped_vol, pedestrian_exposure_class,
      source = from, target = to
    )
  
  # --- Process Nodes ---
  nodes_final <- nodes %>%
    mutate(
      functional = map_func(infra_type),
      # Calculate the missing exposure classes!
      bicycle_exposure_class = get_exposure_class(pred_bike_vol),
      pedestrian_exposure_class = get_exposure_class(pred_ped_vol)
    ) %>%
    select(
      node_id, pred_ped_vol, functional, infra_type, 
      pedestrian_exposure_class, pred_bike_vol, bicycle_exposure_class
    )
  
  return(list(links = links_final, nodes = nodes_final))
}