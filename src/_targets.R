library(targets)
library(tarchetypes)
library(furrr)
library(crew)

# Function to read and write data to directory of store (Box, etc.)
data_io <- function(sub_path) {
  # 1. Get the store path (e.g., "C:/Users/You/Box/Project/_targets")
  store_path <- targets::tar_config_get("store")
  
  # 2. Go up one level to get the Project Root on Box
  box_root <- dirname(store_path)
  
  # 3. Join with your requested file sub-path
  file.path(box_root, sub_path)
}

# Source all functions (including src/functions/expansions.R)
tar_source("src/functions")

tar_option_set(
  packages = c("tidyverse", "sf", "lubridate", "osmextract", "nngeo", "zip",
               "ranger", "readr", "purrr", "furrr", "httr", "jsonlite"),
  controller = crew_controller_local(workers = 8), # adjust depending on RAM for target
  memory = "persistent", # Keep data in RAM so workers don't reload from disk
  garbage_collection = TRUE
)

# ============================================================================
# Weather Stations
# ============================================================================
ca_stations <- c(
  "ACV", "CEC", "EKA", "UKI", "STS", "FRBC1", "SCOC1",
  "SFO", "OAK", "SJC", "APC", "LVK", "HWD", "MRY", "SNS", "SBP", "SBA", "OXR", "KENC1",
  "SAC", "SMF", "RDD", "DAVC1", "CHUC1", "AUBC1", "MHS", "BLU", "AAT", "O05",
  "MOD", "FAT", "VIS", "BFL", "MCE", "MER", "USC00045502",
  "TVL", "TRK", "BIH", "MMH", "RNO", "BODC1", "DWNC1",
  "LAX", "BUR", "LGB", "SNA", "VNY", "ONT", "PMD", "WJF", "CQT",
  "PSP", "TRM", "IPL", "SBD", "RIV", "RIR", "DAG", "NID", "EDW",
  "SAN", "CRQ", "SEE", "RNM", "NKX", "CZZ"
)


list(
  # ============================================================================
  # SECTION 1: NETWORK & INFRASTRUCTURE
  # ============================================================================
  tar_target(
    strava_zip_files, 
    list.files(data_io("data_raw/strava_caltrans_district_2023"), 
               pattern = "all_edges_yearly_.*\\.zip$", 
               full.names = TRUE)
  ),
  
  tar_target(strava_raw, load_strava_network(strava_zip_files)),
  tar_target(osm_reference, get_statewide_osm()),
  tar_target(osm_processed, process_osm_tags(osm_reference)),
  
  tar_target(
    master_network,
    match_strava_to_osm(strava_raw, osm_processed)
  ),
  
  # ============================================================================
  # SECTION 2: CONTEXT LAYERS (SLD, WI, Crash, Weather)
  # ============================================================================
  
  tar_target(sld_shp_file, data_io("data_raw/SLD/SmartLocationDb.shp"), format = "file"),
  tar_target(wi_shp_file, data_io("data_raw/SLD/Natl_WI.shp"), format = "file"),
  
  tar_target(
    sld_data,
    st_read(sld_shp_file, layer = "SmartLocationDb") %>%
      select(
        GEOID10, 
        D1B,      # Population Density (Gross)
        D2A_JPHH, # Jobs per Household
        D3b,      # Street Network Density
        D4a,      # Distance to Transit
        geometry
      ) %>%
      st_transform(3310)
  ),
  
  tar_target(
    wi_data,
    st_read(wi_shp_file, quiet = TRUE) %>%
      st_transform(3310) %>%
      select(GEOID10, NatWalkInd)
  ),
  
  # NOAA Weather Data (Annual Precipitation)
  tar_target(
    weather_data,
    get_spatial_weather(ca_stations, 2023)
  ),
  
  # SWITRS Relational Tables
  tar_target(switrs_col_file, data_io("data_raw/switrs_2019_2023/CRASH_PED_BIKE_2019-2023_20240912.csv"), format = "file"),
  tar_target(switrs_party_file, data_io("data_raw/switrs_2019_2023/PARTY_PED_BIKE_2019-2023_20240912.csv"), format = "file"),
  tar_target(switrs_victim_file, data_io("data_raw/switrs_2019_2023/VICTIM_PED_BIKE_2019-2023_20240912.csv"), format = "file"),
  
  # Process and project crashes immediately
  tar_target(
    processed_crash_proj, 
    prep_switrs_crashes(switrs_col_file, switrs_party_file, switrs_victim_file)
  ),
  
  tar_target(
    census_blocks,
    st_read(data_io("data_raw/census/blocks_optimized.gpkg"), quiet = TRUE) %>% 
      st_transform(3310)
  ),
  # ============================================================================
  # SECTION 3: COUNT DATA (Ground Truth)
  # ============================================================================
  tar_target(ucb_bike_file, data_io("data_raw/ucb_bike_AADB/Model_clean_data_july23_AADBT.csv"), format = "file"),
  tar_target(ucb_ped_file, data_io("data_raw/ucb_ped_AADP/PSIP_allvars_20200503_wSHS_newFC.csv"), format = "file"),
  
  tar_target(ucb_bike_clean, load_ucb_bike(ucb_bike_file)),
  tar_target(ucb_ped_clean, load_ucb_ped(ucb_ped_file)),
  
  tar_target(caltrans_bike_files, list.files(data_io("data_raw/atd"), pattern = "caltrans_bicycle.*\\.csv", full.names = TRUE)),
  tar_target(caltrans_ped_files, list.files(data_io("data_raw/atd"), pattern = "caltrans_pedestrian.*\\.csv", full.names = TRUE)),
  
  tar_target(caltrans_bike_clean, process_caltrans_counts(caltrans_bike_files, mode = "bike")),
  tar_target(caltrans_ped_clean, process_caltrans_counts(caltrans_ped_files, mode = "ped")),
  
  tar_target(all_bike_points, bind_rows(ucb_bike_clean, caltrans_bike_clean)),
  tar_target(all_ped_points, bind_rows(ucb_ped_clean, caltrans_ped_clean)),
  
  tar_target(ground_truth_counts, bind_rows( all_bike_points,all_ped_points)),
  
  # ============================================================================
  # SECTION 4: DATA ENRICHMENT (FIXED & CLEANED)
  # ============================================================================
  
  # 1. Split the Strava spine once
  tar_group_count(
    strava_chunks,
    master_network, 
    count = 8
  ),
  
  # 2. Base Joins (OSM, SLD, WI, Weather - all together)
  tar_target(
    strava_base,
    enrich_base_network(strava_chunks, osm_processed, sld_data, wi_data, weather_data),
    pattern = map(strava_chunks),
    iteration = "list"
  ),
  
  # 3. Census joins
  tar_target(
    strava_with_census,
    enrich_census(strava_base, census_blocks),
    pattern = map(strava_base),
    iteration = "list"
  ),
  
  # 4. Final Math
  tar_target(
    enriched_strava_chunks,
    calculate_model_features(strava_with_census),
    pattern = map(strava_with_census),
    iteration = "list"
  ),
  
  # 5. Re-combine into Master
  tar_target(
    enriched_bike_network,
    bind_rows(enriched_strava_chunks) %>% st_as_sf()
  ),
  
  # 6. Partition data for modeling
  # Takes ~1.25 hrs
  tar_target(
    network_topology, 
    prep_network_topology(enriched_bike_network)
  ),
  
  # Snapping (Fast iteration)
  tar_target(
    partitioned_data, 
    snap_counts_to_network(ground_truth_counts, network_topology)
  ),
  
  # ============================================================================
  # SECTION 5: MODELING & VALIDATION (NEW)
  # ============================================================================
  
  # 1. Extract Training Data (replace all NAs with 0)
  tar_target(
    bike_train, 
    partitioned_data$links_train %>% 
      mutate(across(where(is.numeric), ~tidyr::replace_na(., 0)))
  ),
  tar_target(
    ped_train,  
    partitioned_data$nodes_train %>% 
      mutate(across(where(is.numeric), ~tidyr::replace_na(., 0)))
  ),
  
  # 2. Validation (10-Fold Spatial CV)
  # 'crew' will run val_bike and val_ped in parallel on different workers.
  
  # Bike Random Forest (Ranger)
  tar_target(
    val_bike_rf,
    validate_model_10fold(bike_train, mode = "bike", model_type = "ranger")
  ),
  
  # Bike Poisson Boosting (GBM)
  tar_target(
    val_bike_gbm,
    validate_model_10fold(bike_train, mode = "bike", model_type = "gbm")
  ),
  
  # Ped Random Forest (Ranger)
  tar_target(
    val_ped_rf,
    validate_model_10fold(ped_train, mode = "ped", model_type = "ranger")
  ),
  
  # Ped Poisson Boosting (GBM)
  tar_target(
    val_ped_gbm,
    validate_model_10fold(ped_train, mode = "ped", model_type = "gbm")
  ),
  # Combines "Pooled" metrics from all 4 models into one master table
  tar_target(
    model_comparison_results,
    bind_rows(
      val_bike_rf$metrics_pooled  %>% mutate(mode = "bike", model_type = "ranger"),
      val_bike_gbm$metrics_pooled %>% mutate(mode = "bike", model_type = "gbm"),
      val_ped_rf$metrics_pooled   %>% mutate(mode = "ped",  model_type = "ranger"),
      val_ped_gbm$metrics_pooled  %>% mutate(mode = "ped",  model_type = "gbm")
    )
  ),
  # Ranger RF model performs best

  
  # 3. Train Final Models (Full Data)
  # 'crew' will run these in parallel. 
  # Train final Ranger models on ALL data
  tar_target(model_bike_final, train_model(bike_train, mode = "bike", model_type = "ranger")),
  tar_target(model_ped_final, train_model(ped_train,  mode = "ped",  model_type = "ranger")),
  
  # ============================================================================
  # SECTION 6: PREDICTIONS
  # ============================================================================
  
  # 1. Unpack the "Empty" Networks for Prediction, need to add year for prediction
  # 
  tar_target(network_links, 
             partitioned_data$links_predict %>% 
               mutate(year = 2023)
  ),
  tar_target(network_nodes, partitioned_data$nodes_predict %>% 
               mutate(year = 2023)
  ),
  
  # 2. Generate Predictions (Split Method)
  # Maps bike volumes to Links and ped volumes to Nodes
  tar_target(
    final_predictions,
    predict_split_networks(
      bike_model = model_bike_final, 
      ped_model  = model_ped_final,
      link_net   = network_links,   
      node_net   = network_nodes    
    )
  ),
  
  # ============================================================================
  # SECTION 7: WEBTOOL specific data prep and models
  # ============================================================================
  
  # 1. Group Census Blocks for Parallel Processing
  # Split 400k blocks into 32 chunks
  tar_group_count(
    census_chunks,
    census_blocks,
    count = 32
  ),
  
  # 2. Enrich Chunks in Parallel
  tar_target(
    web_blocks_raw_chunks,
    enrich_census_chunk(
      census_chunk = census_chunks,
      sld_sf       = sld_data,         
      wi_sf        = wi_data,        
      weather_sf   = weather_data      
    ),
    pattern = map(census_chunks),
    iteration = "list"
  ),
  
  # 3. Combine & Finalize
  tar_target(
    web_context_map,
    prepare_web_blocks(
      enriched_chunks = do.call(rbind,web_blocks_raw_chunks),
      min_sqm_threshold = 1000
    )
  ),
  
  # 4. Validation: Bike Model ---
  tar_target(
    hglm_cv_bike,
    validate_hglm_kfold(
      data = bike_train,
      mode_arg = "bike",
      target_col = "aadb",
      id_col = "spatial_id",
      k = 10
    )
  ),
  
  # 5. Validation: Pedestrian Model ---
  tar_target(
    hglm_cv_ped,
    validate_hglm_kfold(
      data = ped_train,
      mode_arg = "ped",
      target_col = "aadp",
      id_col = "spatial_id",
      k = 10
    )
  ),
  
  # 6. Final Training: Bike Model ---
  tar_target(
    hglm_model_bike,
    train_hglm(
      train_data = bike_train,
      mode_arg = "bike",
      target_col = "aadb",
      id_col = "spatial_id"
    )
  ),
  
  # 7. Final Training: Pedestrian Model ---
  tar_target(
    hglm_model_ped,
    train_hglm(
      train_data = ped_train,
      mode_arg = "ped",
      target_col = "aadp",
      id_col = "spatial_id"
    )
  ),
  
  # ============================================================================
  # SECTION 8: NETWORK FINALIZATION
  # ============================================================================
  
  # 8.1 Map Volumes Across Network
  # Cross-pollinates bike volumes to nodes, and ped volumes to links using your network_utils function
  tar_target(
    web_ready_network,
    map_volumes_across_network(
      links = final_predictions$links,
      nodes = final_predictions$nodes
    )
  ),
  
  # 8.2 Finalize Web Network Attributes
  # Calculates exposure classes, maps functional classifications, calculates lengths, and renames fields
  tar_target(
    final_web_network,
    finalize_web_network(
      links = web_ready_network$links, 
      nodes = web_ready_network$nodes
    )
  ),
  
  # ============================================================================
  # SECTION 9: SYSTEMIC SAFETY MODELS (Epidemiological Framework)
  # ============================================================================
  
  # Step A1: GIS Phase - Snap Crashes to Network STRICTLY 1-to-1
  tar_target(prepped_bike_nodes, snap_crashes_to_network(final_web_network$nodes, processed_crash_proj, "Bike", is_node = TRUE)),
  tar_target(prepped_bike_links, snap_crashes_to_network(final_web_network$links, processed_crash_proj, "Bike", is_node = FALSE)),
  tar_target(prepped_ped_nodes,  snap_crashes_to_network(final_web_network$nodes, processed_crash_proj, "Walk", is_node = TRUE)),
  tar_target(prepped_ped_links,  snap_crashes_to_network(final_web_network$links, processed_crash_proj, "Walk", is_node = FALSE)),
  
  # Step A2: Math Phase - Train the 4 distinct Models
  tar_target(bike_node_models, fit_safety_models(prepped_bike_nodes)),
  tar_target(bike_link_models, fit_safety_models(prepped_bike_links)),
  tar_target(ped_node_models,  fit_safety_models(prepped_ped_nodes)),
  tar_target(ped_link_models,  fit_safety_models(prepped_ped_links)),
  
  # Step B: Score the network (Calculate Epidemiological Rates of crash, injury, and death risk)
  tar_target(systemic_risk_bike_nodes, calculate_systemic_risk(final_web_network$nodes, bike_node_models)),
  tar_target(systemic_risk_bike_links, calculate_systemic_risk(final_web_network$links, bike_link_models)),
  tar_target(systemic_risk_ped_nodes,  calculate_systemic_risk(final_web_network$nodes, ped_node_models)),
  tar_target(systemic_risk_ped_links,  calculate_systemic_risk(final_web_network$links, ped_link_models)),
  
  # ============================================================================
  # SECTION 10: WEBTOOL OUTPUT EXPORTS
  # ============================================================================
  
  # 10.1 Extract Marginalized Equations (Fixed Effects Only)
  tar_target(
    web_model_assets_file,
    export_web_model_assets(
      hglm_ped = hglm_model_ped, 
      hglm_bike = hglm_model_bike, 
      output_path = data_io("data_processed/model_coefficients.csv")
    ),
    format = "file"
  ),
  
  # 10.2 Prepare Web Context Blocks 
  tar_target(
    web_blocks_export_file,
    prepare_and_export_web_blocks(
      data = web_blocks_raw_chunks,
      output_path = data_io("data_processed/context_blocks.geojson")
    ),
    format = "file"
  ),
  
  # 10.3 Export Epidemiological Safety Models (CSVs)
  # Notice: we now wrap the export paths in data_io() to match your Box directory setup
  tar_target(
    export_systemic_risk_csvs,
    {
      path_links <- data_io("data_processed/systemic_risk_links.csv")
      path_nodes <- data_io("data_processed/systemic_risk_nodes.csv")
      
      write_csv(bind_rows(systemic_risk_bike_links, systemic_risk_ped_links), path_links)
      write_csv(bind_rows(systemic_risk_bike_nodes, systemic_risk_ped_nodes), path_nodes)
      
      c(path_links, path_nodes)
    },
    format = "file"
  ),
  
  # 10.4 Export Final Network Layers (GeoJSON) - Links
  tar_target(
    export_links_geojson,
    {
      path <- data_io("data_processed/links.geojson")
      st_write(
        final_web_network$links, 
        path, 
        driver = "GeoJSON",
        delete_dsn = TRUE,
        quiet = TRUE
      )
      path
    },
    format = "file"
  ),
  
  # 10.5 Export Final Network Layers (GeoJSON) - Nodes
  tar_target(
    export_nodes_geojson,
    {
      path <- data_io("data_processed/nodes.geojson")
      st_write(
        final_web_network$nodes, 
        path, 
        driver = "GeoJSON", 
        delete_dsn = TRUE,
        quiet = TRUE
      )
      path
    },
    format = "file"
  )
)