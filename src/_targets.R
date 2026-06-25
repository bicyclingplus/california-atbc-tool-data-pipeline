# Data pipeline with R targets package on R 4.2.3
# requires...
library(targets)
library(tarchetypes)
library(furrr)
library(crew)

# Hard coding functions source becuase I'm sourcing from a local git repo while
# the pipeline runs with the working directory set to the cloud drive (Box) 
# Expected working dir to be cloud drive project root so that data paths 
# ("data_raw/...") and the _targets store live on the cloud drive.
tar_source("C:/Users/Dillon/projects/california-atbc-tool-data-pipeline/src/functions")

tar_option_set(
  packages = c("tidyverse", "sf", "lubridate", "osmextract", "nngeo", "zip",
               "readr", "purrr", "furrr", "httr", "jsonlite",
               "lightgbm", "terra", "prism", "rsample", "digest"),
  # 4 crew workers for the parallel (mapped) enrichment targets.
  controller = crew_controller_local(workers = 4),
  # transient: free each target's data once its dependents finish so the
  # memory-heavy main-process targets (topology, snap) don't accumulate every
  # upstream object in RAM (the old "persistent" setting paged at ~50 GB). If a
  # target turns out to need its deps kept resident, override per-target with
  # tar_target(..., memory = "persistent").
  memory = "transient",
  garbage_collection = TRUE
)


list(
  # ============================================================================
  # SECTION 1: NETWORK & INFRASTRUCTURE
  # ============================================================================
  tar_target(
    strava_zip_files, 
    list.files("data_raw/strava_caltrans_district_2023", 
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
  # smart location data and national walk index data 
  tar_target(sld_shp_file, "data_raw/SLD/SmartLocationDb.shp", format = "file"),
  tar_target(wi_shp_file, "data_raw/SLD/Natl_WI.shp", format = "file"),
  
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
  
  # PRISM 4km annual climate (ppt / tmin / tmax), auto-downloaded + cached.
  # Tracked as a file target (the three .bil paths).
  tar_target(
    prism_paths,
    get_prism_climate(2023),
    format = "file"
  ),
  
  # TIMS processed switrs data
  tar_target(switrs_file, "data_raw/switrs_2019_2023/CRASH_PED_BIKE_2019-2023_20240912.csv", format = "file"),
  tar_target(processed_crash, process_switrs_data(switrs_file)),
  
  tar_target(
    census_blocks,
    st_read("data_raw/census/blocks_optimized.gpkg", quiet = TRUE) %>% 
      st_transform(3310)
  ),
  # ============================================================================
  # SECTION 3: COUNT DATA (Ground Truth)
  # ============================================================================
  # files from UCB SafeTREC
  tar_target(ucb_bike_file, "data_raw/ucb_bike_AADB/Model_clean_data_july23_AADBT.csv", format = "file"),
  tar_target(ucb_ped_file, "data_raw/ucb_ped_AADP/PSIP_allvars_20200503_wSHS_newFC.csv", format = "file"),
  
  tar_target(ucb_bike_clean, load_ucb_bike(ucb_bike_file)),
  tar_target(ucb_ped_clean, load_ucb_ped(ucb_ped_file)),
  
  # files from https://data.ca.gov/dataset/at-count-dataset
  tar_target(caltrans_bike_files, list.files("data_raw/atd", pattern = "caltrans_bicycle.*\\.csv", full.names = TRUE)),
  tar_target(caltrans_ped_files, list.files("data_raw/atd", pattern = "caltrans_pedestrian.*\\.csv", full.names = TRUE)),
  
  tar_target(caltrans_bike_clean, process_caltrans_counts(caltrans_bike_files, mode = "bike")),
  tar_target(caltrans_ped_clean, process_caltrans_counts(caltrans_ped_files, mode = "ped")),

  # CAT Data Portal counts (catdataportal.berkeley.edu) -- ~3,400 mostly-new sites
  # incl. Trail / Mid-block facilities the UCB data lacks. Hourly counts expanded
  # via the internal-seasonality method; de-duplicated against UCB sites.
  tar_target(
    catportal_files,
    list.files("data_raw/catdp", pattern = "\\.csv\\.gz$|counts_zip_metadata\\.csv$", full.names = TRUE),
    format = "file"
  ),
  tar_target(
    catportal_clean,
    load_catportal_counts(
      catdp_dir = "data_raw/catdp",
      ucb_sites = bind_rows(
        ucb_bike_clean %>% select(lat, lon),
        ucb_ped_clean  %>% select(lat, lon)
      ),
      dedup_dist_m = 30
    )
  ),
  tar_target(catportal_bike_clean, catportal_clean %>% filter(mode == "bike")),
  tar_target(catportal_ped_clean,  catportal_clean %>% filter(mode == "ped")),

  tar_target(all_bike_points, bind_rows(ucb_bike_clean, caltrans_bike_clean, catportal_bike_clean)),
  tar_target(all_ped_points, bind_rows(ucb_ped_clean, caltrans_ped_clean, catportal_ped_clean)),
  
  tar_target(ground_truth_counts, bind_rows(all_bike_points, all_ped_points)),
  
  # ============================================================================
  # SECTION 4: DATA ENRICHMENT
  # ============================================================================

  # Split the Strava target into 8 chunks for the parallel (mapped) enrichment.
  tar_group_count(
    strava_chunks,
    master_network,
    count = 8
  ),

  # Base Joins (OSM, SLD, WI, Weather - all together)
  tar_target(
    strava_base,
    enrich_base_network(strava_chunks, osm_processed, sld_data, wi_data, prism_paths),
    pattern = map(strava_chunks),
    iteration = "list"
  ),

  # Crashes
  tar_target(processed_crash_proj, st_transform(processed_crash, 3310)),

  tar_target(
    strava_with_crashes,
    enrich_crashes(strava_base, processed_crash_proj),
    pattern = map(strava_base),
    iteration = "list"
  ),

  # Census joins
  tar_target(
    strava_with_census,
    enrich_census(strava_with_crashes, census_blocks),
    pattern = map(strava_with_crashes),
    iteration = "list"
  ),

  # Final Model Feature Calculations
  tar_target(
    enriched_strava_chunks,
    calculate_model_features(strava_with_census),
    pattern = map(strava_with_census),
    iteration = "list"
  ),

  # Re-combine chunks into Master
  tar_target(
    enriched_bike_network,
    bind_rows(enriched_strava_chunks) %>% st_as_sf()
  ),

  # Build network topology (links from/to + node geometry/attributes) over the
  # full network. deployment="main"; memory="transient" (global) keeps upstream
  # objects from accumulating here.
  tar_target(
    network_topology,
    prep_network_topology(enriched_bike_network),
    deployment = "main"
  ),

  # Snap ground-truth counts onto the network (bike -> links via axis assignment,
  # ped -> nearest node). deployment="main".
  tar_target(
    partitioned_data,
    snap_counts_to_network(ground_truth_counts, network_topology),
    deployment = "main"
  ),
  
  # Ambient Strava demand field (raster focal-sum), added later -----------
  # Attempted vector operatoins that were far too computationally intensive, so 
  # shifted to raster with output stored as a .tif file target. All
  # ambient extractions below are fast lookups against it.
  tar_target(
    strava_grid,
    build_strava_grid(master_network, "data_processed/strava_grid.tif"),
    format = "file",
    deployment = "main"
  ),

  # ============================================================================
  # SECTION 5: MODELING (LightGBM Tweedie)
  # ============================================================================
  # --- Training data: snapped counts + ambient features ----------------------
  tar_target(
    bike_train,
    partitioned_data$links_train %>%
      mutate(across(where(is.numeric), ~tidyr::replace_na(., 0))) %>%
      bind_cols(extract_ambient(strava_grid, .))
  ),
  tar_target(
    ped_train,
    partitioned_data$nodes_train %>%
      mutate(across(where(is.numeric), ~tidyr::replace_na(., 0))) %>%
      bind_cols(extract_ambient(strava_grid, .))
  ),

  # --- Validation (spatial 10-fold; reports class accuracy + absolute error) -
  # Track A = existing network (with on-link Strava); Track B = new off-street
  # paths (Strava-free, for the web tool). crew runs these in parallel.
  tar_target(val_bike_A, validate_lgb(bike_train, PREDICTORS_A, "aadb")),
  tar_target(val_bike_B, validate_lgb(bike_train, PREDICTORS_B, "aadb")),
  tar_target(val_ped_A,  validate_lgb(ped_train,  PREDICTORS_A, "aadp")),
  tar_target(val_ped_B,  validate_lgb(ped_train,  PREDICTORS_B, "aadp")),

  # --- Final models trained on ALL data --------------------------------------
  tar_target(model_bike_A, train_lgb(bike_train, PREDICTORS_A, "aadb")),  # existing network
  tar_target(model_ped_A,  train_lgb(ped_train,  PREDICTORS_A, "aadp")),
  tar_target(model_bike_B, train_lgb(bike_train, PREDICTORS_B, "aadb")),  # new paths (web tool)
  tar_target(model_ped_B,  train_lgb(ped_train,  PREDICTORS_B, "aadp")),
  
  # ============================================================================
  # SECTION 6: PREDICTIONS
  # ============================================================================
  
  # Prediction networks + ambient features (fast lookup against strava_grid).
  tar_target(network_links,
             partitioned_data$links_predict %>% bind_cols(extract_ambient(strava_grid, .)),
             deployment = "main"
  ),
  tar_target(network_nodes,
             partitioned_data$nodes_predict %>% bind_cols(extract_ambient(strava_grid, .)),
             deployment = "main"
  ),

  # Generate Predictions (Track A models: on-link Strava available on the
  # existing network). Maps bike volumes to Links and ped volumes to Nodes.
  tar_target(
    final_predictions,
    predict_split_networks(
      bike_model = model_bike_A,
      ped_model  = model_ped_A,
      link_net   = network_links,
      node_net   = network_nodes
    )
  ),
  
  # ============================================================================
  # SECTION 7: WEBTOOL specific data prep and models
  # ============================================================================
  
  # Group Census Blocks for Parallel Processing
  # Split 400k blocks into 32 chunks
  tar_group_count(
    census_chunks,
    census_blocks,
    count = 32
  ),
  
  # Enrich Chunks in Parallel
  tar_target(
    web_blocks_raw_chunks,
    enrich_census_chunk(
      census_chunk = census_chunks,
      sld_sf       = sld_data,         
      wi_sf        = wi_data,          
      crash_sf     = processed_crash,
      prism_paths  = prism_paths
    ),
    pattern = map(census_chunks),
    iteration = "list"
  ),
  
  # Combine & Finalize
  tar_target(
    web_context_map,
    prepare_web_blocks(
      enriched_chunks = do.call(rbind,web_blocks_raw_chunks),
      min_sqm_threshold = 1000
    )
  ),

  # ============================================================================
  # SECTION 8: WEBTOOL OUTPUTS
  # ============================================================================

  # Export Track B models for the Node.js web tool (new-path prediction).
  # LightGBM -> text and JSON and a JSON feature-spec. post-pipeline
  # python stripc (convert_to_onnx.py) needed to convert to ONNX (onnxruntime-node)
  tar_target(
    web_model_assets_file,
    export_models_for_node(
      bike_model = model_bike_B,
      ped_model  = model_ped_B,
      out_dir    = "data_processed/web_models"
    ),
    format = "file"
  ),
  
  # Prepare Web Context Blocks 
  # Only keeping the geographic predictors. 
  # Note: Year and Network vars are handled by the web UI, not the spatial join.
  tar_target(
    web_blocks_export_file,
    prepare_and_export_web_blocks(
      data = web_blocks_raw_chunks,
      output_path = "data_processed/context_blocks.geojson"
    ),
    format = "file"
  ),
  
  # Map Volumes Across Network
  # Spreads bike volumes to nodes, and ped volumes to links
  tar_target(
    web_ready_network,
    map_volumes_across_network(
      links = final_predictions$links,
      nodes = final_predictions$nodes
    )
  ),

  # Finalize Web Network Attributes
  # Calculates exposure classes, maps functional classifications, calculates lengths, and renames fields
  tar_target(
    final_web_network,
    finalize_web_network(
      links = web_ready_network$links,
      nodes = web_ready_network$nodes
    )
  ),
  
  # Generates Localized Safety Appendix A (CSVs)
  # Uses the finalized assets so that `exposure_class` and `functional` are present for the aggregation
  tar_target(
    appendix_a_csvs,
    generate_localized_appendix_a(
      links = final_web_network$links, 
      nodes = final_web_network$nodes, 
      processed_crash_proj = processed_crash_proj,
      output_path_links = "data_processed/appendix_a_links.csv",
      output_path_nodes = "data_processed/appendix_a_nodes.csv"
    ),
    format = "file"
  ),
  
  # Export Final Network Layers (GeoJSON)
  tar_target(
    export_links_geojson,
    {
      path <- "data_processed/links.geojson"
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
  
  # Save Intersections (Nodes) to geojson
  tar_target(
    export_nodes_geojson,
    {
      path <- "data_processed/nodes.geojson"
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