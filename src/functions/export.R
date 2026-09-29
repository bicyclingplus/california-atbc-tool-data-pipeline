# Export blocks for spatial join on web tool
#
# Also computes the Track B (link-level Strava-free) new-off-street-path volumes per
# block so the web tool reads. This includes the simplifying assumption that the four
# facility predictors (infra_type, functional, is_paved, speed_limit) are constant --
# so a block's off-street path prediction is a function of location alone.
prepare_and_export_web_blocks <- function(data, strava_grid, bike_model, ped_model, output_path) {
  require(sf)
  require(dplyr)

  # collapse list of sf objects into one
  if (inherits(data, "list")) {
    data <- bind_rows(data)
  }

  # Ambient Strava at each block (centroid lookup against the strava_grid .tif).
  # The Track B model needs amb_strava_<ring>m and they are not otherwise in the
  # block attributes; adding them here lets the web tool's spatial join supply
  # every location-derived predictor in one lookup. Already log1p-scaled.
  data <- bind_cols(data, extract_ambient(strava_grid, data))

  # --- Precompute Track B new-path volumes ----------------------------------
  # Fix the four facility predictors to the new-off-street-path constants
  facility_fixed <- mutate(
    data,
    infra_type  = "separated_path",
    functional  = "Local Road",
    is_paved    = 1,
    speed_limit = 15
  )
  data$pred_bike_vol_newpath <- predict_lgb(bike_model, facility_fixed)
  data$pred_ped_vol_newpath  <- predict_lgb(ped_model,  facility_fixed)

  # Spatial Cleaning & Projection
  web_blocks <- data %>%
    st_make_valid() %>%
    st_transform(4326) %>% # lon/lat (EPSG:4326) -- matches links/nodes and RFC 7946
    # Select the location-derived model predictors (context + ambient) plus the
    # precomputed new-path volumes. The web tool spatially joins a drawn feature
    # to a block and reads pred_bike_vol_newpath (a drawn path/link) and/or
    # pred_ped_vol_newpath (its intersections) -- no live model needed.
    select(
      any_of(c(
        "emp_density", "int_density", "walk_index", "housing_total",
        "pop_low", "pop_high", "emp_low", "emp_high",
        "schools_low", "schools_high", "colleges_low", "colleges_high",
        "doctors_low", "doctors_high", "pharmacies_low", "pharmacies_high",
        "retail_low", "retail_high", "supermarket_low", "supermarket_high",
        "parks_low", "parks_high", "trails_low", "trails_high",
        "community_low", "community_high", "transit_low", "transit_high",
        "precip_annual", "temp_min", "temp_max",
        "amb_strava_250m", "amb_strava_500m", "amb_strava_1000m", "amb_strava_2000m",
        "pred_bike_vol_newpath", "pred_ped_vol_newpath"
      ))
    )

  # Export 
  if(!dir.exists(dirname(output_path))) dir.create(dirname(output_path), recursive = TRUE)
  
  st_write(
    web_blocks, 
    output_path, 
    delete_dsn = TRUE, 
    quiet = TRUE
  )
  
  return(output_path)
}

# Generate Systemic Risk Tables (Appendix A)
generate_localized_appendix_a <- function(links, nodes, processed_crash_proj, 
                                          output_path_links = "data_processed/appendix_a_links.csv",
                                          output_path_nodes = "data_processed/appendix_a_nodes.csv") {
  require(dplyr)
  require(sf)
  require(readr)
  require(tidyr)
  
  p <- 0.4 # Power parameter from Elvik and Goel (2019)
  
  # --- Determine the year span for normalization ---
  year_span <- length(unique(processed_crash_proj$ACCIDENT_YEAR))
  if (year_span == 0) year_span <- 5 # Fallback if missing
  
  # --- PREVENT DUPLICATES: Strict 1-to-1 Snapping ---
  crashes_int <- processed_crash_proj %>% filter(INTERSECTION == "Y")
  crashes_seg <- processed_crash_proj %>% filter(INTERSECTION != "Y" | is.na(INTERSECTION))
  
  node_idx <- st_nearest_feature(crashes_int, nodes)
  crashes_int$node_id <- nodes$node_id[node_idx]
  
  link_idx <- st_nearest_feature(crashes_seg, links)
  crashes_seg$edge_uid <- links$edge_uid[link_idx]
  
  # --- AGGREGATION ---
  calculate_alphas <- function(network_df, crash_df, mode_label, loc_type, is_node) {
    
    # Safely define dynamic column names upfront
    is_bike <- mode_label == "Bike"
    exp_col <- if (is_bike) "bicycle_exposure_class" else "pedestrian_exposure_class"
    vol_col <- if (is_bike) "pred_bike_vol" else "pred_ped_vol"
    join_col <- if (is_node) "node_id" else "edge_uid"
    
    mode_crashes <- crash_df %>% 
      filter(if (is_bike) BICYCLE_ACCIDENT == "Y" else PEDESTRIAN_ACCIDENT == "Y")
    
    crash_summary <- mode_crashes %>%
      st_drop_geometry() %>%
      group_by(!!sym(join_col)) %>%
      summarise(
        total_crashes = n(),
        total_injuries = sum(NUMBER_INJURED, na.rm = TRUE),
        total_deaths = sum(NUMBER_KILLED, na.rm = TRUE),
        .groups = "drop"
      )
    
    network_df %>%
      st_drop_geometry() %>%
      left_join(crash_summary, by = join_col) %>%
      mutate(across(starts_with("total_"), ~replace_na(.x, 0))) %>%
      # Safely inject the defined column names
      group_by(
        exposure_class = .data[[exp_col]], 
        functional
      ) %>%
      summarise(
        prevalence = if(is_node) n() else sum(length_ft / 5280, na.rm = TRUE),
        avg_vol = mean(.data[[vol_col]], na.rm = TRUE),
        crashes_py = sum(total_crashes) / year_span,
        injuries_py = sum(total_injuries) / year_span,
        deaths_py = sum(total_deaths) / year_span,
        .groups = "drop"
      ) %>%
      mutate(
        location = loc_type,
        mode = mode_label,
        rate_c = crashes_py / prevalence,
        rate_i = injuries_py / prevalence,
        rate_d = deaths_py / prevalence,
        alpha_crash = log(rate_c / (avg_vol^p)),
        alpha_injury = log(rate_i / (avg_vol^p)),
        alpha_death = log(rate_d / (avg_vol^p))
      ) %>%
      mutate(across(starts_with("alpha_"), ~ifelse(is.infinite(.), -15, .)))
  }
  
  # --- COMPILE FINAL TABLES ---
  appendix_a_links <- bind_rows(
    calculate_alphas(links, crashes_seg, "Bike", "Roadway", FALSE),
    calculate_alphas(links, crashes_seg, "Walk", "Roadway", FALSE)
  ) %>%
    select(
      `Location` = location, `Mode` = mode, `Exposure Class` = exposure_class,
      `Functional Class` = functional, `Prevalence (miles)` = prevalence,
      `Average Daily Volume (bike/ped)` = avg_vol, `Crashes/mile/year` = rate_c,
      `Injuries/mile/year` = rate_i, `Deaths/mile/year` = rate_d,
      `α Crash` = alpha_crash, `α Injury` = alpha_injury, `α Death` = alpha_death
    )
  
  appendix_a_nodes <- bind_rows(
    calculate_alphas(nodes, crashes_int, "Bike", "Intersection", TRUE),
    calculate_alphas(nodes, crashes_int, "Walk", "Intersection", TRUE)
  ) %>%
    select(
      `Location` = location, `Mode` = mode, `Exposure Class` = exposure_class,
      `Functional Class` = functional, `Prevalence (count)` = prevalence,
      `Average Daily Volume (bike/ped)` = avg_vol, `Crashes/intersection/year` = rate_c,
      `Injuries/intersection/year` = rate_i, `Deaths/intersection/year` = rate_d,
      `α Crash` = alpha_crash, `α Injury` = alpha_injury, `α Death` = alpha_death
    )
  
  # Added back the directory check!
  if(!dir.exists(dirname(output_path_links))) dir.create(dirname(output_path_links), recursive = TRUE)
  
  write_csv(appendix_a_links, output_path_links)
  write_csv(appendix_a_nodes, output_path_nodes)
  
  return(c(output_path_links, output_path_nodes))
}
