
# Export GLM coefficient table
export_web_model_assets <- function(hglm_ped, hglm_bike, output_path) {
  require(dplyr)
  require(readr)
  require(lme4)
  
  # 1. Internal Helper to Extract Fixed Effects
  get_fixed_effects <- function(model_obj, mode_name) {
    fe <- lme4::fixef(model_obj)
    tibble(
      mode = mode_name,
      coef = names(fe),
      estimate = as.numeric(fe)
    )
  }
  
  # 2. Combine and Transform
  coeffs <- bind_rows(
    get_fixed_effects(hglm_ped, "ped"),
    get_fixed_effects(hglm_ped, "bike")
  ) %>%
    mutate(
      # Define UI Data Dictionary
      possible_values = case_when(
        coef == "(Intercept)" ~ "base_constant",
        coef == "year" ~ "numeric: 2020-2026",
        coef == "is_paved" ~ "binary: 0, 1",
        coef == "speed_limit" ~ "numeric: 5-65",
        grepl("infra_type", coef) ~ "binary_flag: 0, 1",
        coef %in% c("emp_density", "int_density", "walk_index", "housing_total") ~ "numeric: density_value",
        coef == "precip_annual" ~ "numeric: annual_inches",
        grepl("low$|high$", coef) ~ "numeric: buffer_count",
        TRUE ~ "numeric"
      ),
      # Define UI Interaction Defaults
      web_default = case_when(
        coef == "year" ~ "2024",
        coef == "is_paved" ~ "1",
        coef == "speed_limit" ~ "15",
        coef == "infra_typeseparated_path" ~ "1",
        grepl("infra_type", coef) ~ "0",
        coef == "(Intercept)" ~ "1",
        TRUE ~ "extract_from_spatial_join"
      )
    )
  
  # 3. Export and return path
  if(!dir.exists(dirname(output_path))) dir.create(dirname(output_path), recursive = TRUE)
  write_csv(coeffs, output_path)
  
  return(output_path)
}

# Export blocks for spatial join on web tool
prepare_and_export_web_blocks <- function(data, output_path) {
  require(sf)
  require(dplyr)
  
  # collapse list of sf objects into one
  if (inherits(data, "list")) {
    data <- bind_rows(data)
  }
  
  # 1. Spatial Cleaning & Projection
  web_blocks <- data %>%
    st_make_valid() %>%
    st_transform(3857) %>% # Web Mercator as requested
    # 2. Select only the necessary GLM predictors
    select(
      any_of(c(
        "emp_density", "int_density", "walk_index", "housing_total",
        "pop_low", "pop_high", "emp_low", "emp_high",
        "schools_low", "schools_high", "colleges_low", "colleges_high",
        "doctors_low", "doctors_high", "pharmacies_low", "pharmacies_high",
        "retail_low", "retail_high", "supermarket_low", "supermarket_high",
        "parks_low", "parks_high", "trails_low", "trails_high",
        "community_low", "community_high", "transit_low", "transit_high",
        "precip_annual"
      ))
    )
  
  # 3. Export with overwrite safety
  if(!dir.exists(dirname(output_path))) dir.create(dirname(output_path), recursive = TRUE)
  
  st_write(
    web_blocks, 
    output_path, 
    delete_dsn = TRUE, 
    quiet = TRUE
  )
  
  return(output_path)
}

# Appendix A
# Calculate Systemic Risk (Normalized per year and per mile/intersection)
calculate_systemic_risk <- function(network_sf, models_list, years_of_data = 5) {
  require(dplyr)
  require(sf)
  require(MASS)
  
  mod_hurdle <- models_list$model_hurdle
  mod_count  <- models_list$model_count
  mod_sev    <- models_list$model_severity
  mode_name  <- models_list$mode
  is_node    <- models_list$is_node
  
  message(paste("Scoring Epidemiological Rates for:", mode_name, ifelse(is_node, "(Nodes)", "(Links)")))
  
  # 1. Prepare Network
  vol_col <- ifelse(mode_name == "Bike", "pred_bike_vol", "pred_ped_vol")
  
  df_pred <- network_sf %>%
    mutate(
      vol_safe = ifelse(.data[[vol_col]] <= 0, 0.1, as.numeric(.data[[vol_col]])),
      # Calculate length in miles (nodes are treated as 1 unit)
      len_miles = if (is_node) 1 else (as.numeric(length_ft) / 5280),
      len_miles = ifelse(len_miles <= 0, 0.001, len_miles)
    )
  
  # 2. Predict Raw Frequency (Total Expected Crashes over 5 years on this exact geometry)
  p_crash <- predict(mod_hurdle, newdata = df_pred, type = "response")
  exp_count_given_crash <- predict(mod_count, newdata = df_pred, type = "response")
  
  df_pred$raw_expected_crashes <- p_crash * exp_count_given_crash
  
  # 3. Predict Severity Probabilities
  sev_probs <- predict(mod_sev, newdata = df_pred, type = "probs")
  
  prob_minor  <- sev_probs[, "Minor"]
  prob_severe <- sev_probs[, "Severe"]
  prob_fatal  <- sev_probs[, "Fatal"]
  
  # Injuries = Minor + Severe
  df_pred$prob_injury <- prob_minor + prob_severe
  df_pred$prob_fatal  <- prob_fatal
  
  # 4. Calculate Base Normalized Rates
  df_rates <- df_pred %>%
    mutate(
      mode = mode_name,
      location_type = ifelse(is_node, "Intersection", "Roadway"),
      
      # Step A: Normalize to Annual Base
      annual_crashes = raw_expected_crashes / years_of_data,
      
      # Step B: Normalize to Spatial Unit 
      base_rate_crashes = annual_crashes / len_miles,
      
      # Step C: Apply Severity Probabilities
      base_rate_injuries   = base_rate_crashes * prob_injury,
      base_rate_fatalities = base_rate_crashes * prob_fatal
    ) %>%
    st_drop_geometry()
  
  # 5. Apply Explicit Column Names Based on Location Type
  if (is_node) {
    df_final <- df_rates %>%
      dplyr::select(
        edge_uid = dplyr::any_of(c("edge_uid", "node_id")), 
        mode,
        location_type,
        functional,
        crashes_per_intersection_year = base_rate_crashes,
        injuries_per_intersection_year = base_rate_injuries,
        fatalities_per_intersection_year = base_rate_fatalities
      )
  } else {
    df_final <- df_rates %>%
      dplyr::select(
        edge_uid = dplyr::any_of(c("edge_uid", "node_id")), 
        mode,
        location_type,
        functional,
        crashes_per_mile_year = base_rate_crashes,
        injuries_per_mile_year = base_rate_injuries,
        fatalities_per_mile_year = base_rate_fatalities
      )
  }
  
  return(df_final)
}
