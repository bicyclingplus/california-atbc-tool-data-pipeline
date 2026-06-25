
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
    get_fixed_effects(hglm_ped,  "ped"),
    get_fixed_effects(hglm_bike, "bike")
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
        coef == "precip_annual" ~ "numeric: annual_mm",
        coef %in% c("temp_min", "temp_max") ~ "numeric: annual_degC",
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
        "precip_annual", "temp_min", "temp_max"
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
# ============================================================================
# Export Track B (Strava-free) models for the Node.js web tool.
# ----------------------------------------------------------------------------
# Writes, per mode, the canonical LightGBM artifacts plus a JSON feature-spec so
# Node can build the input vector identically. ONNX conversion (for
# onnxruntime-node) is a separate one-time step: scripts/convert_to_onnx.py.
#
# Outputs (returned as a character vector for a format="file" target):
#   <out_dir>/<mode>_model.txt      LightGBM text model (canonical; lgb.save)
#   <out_dir>/<mode>_model.json     lgb.dump (tree structure, JS-parseable)
#   <out_dir>/<mode>_feature_spec.json  predictors, one-hot column order, transforms
# ============================================================================
export_models_for_node <- function(bike_model, ped_model, out_dir) {
  require(lightgbm); require(jsonlite)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  write_one <- function(model, mode) {
    txt  <- file.path(out_dir, paste0(mode, "_model.txt"))
    json <- file.path(out_dir, paste0(mode, "_model.json"))
    spec <- file.path(out_dir, paste0(mode, "_feature_spec.json"))

    # model is a stored lgb_model (text + metadata). Reconstitute a live Booster.
    booster <- lgb_booster(model)
    lightgbm::lgb.save(booster, txt)
    writeLines(lightgbm::lgb.dump(booster), json)

    # Feature contract: the exact one-hot column order the model expects, the
    # raw predictors, which are categorical, and the transforms Node must apply.
    train_cols <- model$train_cols
    predictors <- model$predictors
    spec_list <- list(
      mode = mode,
      target = model$target,
      objective = "tweedie",
      note = "LightGBM Tweedie returns predictions on the COUNT scale (already exp-linked). Do NOT exponentiate.",
      raw_predictors = predictors,
      onehot_columns = train_cols,         # exact column order for the model matrix
      transforms = list(
        ambient = "amb_strava_<ring>m features are log1p(sum of network Strava in the annulus). Provided by the spatial join / precomputed grid; Node receives them already log1p-scaled.",
        missing = "numeric NA -> 0",
        categorical = "factor levels one-hot encoded as <var><level> matching onehot_columns"
      )
    )
    write_json(spec_list, spec, auto_unbox = TRUE, pretty = TRUE)
    c(txt, json, spec)
  }

  out <- c(write_one(bike_model, "bike"), write_one(ped_model, "ped"))
  message("...Exported ", length(out), " Node model files to ", out_dir)
  out
}
