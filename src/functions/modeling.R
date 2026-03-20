library(ranger)
library(dplyr)
library(sf)

# 
# --- Metrics Calculator ---
calculate_model_metrics <- function(data, truth_col, pred_col) {
  
  # Extract vectors
  obs  <- data[[truth_col]]
  pred <- data[[pred_col]]
  
  # 1. Standard Metrics
  rmse_val <- rmse_vec(obs, pred)
  rsq_val  <- rsq_vec(obs, pred)
  mae_val  <- mae_vec(obs, pred)
  
  # 2. Relative Metrics
  # Mean Actual (to normalize RMSE)
  mean_obs <- mean(obs, na.rm = TRUE)
  
  # % RMSE
  pct_rmse <- (rmse_val / mean_obs) * 100
  
  # MAPE (Mean Absolute Percentage Error)
  # Protect against division by zero by filtering
  valid_mape_idx <- obs > 0
  mape_val <- mean(abs((obs[valid_mape_idx] - pred[valid_mape_idx]) / obs[valid_mape_idx])) * 100
  
  # Bias / MPE (Mean Percentage Error)
  # Negative = Under-prediction, Positive = Over-prediction
  mpe_val <- mean((pred[valid_mape_idx] - obs[valid_mape_idx]) / obs[valid_mape_idx]) * 100
  
  tibble(
    rmse      = rmse_val,
    rsq       = rsq_val,
    mae       = mae_val,
    pct_rmse  = pct_rmse,  # "We are off by roughly X% of the average volume"
    mape      = mape_val,  # "On average, a specific prediction is off by X%"
    bias_pct  = mpe_val,   # "The model systematically over/under predicts by X%"
    n_obs     = length(obs)
  )
}

# --- Train Model (Supports Ranger and GBM) ---
#' @param train_data The snapped training set
#' @param mode Character: "bike" or "ped"
#' @param model_type Character: "ranger" (Random Forest) or "gbm" (Poisson Boosting)
#' Train Model (Integer Fix Version)
train_model <- function(train_data, mode = "bike", model_type = "ranger") {
  
  require(dplyr)
  require(ranger)
  require(gbm)
  
  target_var <- if(mode == "bike") "aadb" else "aadp"
  
  predictors <- c(
    "strava_vol_total", "WWI", "year",
    "emp_density", "int_density", "walk_index", "housing_total",
    "pop_low", "pop_high", "emp_low", "emp_high",
    "schools_low", "schools_high", "colleges_low", "colleges_high",
    "doctors_low", "doctors_high", "pharmacies_low", "pharmacies_high",
    "retail_low", "retail_high", "supermarket_low", "supermarket_high",
    "parks_low", "parks_high", "trails_low", "trails_high",
    "community_low", "community_high", "transit_low", "transit_high",
    "infra_type", "is_paved", "speed_limit", "crash_count_30m", "precip_annual"
  )
  
  if(mode == "ped"){
    predictors <- predictors[!predictors %in% c("is_paved", "infra_type")]
  } 
  
  # Data Prep
  train_df <- train_data
  if (inherits(train_df, "sf")) train_df <- st_drop_geometry(train_df)
  
  train_df <- train_df %>%
    select(all_of(c(target_var, predictors))) %>%
    na.omit() %>%
    mutate(across(where(is.character), as.factor))
  
  # --- Round to Integer ---
  # Forces data to be valid counts (0, 1, 2...)
  clean_vals <- as.integer(round(pmax(train_df[[target_var]], 0), 0))
  train_df[[target_var]] <- clean_vals
  
  if (model_type == "ranger") {
    train_df$target_ln <- log1p(train_df[[target_var]])
    formula_str <- as.formula(paste("target_ln ~", paste(predictors, collapse = " + ")))
    
    model <- ranger::ranger(
      formula = formula_str,
      data = train_df,
      importance = "permutation",
      num.trees = 500,
      mtry = 5,
      splitrule = "extratrees"
    )
    
  } else if (model_type == "gbm") {
    
    formula_str <- as.formula(paste(target_var, "~ ."))
    
    model <- gbm::gbm(
      formula = formula_str,
      data = train_df,
      distribution = "poisson",
      n.trees = 2000,
      interaction.depth = 5,
      shrinkage = 0.05,
      n.minobsinnode = 10,
      bag.fraction = 0.8,
      verbose = FALSE
    )
  }
  
  model$model_type <- model_type 
  return(model)
}


# --- Validate Model (Auto-detects Model Type) ---
#' @param train_data The snapped training set
#' @param mode Character: "bike" or "ped"
#' @param model_type Character: "ranger" or "gbm"
validate_model_10fold <- function(train_data, mode = "bike", model_type = "ranger") {
  library(rsample)
  library(yardstick)
  library(furrr) 
  library(dplyr)
  
  set.seed(123)
  target_var <- if(mode == "bike") "aadb" else "aadp"
  
  # Prepare Data
  train_df <- train_data %>% st_drop_geometry() %>% filter(!is.na(spatial_id))
  folds <- group_vfold_cv(train_df, group = spatial_id, v = 10)
  
  all_predictions <- furrr::future_map_dfr(folds$splits, function(s) {
    
    # Load libs inside worker
    library(ranger)
    library(gbm)
    
    train_obj <- analysis(s)
    test_obj  <- assessment(s)
    
    # 1. Train
    m <- train_model(train_obj, mode = mode, model_type = model_type)
    
    # 2. Predict (Handle Model Differences)
    if (model_type == "ranger") {
      # Ranger: Predict Log -> Exponentiate
      raw_preds <- predict(m, data = test_obj)$predictions
      final_preds <- pmax(expm1(raw_preds), 0)
      
    } else if (model_type == "gbm") {
      # GBM: Predict Response (Counts) directly
      # Must specify n.trees same as training
      final_preds <- predict(m, newdata = test_obj, n.trees = 2000, type = "response")
    }
    
    # Return Results
    test_obj %>%
      select(spatial_id, all_of(target_var)) %>%
      mutate(
        predicted = final_preds,
        fold = s$id[[1]],
        model_type = model_type
      )
  }, .options = furrr_options(seed = TRUE))
  
  # Metrics
  pooled_metrics <- calculate_model_metrics(all_predictions, target_var, "predicted") %>%
    mutate(type = "Pooled Global")
  
  fold_metrics <- all_predictions %>%
    group_by(fold) %>%
    group_modify(~ calculate_model_metrics(.x, target_var, "predicted")) %>%
    mutate(type = "Fold-wise")
  
  list(
    metrics_pooled = pooled_metrics,
    metrics_folds  = fold_metrics,
    raw_predictions = all_predictions
  )
}


# --- Predict on Network ---
#' Predict Separately: Bikes on Links, Peds on Nodes
#' @param bike_model The trained Ranger model for bikes
#' @param ped_model The trained Ranger model for pedestrians
#' @param link_net SF Object: The road network (Lines)
#' @param node_net SF Object: The intersections/nodes (Points)
predict_split_networks <- function(bike_model, ped_model, link_net, node_net) {
  
  require(dplyr)
  require(ranger)
  require(sf)
  
  # --- PART A: BIKE PREDICTION (ON LINKS) ---
  message("--- Processing Bike Predictions (Links) ---")
  
  # 1. Clean Link Data (Drop geometry for ranger)
  link_df <- link_net %>%
    st_drop_geometry() %>%
    mutate(across(where(is.character), as.factor)) %>%
    # Fill NAs with 0 to prevent crashes
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))
  
  # 2. Predict & Back-Transform (Log -> Count)
  bike_raw <- predict(bike_model, data = link_df)$predictions
  link_net$pred_bike_vol <- pmax(expm1(bike_raw), 0)
  
  
  # --- PART B: PEDESTRIAN PREDICTION (ON NODES) ---
  message("--- Processing Ped Predictions (Nodes) ---")
  
  # 3. Clean Node Data
  node_df <- node_net %>%
    st_drop_geometry() %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))
  
  # 4. Predict & Back-Transform (Log -> Count)
  ped_raw <- predict(ped_model, data = node_df)$predictions
  node_net$pred_ped_vol <- pmax(expm1(ped_raw), 0)
  
  # 5. Return as a named list
  return(list(
    links = link_net, 
    nodes = node_net
  ))
}


# --- Fit a Hierarchical Poisson GLM for on-the-fly webtool predictions ---
#' @param data The training data (sf or dataframe)
#' @param formula_obj The model formula (e.g., crash_count ~ flow + width)
train_hglm <- function(train_data, mode_arg = "bike", 
                       target_col = "aadt", id_col = "spatial_id") {
  
  require(dplyr)
  require(lme4)
  require(sf)
  
  message("...Fitting HGLM for Mode: ", mode_arg)
  
  # 1. Define Predictors Internally
  predictors <- c(
    "year",
    # Network variables 
    "infra_type", "is_paved", "speed_limit",
    # Density & Demographics
    "emp_density", "int_density", "walk_index", "housing_total",
    "pop_low", "pop_high", "emp_low", "emp_high",
    # POIs (Low = 800m, High = 2400m)
    "schools_low", "schools_high", "colleges_low", "colleges_high",
    "doctors_low", "doctors_high", "pharmacies_low", "pharmacies_high",
    "retail_low", "retail_high", "supermarket_low", "supermarket_high",
    "parks_low", "parks_high", "trails_low", "trails_high",
    "community_low", "community_high", "transit_low", "transit_high",
    # Environment
    "precip_annual"
    # Note: road infrastructure vars (speed_limit, etc.) are excluded 
    # as they don't exist on the context blocks.
  )
  
  # 2. Prepare Data
  df <- if(inherits(train_data, "sf")) st_drop_geometry(train_data) else train_data
  
  # Filter for Mode
  if("mode" %in% names(df)) {
    df <- df %>% filter(mode == mode_arg)
  } else {
    warning("   'mode' column missing. Using all data.")
  }
  
  # 3. Select Columns & Safety Check
  missing_preds <- setdiff(predictors, names(df))
  if(length(missing_preds) > 0) {
    # warning("   Skipping missing predictors: ", paste(missing_preds, collapse=", "))
    predictors <- setdiff(predictors, missing_preds)
  }
  
  cols_to_keep <- c(target_col, predictors, id_col)
  
  df <- df %>%
    select(all_of(cols_to_keep)) %>%
    na.omit() %>%
    mutate(across(where(is.character), as.factor))
  
  # Force Integer Counts
  df[[target_col]] <- as.integer(round(pmax(df[[target_col]], 0), 0))
  
  # 4. Construct Formula
  # target ~ fixed_preds + (1 | spatial_id)
  rhs <- paste(c(predictors, paste0("(1 | ", id_col, ")")), collapse = " + ")
  f_obj <- as.formula(paste(target_col, "~", rhs))
  
  # 5. Fit Model
  model <- lme4::glmer(f_obj, data = df, family = poisson(link = "log"), nAGQ = 0)
  
  return(model)
}

# --- Perform K-Fold Cross Validation for HGLM ---
#' @param data The full dataset
#' @param formula_obj The formula to test
#' @param k Number of folds (default 10)
validate_hglm_kfold <- function(data, mode_arg = "bike", 
                                target_col = "aadt", id_col = "spatial_id", k = 10) {
  
  require(dplyr)
  require(purrr)
  require(lme4)
  require(sf)
  require(yardstick) 
  
  message("...Starting ", k, "-Fold CV for Mode: ", mode_arg)
  
  # 1. Define Predictors (Same as before)
  predictors <- c(
    "year", "infra_type", "is_paved", "speed_limit",
    "emp_density", "int_density", "walk_index", "housing_total",
    "pop_low", "pop_high", "emp_low", "emp_high",
    "schools_low", "schools_high", "colleges_low", "colleges_high",
    "doctors_low", "doctors_high", "pharmacies_low", "pharmacies_high",
    "retail_low", "retail_high", "supermarket_low", "supermarket_high",
    "parks_low", "parks_high", "trails_low", "trails_high",
    "community_low", "community_high", "transit_low", "transit_high",
    "precip_annual"
  )
  
  # 2. Prepare Data
  df <- if(inherits(data, "sf")) st_drop_geometry(data) else data
  if("mode" %in% names(df)) df <- df %>% filter(mode == mode_arg)
  
  missing_preds <- setdiff(predictors, names(df))
  if(length(missing_preds) > 0) predictors <- setdiff(predictors, missing_preds)
  
  cols_to_keep <- c(target_col, predictors, id_col)
  df <- df %>%
    select(all_of(cols_to_keep)) %>%
    na.omit() %>%
    mutate(across(where(is.character), as.factor))
  
  df[[target_col]] <- as.integer(round(pmax(df[[target_col]], 0), 0))
  
  # 3. Formula
  rhs <- paste(c(predictors, paste0("(1 | ", id_col, ")")), collapse = " + ")
  f_obj <- as.formula(paste(target_col, "~", rhs))
  
  # 4. CV Loop: COLLECT ALL PREDICTIONS
  set.seed(42)
  df_shuffled <- df[sample(nrow(df)), ]
  k <- min(k, nrow(df))
  folds <- split(1:nrow(df_shuffled), cut(seq_along(1:nrow(df_shuffled)), breaks = k, labels = FALSE))
  
  # --- CHANGE HERE: Map to predictions, not metrics ---
  all_cv_predictions <- map_dfr(seq_along(folds), function(i) {
    test_idx <- folds[[i]]
    train_data <- df_shuffled[-test_idx, ]
    test_data  <- df_shuffled[test_idx, ]
    
    mod <- lme4::glmer(f_obj, data = train_data, family = poisson(link = "log"), nAGQ = 0)
    
    # Predict using Fixed Effects Only (simulating new location)
    preds <- predict(mod, newdata = test_data, type = "response", re.form = NA)
    
    test_data %>%
      select(all_of(target_col), all_of(id_col)) %>%
      mutate(
        predicted = preds,
        fold = i
      )
  })
  
  # 5. Summarize: Calculate metrics on the TOTAL dataset
  # This ensures n_obs matches your 1389/4340 totals
  summary_stats <- calculate_model_metrics(
    all_cv_predictions, 
    truth_col = target_col, 
    pred_col = "predicted"
  ) %>%
    mutate(
      model_type = "HGLM_Poisson",
      mode = mode_arg,
      k_folds = k,
      type = "Pooled Global"
    )
  
  print(summary_stats)
  return(summary_stats)
}

 