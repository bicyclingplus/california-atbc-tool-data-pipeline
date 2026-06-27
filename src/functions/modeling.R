library(lightgbm)
library(dplyr)
library(sf)

# ============================================================================
# PREDICTOR SETS (single source of truth)
# ----------------------------------------------------------------------------
# ONE base set used by BOTH model tracks. The only difference between tracks is
# the on-link Strava variable:
#
#   Track A (existing network)   : PREDICTORS_A = base + strava_vol_total
#   Track B (new off-street paths): PREDICTORS_B = base   (no on-link Strava;
#                                    a new path has no Strava history yet)
#
# Both tracks use AMBIENT Strava rings (amb_strava_*), which are available even
# for a not-yet-built path (computed from the surrounding network) and were
# tested to carry most of the demand signal.
#
# CLIMATE (precip_annual, temp_min, temp_max) is sampled per-segment from the
# PRISM 4km annual grid (terra::extract), replacing the old nearest-station
# Voronoi join that mis-assigned foothill segments statewide. temp_min/temp_max
# were added alongside the existing precip_annual.
# ============================================================================
AMBIENT_FEATURES <- c("amb_strava_250m", "amb_strava_500m",
                      "amb_strava_1000m", "amb_strava_2000m")

PREDICTORS_BASE <- c(
  "infra_type", "functional", "is_paved", "speed_limit",
  "emp_density", "int_density", "walk_index", "housing_total",
  "pop_low", "pop_high", "emp_low", "emp_high",
  "schools_low", "schools_high", "colleges_low", "colleges_high",
  "doctors_low", "doctors_high", "pharmacies_low", "pharmacies_high",
  "retail_low", "retail_high", "supermarket_low", "supermarket_high",
  "parks_low", "parks_high", "trails_low", "trails_high",
  "community_low", "community_high", "transit_low", "transit_high",
  "precip_annual", "temp_min", "temp_max",
  AMBIENT_FEATURES
)

PREDICTORS_A <- c("strava_vol_total", PREDICTORS_BASE)   # existing network
PREDICTORS_B <- PREDICTORS_BASE                          # new off-street paths

# LightGBM hyperparameters -- ALL behavior-affecting parameters listed explicitly
# (no hidden defaults). Selected in TWO stages of grid search under spatial 5-fold
# CV with early stopping (src/model_tuning): stage 1 = tweedie_variance_power x
# num_leaves x min_data_in_leaf x feature_fraction (tvp swept at 0.1 resolution);
# stage 2 = regularization (lambda_l1/l2, bagging, max_depth) holding stage-1
# winners fixed. Re-tuned 2026-06-26 WITH the `functional` predictor.
#
# Selection is NOT single-metric: priority order is (1) volume-class accuracy,
# (2) low severe-misclassification (off-by-2, low<->high), (3) lower RMSE as the
# tie-breaker, with the per-fold accuracy SE (~0.01) as the equivalence band.
# Result: bike favors a heavy Tweedie power (1.9, best on all three); ped uses a
# lower power (1.7, since higher powers buy only within-SE accuracy at worse off2
# and RMSE) plus mild regularization incl. light row-subsampling that improves
# ped calibration. See README_modeling.md "Hyperparameter selection".
#
#   target : "aadb" (bicycle) or "aadp" (pedestrian)
#   track  : "A" (existing network, with on-link Strava) or "B" (Strava-free)
lgb_params <- function(target, track = "A") {
  label <- paste0(track, "_", if (identical(target, "aadb")) "bike" else "ped")

  # --- per-(mode, track) tuned values ---------------------------------------
  # tvp, num_leaves, min_data_in_leaf, feature_fraction, lambda_l1, lambda_l2,
  # bagging_fraction. bagging_freq is derived (1 when bagging<1, else 0).
  p <- switch(label,
    A_bike = list(tvp = 1.9, nleaf = 63, mdil = 20, ff = 0.7, l1 = 0.0, l2 = 0.0, bag = 1.0),
    B_bike = list(tvp = 1.9, nleaf = 95, mdil = 20, ff = 0.7, l1 = 2.0, l2 = 2.0, bag = 1.0),
    A_ped  = list(tvp = 1.7, nleaf = 63, mdil = 50, ff = 0.7, l1 = 0.5, l2 = 0.0, bag = 0.8),
    B_ped  = list(tvp = 1.7, nleaf = 31, mdil = 20, ff = 1.0, l1 = 2.0, l2 = 2.0, bag = 0.8),
    stop("lgb_params: unknown target/track combination: ", label)
  )

  list(
    # --- estimation approach (LightGBM defaults, stated explicitly so the model
    #     is reproducible against future default changes) ---------------------
    boosting               = "gbdt", # gradient boosted decision trees (not dart/goss/rf)
    # trees are grown LEAF-WISE (best-first); num_leaves -- not max_depth -- is the
    # primary complexity control, which is why max_depth is left uncapped below.
    max_bin                = 255L,   # histogram bins for split finding (default)
    min_sum_hessian_in_leaf = 1e-3,  # default; min hessian (curvature) per leaf
    min_data_in_bin        = 3L,     # default
    # --- objective ------------------------------------------------------------
    objective              = "tweedie",
    tweedie_variance_power = p$tvp,
    # --- tuned + complexity ---------------------------------------------------
    learning_rate          = 0.05,
    num_leaves             = p$nleaf,
    min_data_in_leaf       = p$mdil,
    feature_fraction       = p$ff,                       # column subsampling
    bagging_fraction       = p$bag,                      # row subsampling
    bagging_freq           = if (p$bag < 1) 1L else 0L,  # bagging needs freq>0
    max_depth              = -1,     # no depth limit; num_leaves governs (leaf-wise)
    min_gain_to_split      = 0.0,    # LightGBM default, stated explicitly
    lambda_l1              = p$l1,
    lambda_l2              = p$l2,
    verbosity              = -1
  )
}

# ============================================================================
# LightGBM Tweedie models (Track A: with on-link Strava; Track B: Strava-free)
# ----------------------------------------------------------------------------
# Encoding: factors -> one-hot via model.matrix. The exact column schema is
# captured with the model (attr "train_cols") so prediction aligns identically
# (and so the Node/ONNX export can reproduce it).
# ============================================================================

#' Build the numeric design matrix for a predictor set (one-hot factors, NA->0).
#' Returns list(x = matrix, y = numeric|NULL, cols = colnames).
lgb_matrix <- function(data, predictors, target = NULL) {
  d <- data
  if (inherits(d, "sf")) d <- sf::st_drop_geometry(d)
  d <- d[, predictors, drop = FALSE]
  d <- d %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor)) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0)))

  # Build the design matrix column by column so that single-level factors expand
  # to a single dummy (model.matrix's contrasts fail on one-level factors, which
  # occurs when a prediction chunk happens to contain one facility type).
  cols <- list()
  for (nm in predictors) {
    v <- d[[nm]]
    if (is.factor(v)) {
      for (lv in levels(v)) cols[[paste0(nm, lv)]] <- as.numeric(v == lv)
    } else {
      cols[[nm]] <- as.numeric(v)
    }
  }
  mm <- matrix(unlist(cols), nrow = nrow(d), dimnames = list(NULL, names(cols)))
  y <- if (!is.null(target) && target %in% names(data)) pmax(as.numeric(data[[target]]), 0) else NULL
  list(x = mm, y = y, cols = colnames(mm))
}

#' Train a LightGBM Tweedie model on the full training data.
#' @param train_data snapped training set (sf or data.frame)
#' @param predictors PREDICTORS_A (Track A, with on-link Strava) or PREDICTORS_B
#'   (Track B, Strava-free). The track is inferred from this to select the tuned
#'   per-track regularization.
#' @param target     "aadb" or "aadp"
# NOTE on persistence: a live lgb.Booster is a C++ object that does NOT survive
# saveRDS() -- and `targets` caches every target with saveRDS. So train_lgb does
# NOT return a raw Booster; it returns a plain list holding the model's TEXT dump
# (an ordinary string, RDS-safe) plus metadata. lgb_booster() reconstitutes a
# live Booster from the text on demand (predict_lgb, export, validation).
train_lgb <- function(train_data, predictors, target, nrounds = 600) {
  track <- if ("strava_vol_total" %in% predictors) "A" else "B"
  m <- lgb_matrix(train_data, predictors, target)
  keep <- !is.na(m$y)
  dtrain <- lightgbm::lgb.Dataset(m$x[keep, , drop = FALSE], label = m$y[keep])
  booster <- lightgbm::lgb.train(params = lgb_params(target, track),
                                 data = dtrain, nrounds = nrounds)
  tmp <- tempfile(fileext = ".txt")
  lightgbm::lgb.save(booster, tmp)
  model_text <- paste(readLines(tmp), collapse = "\n")
  unlink(tmp)

  structure(
    list(model_text = model_text,
         train_cols = m$cols,
         predictors = predictors,
         target     = target,
         track      = track),
    class = "lgb_model"
  )
}

#' Reconstitute a live lgb.Booster from a stored lgb_model (or accept a raw
#' Booster for backward compatibility).
lgb_booster <- function(model) {
  if (inherits(model, "lgb.Booster")) return(model)
  lightgbm::lgb.load(model_str = model$model_text)
}

#' Predict with a trained model, aligning columns to the training schema.
predict_lgb <- function(model, newdata) {
  predictors <- model$predictors
  tc <- model$train_cols
  booster <- lgb_booster(model)
  m <- lgb_matrix(newdata, predictors, target = NULL)
  x <- matrix(0, nrow = nrow(m$x), ncol = length(tc), dimnames = list(NULL, tc))
  common <- intersect(colnames(m$x), tc)
  x[, common] <- m$x[, common]
  pmax(stats::predict(booster, x), 0)
}

#' Spatial 10-fold CV reporting CLASS accuracy + ABSOLUTE error
validate_lgb <- function(train_data, predictors, target, v = 10) {
  require(rsample); require(dplyr)
  df <- train_data
  if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
  df <- df %>% dplyr::filter(!is.na(.data[[target]]), !is.na(spatial_id))

  set.seed(123)
  folds <- rsample::group_vfold_cv(df, group = spatial_id, v = v)
  oof <- rep(NA_real_, nrow(df)); fold_id <- rep(NA_integer_, nrow(df))
  for (i in seq_along(folds$splits)) {
    s <- folds$splits[[i]]
    te <- as.integer(rsample::complement(s)); tr <- setdiff(seq_len(nrow(df)), te)
    fold_id[te] <- i
    m <- train_lgb(df[tr, , drop = FALSE], predictors, target)
    oof[te] <- predict_lgb(m, df[te, , drop = FALSE])
  }

  y <- pmax(df[[target]], 0)
  q <- stats::quantile(y, c(0, 1/3, 2/3, 1), na.rm = TRUE)
  oc <- cut(y,   q, include.lowest = TRUE, labels = c("low", "mid", "high"))
  pc <- cut(oof, q, include.lowest = TRUE, labels = c("low", "mid", "high"))
  pc[oof > q[4]] <- "high"; pc[is.na(pc)] <- "high"
  cm <- table(actual = oc, predicted = pc)
  ord <- c(low = 1, mid = 2, high = 3)
  off2 <- mean(abs(ord[as.character(oc)] - ord[as.character(pc)]) == 2)
  ae <- tapply(abs(oof - y), oc, median)

  list(
    confusion = cm,
    class_accuracy = sum(diag(cm)) / sum(cm),
    off_by_two = off2,
    median_abs_err = ae,
    rmse = sqrt(mean((oof - y)^2)),
    oof = tibble::tibble(spatial_id = df$spatial_id, obs = y, pred = oof, fold = fold_id)
  )
}

#' Predict bike volumes on Links and ped volumes on Nodes (Track A models, which
#' have on-link Strava available on the existing network).
predict_split_networks <- function(bike_model, ped_model, link_net, node_net) {
  require(sf)
  message("--- Predicting bike volumes on links ---")
  link_net$pred_bike_vol <- predict_lgb(bike_model, link_net)
  message("--- Predicting ped volumes on nodes ---")
  node_net$pred_ped_vol  <- predict_lgb(ped_model, node_net)
  list(links = link_net, nodes = node_net)
}
