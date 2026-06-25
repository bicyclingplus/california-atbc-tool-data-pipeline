# Tests for src/functions/modeling.R -- predictor sets, encoding, prediction, scoring.

test_that("lgb_params returns mode-specific tuned values", {
  pb <- lgb_params("aadb")               # bicycle (track A default)
  pp <- lgb_params("aadp")               # pedestrian
  expect_equal(pb$tweedie_variance_power, 1.9)
  expect_equal(pb$feature_fraction, 0.7)
  expect_equal(pp$tweedie_variance_power, 1.6)
  expect_equal(pp$feature_fraction, 1.0)
  expect_equal(pb$objective, "tweedie")
})

test_that("lgb_params lists ALL behavior-affecting params explicitly", {
  p <- lgb_params("aadb", "A")
  needed <- c("objective", "tweedie_variance_power", "learning_rate", "num_leaves",
              "min_data_in_leaf", "feature_fraction", "bagging_fraction",
              "bagging_freq", "max_depth", "min_gain_to_split",
              "lambda_l1", "lambda_l2", "verbosity")
  expect_true(all(needed %in% names(p)))   # nothing hidden
})

test_that("lgb_params applies per-track regularization", {
  expect_equal(lgb_params("aadb", "A")$lambda_l1, 0.5)
  expect_equal(lgb_params("aadb", "A")$lambda_l2, 0.0)
  expect_equal(lgb_params("aadb", "B")$lambda_l2, 2.0)
  expect_equal(lgb_params("aadp", "A")$lambda_l2, 0.0)   # ped A: no reg
  expect_equal(lgb_params("aadp", "B")$lambda_l1, 0.5)
})

test_that("train_lgb infers track from presence of on-link Strava", {
  d <- data.frame(aadb = rpois(40, 10), spatial_id = as.character(1:40),
                  emp_density = runif(40), strava_vol_total = runif(40))
  mA <- train_lgb(d, c("strava_vol_total", "emp_density"), "aadb", nrounds = 5)
  mB <- train_lgb(d, c("emp_density"), "aadb", nrounds = 5)
  expect_equal(mA$track, "A")
  expect_equal(mB$track, "B")
})

test_that("predictor sets differ only by on-link Strava", {
  expect_true("strava_vol_total" %in% PREDICTORS_A)
  expect_false("strava_vol_total" %in% PREDICTORS_B)
  expect_setequal(setdiff(PREDICTORS_A, PREDICTORS_B), "strava_vol_total")
  # crash + per-site WWI dropped from both
  expect_false("crash_count_30m" %in% PREDICTORS_A)
  expect_false("WWI" %in% PREDICTORS_A)
})

test_that("lgb_matrix one-hot encodes factors and fills numeric NA with 0", {
  d <- data.frame(
    aadb = c(1, 2),
    infra_type = c("bike_lane", "quiet_street"),
    speed_limit = c(25, NA),
    stringsAsFactors = FALSE
  )
  m <- lgb_matrix(d, c("infra_type", "speed_limit"), "aadb")
  expect_true(is.matrix(m$x))
  expect_equal(m$y, c(1, 2))
  expect_false(any(is.na(m$x)))                       # NA speed_limit -> 0
  expect_true(any(grepl("infra_type", m$cols)))       # factor expanded
})

test_that("train_lgb + predict_lgb run and align to the training schema", {
  set.seed(1)
  n <- 120
  d <- data.frame(
    aadb = rpois(n, 20),
    spatial_id = as.character(1:n),
    infra_type = sample(c("bike_lane", "quiet_street", "shared_arterial"), n, TRUE),
    speed_limit = sample(c(25, 35, 45), n, TRUE),
    emp_density = runif(n)
  )
  preds <- c("infra_type", "speed_limit", "emp_density")
  m <- train_lgb(d, preds, "aadb", nrounds = 20)
  expect_s3_class(m, "lgb_model")          # RDS-safe wrapper, not a raw Booster
  expect_equal(m$target, "aadb")
  expect_type(m$model_text, "character")   # serialized model survives saveRDS

  # Prediction on data MISSING a factor level still works (schema alignment).
  newd <- d[1:5, ]
  newd$infra_type <- "bike_lane"
  p <- predict_lgb(m, newd)
  expect_length(p, 5)
  expect_true(all(p >= 0))                            # counts, non-negative
})

test_that("validate_lgb reports class accuracy and confusion matrix", {
  set.seed(2)
  n <- 200
  d <- data.frame(
    aadb = rpois(n, 30),
    spatial_id = as.character(rep(1:40, each = 5)),   # grouped for spatial CV
    emp_density = runif(n),
    pop_high = runif(n)
  )
  v <- validate_lgb(d, c("emp_density", "pop_high"), "aadb", v = 3)
  expect_true(all(c("confusion", "class_accuracy", "off_by_two", "rmse") %in% names(v)))
  expect_true(v$class_accuracy >= 0 && v$class_accuracy <= 1)
  expect_true(is.table(v$confusion))
})
