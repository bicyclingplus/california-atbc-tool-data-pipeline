# Tests for src/functions/export.R -- the Node model export.

test_that("export_models_for_node writes model, json, and feature-spec per mode", {
  # Train tiny bike + ped models with known predictors.
  mk <- function(target) {
    set.seed(1)
    d <- data.frame(
      y = rpois(60, 10),
      spatial_id = as.character(1:60),
      infra_type = sample(c("bike_lane", "quiet_street"), 60, TRUE),
      emp_density = runif(60)
    )
    names(d)[1] <- target
    train_lgb(d, c("infra_type", "emp_density"), target, nrounds = 10)
  }
  bike <- mk("aadb"); ped <- mk("aadp")

  out_dir <- tempfile(); dir.create(out_dir)
  files <- export_models_for_node(bike, ped, out_dir)

  # 3 files per mode = 6 returned paths, all existing
  expect_length(files, 6)
  expect_true(all(file.exists(files)))
  expect_true(file.exists(file.path(out_dir, "bike_model.txt")))
  expect_true(file.exists(file.path(out_dir, "ped_feature_spec.json")))

  # Feature spec records the exact one-hot column order and the no-exponentiate note.
  spec <- jsonlite::read_json(file.path(out_dir, "bike_feature_spec.json"))
  expect_equal(spec$objective, "tweedie")
  expect_true(length(spec$onehot_columns) >= 2)
  expect_match(spec$note, "do NOT exponentiate", ignore.case = TRUE)
})
