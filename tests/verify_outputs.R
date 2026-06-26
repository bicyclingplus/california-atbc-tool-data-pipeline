# =============================================================================
# Verify data_processed/ pipeline outputs before handoff to the web developer.
# -----------------------------------------------------------------------------
# Standalone output-QA harness, separate from tests/testthat/ (which uses
# synthetic inputs and never touches Box). This one reads the real multi-GB
# outputs, so it is NOT part of the testthat::test_dir() run.
#
# Read-only. It never loads the geojsons whole: it asks GDAL for the feature
# count and a small sample of rows (OGR SQL COUNT / LIMIT), reads only the
# header of the raster, and loads the small model/JSON files directly.
#
# Assumes src/config.R has already run this session, so the working directory
# is the Box project root (data_processed/ is a relative path) and the packages
# (sf, terra, jsonlite, lightgbm, dplyr) are loaded. Then source this script.
#
# The three large geojson queries take several minutes each, because GeoJSON
# has no spatial index and GDAL must scan the whole file.
# =============================================================================

dp <- "data_processed"
wm <- file.path(dp, "web_models")

# Each check records its result here; we print a tally at the end.
results <- new.env()
results$pass <- 0
results$warn <- 0
results$fail <- 0

# Print a check result and bump the matching counter.
report_pass <- function(msg) {
  cat("  [PASS]", msg, "\n")
  results$pass <- results$pass + 1
}
report_warn <- function(msg) {
  cat("  [WARN]", msg, "\n")
  results$warn <- results$warn + 1
}
report_fail <- function(msg) {
  cat("  [FAIL]", msg, "\n")
  results$fail <- results$fail + 1
}
section <- function(title) {
  cat("\n===", title, "===\n")
}

# Count features without reading any geometry.
ogr_count <- function(path, layer) {
  query <- sprintf('SELECT COUNT(*) AS n FROM "%s"', layer)
  as.integer(st_read(path, query = query, quiet = TRUE)$n)
}

# Read the first n features (attributes + geometry) as a sample.
ogr_head <- function(path, layer, n = 50000) {
  query <- sprintf('SELECT * FROM "%s" LIMIT %d', layer, n)
  st_read(path, query = query, quiet = TRUE)
}

# One-line numeric summary of a column.
num_summary <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  sprintf(
    "min=%.3g med=%.3g max=%.3g NA=%d neg=%d",
    min(x, na.rm = TRUE), median(x, na.rm = TRUE), max(x, na.rm = TRUE),
    sum(is.na(x)), sum(x < 0, na.rm = TRUE)
  )
}

# Assert that no attribute column in a sample has any NULL/NA value. Every field
# in the finished network is filled in by the pipeline (volumes get
# replace_na(0), exposure classes default to "Low", functional falls through to
# "Local Road"), so any NA in the output points to a real defect -- e.g. an
# infra_type that was never assigned to the nodes. Geometry is excluded; we only
# check attributes.
check_no_na_attributes <- function(sample, label) {
  attributes_only <- sf::st_drop_geometry(sample)
  na_counts <- vapply(attributes_only, function(col) sum(is.na(col)), integer(1))
  columns_with_na <- na_counts[na_counts > 0]
  if (length(columns_with_na) == 0) {
    report_pass(paste(label, "has no NA in any attribute (sample)"))
  } else {
    detail <- paste(names(columns_with_na), columns_with_na, sep = "=", collapse = ", ")
    report_fail(paste(label, "has NA attributes (sample):", detail))
  }
}


# ===========================================================================
section("links.geojson")

tryCatch({
  path <- file.path(dp, "links.geojson")
  layer <- st_layers(path)$name[1]

  # Feature count should be in the millions for the statewide link network.
  n <- ogr_count(path, layer)
  cat("  feature count:", n, "\n")
  if (n > 1e5) {
    report_pass("link count plausible")
  } else {
    report_warn(sprintf("link count low: %d", n))
  }

  sample <- ogr_head(path, layer)

  # All fields that finalize_web_network() selects for links must be present.
  expected <- c(
    "edge_uid", "pred_bike_vol", "length_ft", "bicycle_exposure_class",
    "functional", "infra_type", "pred_ped_vol", "pedestrian_exposure_class",
    "source", "target"
  )
  missing <- setdiff(expected, names(sample))
  if (length(missing) == 0) {
    report_pass("all expected link fields present")
  } else {
    report_fail(paste("missing link fields:", paste(missing, collapse = ", ")))
  }

  # No attribute should be NA: the pipeline fills every link field.
  check_no_na_attributes(sample, "links")

  # Web data must be lon/lat (EPSG:4326) for the web map.
  if (isTRUE(st_crs(sample)$epsg == 4326)) {
    report_pass("CRS = EPSG:4326")
  } else {
    report_fail(paste("CRS not 4326:", st_crs(sample)$epsg))
  }

  cat("  pred_bike_vol (sample):", num_summary(sample$pred_bike_vol), "\n")
  cat("  pred_ped_vol  (sample):", num_summary(sample$pred_ped_vol), "\n")

  # Predicted volumes must be non-negative and not uniformly zero.
  bike_vol <- as.numeric(sample$pred_bike_vol)
  if (all(bike_vol >= 0, na.rm = TRUE) && any(bike_vol > 0, na.rm = TRUE)) {
    report_pass("pred_bike_vol non-negative and not all-zero")
  } else {
    report_fail("pred_bike_vol bad (negatives or all-zero)")
  }

  exposure <- table(sample$bicycle_exposure_class, useNA = "ifany")
  cat("  bike exposure dist (sample):",
      paste(names(exposure), exposure, sep = "=", collapse = "  "), "\n")

  # Exposure class should only ever be Low / Medium / High.
  unexpected <- setdiff(names(exposure), c("Low", "Medium", "High"))
  if (length(unexpected) == 0) {
    report_pass("bike exposure classes valid")
  } else {
    report_fail(paste("unexpected exposure class:", paste(unexpected, collapse = ",")))
  }

  # All three classes should appear; only Low would mean the quantile cut collapsed.
  if (all(c("High", "Medium") %in% names(exposure))) {
    report_pass("exposure classes not collapsed to all-Low")
  } else {
    report_warn("exposure classes look collapsed (mostly Low) in sample")
  }

  functional <- table(sample$functional)
  cat("  functional (sample):",
      paste(names(functional), functional, sep = "=", collapse = "  "), "\n")

  # functional must be the road hierarchy (Major/Minor/Local Road), not bike
  # infra. Major Road being absent was the symptom of the old map_func bug.
  bad_functional <- setdiff(names(functional), c("Major Road", "Minor Road", "Local Road"))
  if (length(bad_functional) > 0) {
    report_fail(paste("unexpected functional values:", paste(bad_functional, collapse = ", ")))
  } else if (!("Major Road" %in% names(functional))) {
    report_warn("no Major Road in links sample (expected on a statewide network)")
  } else {
    report_pass("functional has the road-hierarchy classes incl. Major Road")
  }

  infra <- table(sample$infra_type)
  cat("  infra_type (sample):",
      paste(names(infra), infra, sep = "=", collapse = "  "), "\n")
}, error = function(e) report_fail(paste("links.geojson errored:", conditionMessage(e))))


# ===========================================================================
section("nodes.geojson")

tryCatch({
  path <- file.path(dp, "nodes.geojson")
  layer <- st_layers(path)$name[1]

  # Node count: expected to be smaller than the link count, but still large.
  n <- ogr_count(path, layer)
  cat("  feature count:", n, "\n")
  report_pass(sprintf("node count = %d", n))

  sample <- ogr_head(path, layer)

  # All fields that finalize_web_network() selects for nodes must be present.
  expected <- c(
    "node_id", "pred_ped_vol", "functional", "infra_type",
    "pedestrian_exposure_class", "pred_bike_vol", "bicycle_exposure_class"
  )
  missing <- setdiff(expected, names(sample))
  if (length(missing) == 0) {
    report_pass("all expected node fields present")
  } else {
    report_fail(paste("missing node fields:", paste(missing, collapse = ", ")))
  }

  # Web data must be lon/lat (EPSG:4326).
  if (isTRUE(st_crs(sample)$epsg == 4326)) {
    report_pass("CRS = EPSG:4326")
  } else {
    report_fail(paste("CRS not 4326:", st_crs(sample)$epsg))
  }

  cat("  pred_ped_vol (sample):", num_summary(sample$pred_ped_vol), "\n")

  # No attribute should be NA. Nodes inherit infra_type (most-protective) and
  # functional (highest class) from their touching links in prep_network_topology.
  check_no_na_attributes(sample, "nodes")

  # functional must be the road hierarchy and should include Major Road.
  functional <- table(sample$functional)
  cat("  node functional (sample):",
      paste(names(functional), functional, sep = "=", collapse = "  "), "\n")
  bad_functional <- setdiff(names(functional), c("Major Road", "Minor Road", "Local Road"))
  if (length(bad_functional) > 0) {
    report_fail(paste("unexpected node functional values:", paste(bad_functional, collapse = ", ")))
  } else if (!("Major Road" %in% names(functional))) {
    report_warn("no Major Road in nodes sample (expected on a statewide network)")
  } else {
    report_pass("node functional has the road-hierarchy classes incl. Major Road")
  }
}, error = function(e) report_fail(paste("nodes.geojson errored:", conditionMessage(e))))


# ===========================================================================
section("context_blocks.geojson")

tryCatch({
  path <- file.path(dp, "context_blocks.geojson")
  layer <- st_layers(path)$name[1]

  # One feature per census block; expect roughly 400k.
  n <- ogr_count(path, layer)
  cat("  feature count:", n, "\n")
  if (n > 1e5) {
    report_pass("block count plausible")
  } else {
    report_warn(sprintf("block count low: %d", n))
  }

  sample <- ogr_head(path, layer, 20000)

  # prepare_and_export_web_blocks() projects to Web Mercator (EPSG:3857).
  if (isTRUE(st_crs(sample)$epsg == 3857)) {
    report_pass("CRS = EPSG:3857 (Web Mercator)")
  } else {
    report_warn(paste("CRS not 3857:", st_crs(sample)$epsg))
  }

  # Every predictor the web tool reads from the block (i.e. everything in the
  # feature spec except the UI-supplied infra_type / is_paved / speed_limit)
  # must exist as a column here, or the spatial join would be missing inputs.
  spec <- fromJSON(file.path(wm, "bike_feature_spec.json"))
  ui_supplied <- c("infra_type", "is_paved", "speed_limit")
  block_needed <- setdiff(spec$raw_predictors, ui_supplied)
  missing <- setdiff(block_needed, names(sample))
  if (length(missing) == 0) {
    report_pass("all block-supplied model predictors present")
  } else {
    report_fail(paste("context_blocks MISSING predictors needed by web tool:",
                      paste(missing, collapse = ", ")))
  }

  ambient <- c("amb_strava_250m", "amb_strava_500m",
               "amb_strava_1000m", "amb_strava_2000m")
  for (col in ambient) {
    if (col %in% names(sample)) {
      cat("  ", col, "(sample):", num_summary(sample[[col]]), "\n")
    }
  }

  # Ambient features are log1p-scaled, so values should be modest, not raw
  # counts in the thousands.
  if (all(ambient %in% names(sample))) {
    max_ambient <- max(sapply(ambient, function(col) {
      max(as.numeric(sample[[col]]), na.rm = TRUE)
    }))
    if (max_ambient < 25) {
      report_pass(sprintf("ambient looks log1p-scaled (max=%.1f)", max_ambient))
    } else {
      report_warn(sprintf("ambient max=%.1f too large for log1p", max_ambient))
    }
  }

  # PRISM weather columns should be populated (this run depended on a PRISM
  # rerun, so confirm they are not mostly NA / zero).
  for (col in c("precip_annual", "temp_min", "temp_max")) {
    if (col %in% names(sample)) {
      cat("  ", col, "(sample):", num_summary(sample[[col]]), "\n")
      mostly_empty <- mean(is.na(sample[[col]]) | as.numeric(sample[[col]]) == 0) > 0.5
      if (mostly_empty) {
        report_warn(paste(col, "mostly NA/zero -> PRISM weather may not have populated"))
      }
    }
  }
}, error = function(e) report_fail(paste("context_blocks.geojson errored:", conditionMessage(e))))


# ===========================================================================
section("strava_grid.tif")

tryCatch({
  raster <- rast(file.path(dp, "strava_grid.tif"))
  value_range <- minmax(raster)
  cat("  dims:", nrow(raster), "x", ncol(raster),
      "| res:", paste(round(res(raster), 1), collapse = "x"),
      "| range: [", round(value_range[1], 3), ",", round(value_range[2], 3), "]\n")

  # The demand grid should have positive values somewhere.
  if (is.finite(value_range[2]) && value_range[2] > 0) {
    report_pass("raster has positive values")
  } else {
    report_fail("raster empty / all-zero / all-NA")
  }

  # Built at 100 m resolution.
  if (all(res(raster) > 50 & res(raster) < 200)) {
    report_pass("resolution ~100 m")
  } else {
    report_warn(paste("resolution unexpected:", paste(round(res(raster), 1), collapse = "x")))
  }
}, error = function(e) report_fail(paste("strava_grid.tif errored:", conditionMessage(e))))


# ===========================================================================
section("Appendix A CSVs")

for (file in c("appendix_a_links.csv", "appendix_a_nodes.csv")) {
  tryCatch({
    appendix <- read.csv(file.path(dp, file), check.names = FALSE)
    cat("  ", file, ":", nrow(appendix), "rows x", ncol(appendix), "cols\n")

    # The export writes exactly 12 columns.
    if (ncol(appendix) == 12) {
      report_pass(paste(file, "has 12 columns"))
    } else {
      report_fail(sprintf("%s has %d cols (expected 12)", file, ncol(appendix)))
    }

    # Mode is only ever Bike or Walk.
    if (all(appendix$Mode %in% c("Bike", "Walk"))) {
      report_pass(paste(file, "Mode valid"))
    } else {
      report_fail(paste(file, "bad Mode values"))
    }

    # Functional class values must be valid AND all three classes should be
    # present. A missing "Major Road" was the symptom of the old map_func bug
    # (which fed bike infra_type into the road-hierarchy mapping), so a
    # value-set check alone is not enough -- assert all three appear.
    classes <- unique(appendix[["Functional Class"]])
    if (!all(classes %in% c("Major Road", "Minor Road", "Local Road"))) {
      report_fail(paste(file, "unexpected Functional Class:",
                        paste(setdiff(classes, c("Major Road", "Minor Road", "Local Road")), collapse = ", ")))
    } else if (!all(c("Major Road", "Minor Road", "Local Road") %in% classes)) {
      report_fail(paste(file, "missing functional class(es):",
                        paste(setdiff(c("Major Road", "Minor Road", "Local Road"), classes), collapse = ", ")))
    } else {
      report_pass(paste(file, "all three functional classes present"))
    }

    # The alpha columns use -15 as an infinity sentinel; every row at -15 would
    # mean every rate came out zero or undefined.
    alpha_crash <- appendix[["α Crash"]]
    n_sentinel <- sum(alpha_crash == -15, na.rm = TRUE)
    cat("  α Crash:", num_summary(alpha_crash),
        "| at -15 sentinel:", n_sentinel, "/", nrow(appendix), "\n")
    if (n_sentinel == nrow(appendix)) {
      report_fail(paste(file, "all alphas at -15 infinity sentinel"))
    } else {
      report_pass(sprintf("%s alphas finite (%d sentinel)", file, n_sentinel))
    }
  }, error = function(e) report_fail(paste(file, "errored:", conditionMessage(e))))
}


# ===========================================================================
section("web_models/ (Track B bundle)")

for (mode in c("bike", "ped")) {
  tryCatch({
    spec <- fromJSON(file.path(wm, paste0(mode, "_feature_spec.json")))
    # lgb.load needs the path passed as the named `filename` argument; a
    # positional path mis-binds and yields a non-Booster (the earlier
    # "attempt to apply non-function" failure).
    model <- lightgbm::lgb.load(filename = file.path(wm, paste0(mode, "_model.txt")))
    n_model_feat <- model$num_feature()
    n_spec_cols <- length(spec$onehot_columns)
    cat("  ", mode, ": model features =", n_model_feat,
        "| spec onehot_columns =", n_spec_cols, "\n")

    # The model's feature count must match the spec the web tool builds against.
    if (n_model_feat == n_spec_cols) {
      report_pass(paste(mode, "feature count matches spec"))
    } else {
      report_fail(sprintf("%s feature mismatch: model=%d spec=%d",
                          mode, n_model_feat, n_spec_cols))
    }

    # An all-zero input row should still yield a finite, non-negative count.
    input <- matrix(0, nrow = 1, ncol = n_spec_cols)
    colnames(input) <- spec$onehot_columns
    prediction <- predict(model, input)
    cat("  ", mode, "dummy prediction (all-zero row):", round(prediction, 4), "\n")
    if (is.finite(prediction) && prediction >= 0) {
      report_pass(paste(mode, "predicts finite non-negative count"))
    } else {
      report_fail(paste(mode, "prediction invalid:", prediction))
    }

    # The lgb.dump JSON the web tool may parse should be valid JSON.
    invisible(fromJSON(file.path(wm, paste0(mode, "_model.json"))))
    report_pass(paste0(mode, "_model.json parses"))
  }, error = function(e) report_fail(paste(mode, "model errored:", conditionMessage(e))))
}


# ===========================================================================
section("ONNX freshness")

for (mode in c("bike", "ped")) {
  onnx <- file.path(wm, paste0(mode, "_model.onnx"))
  txt <- file.path(wm, paste0(mode, "_model.txt"))

  if (file.exists(onnx) && file.exists(txt)) {
    # The .onnx is produced outside the pipeline. If it predates the .txt it is
    # stale and the Node tool would serve an old model.
    hours_older <- as.numeric(difftime(file.mtime(txt), file.mtime(onnx), units = "hours"))
    cat("  ", mode, ": onnx =", format(file.mtime(onnx)),
        "| txt =", format(file.mtime(txt)), "\n")
    if (hours_older > 1) {
      report_warn(sprintf("%s .onnx older than .txt by %.1f h -> stale, re-run src/convert_to_onnx.py",
                          mode, hours_older))
    } else {
      report_pass(paste(mode, ".onnx newer than/equal to .txt"))
    }
  } else {
    report_warn(paste(mode, ".onnx missing -> run src/convert_to_onnx.py"))
  }
}


# ===========================================================================
cat("\n========================================\n")
cat(sprintf("SUMMARY: %d PASS, %d WARN, %d FAIL\n",
            results$pass, results$warn, results$fail))
cat("========================================\n")
