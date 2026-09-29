# Report Section 4 tables + figures -> one Word document
#
# Builds every table and map for report Section 4 ("Estimate Existing Active
# Travel") from the targets store and stitches them into a single .docx, in
# report order with report numbering, for copy/paste into the report. Text for
# the section lives in README_Section4.md; its placeholders name the captions here.
#
# Run INTERACTIVELY in R 4.2.3 after config.R (working dir = Box project root,
# function library sourced). Rscript segfaults reading the Box RDS files.
#
# Output: data_processed/report/section4_outputs.docx
library(targets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(officer)
library(flextable)

# ============================================================================
# LABELS
# ============================================================================
SOURCE_LABELS <- c(
  UCB_GoldStandard     = "UC Berkeley SafeTREC",   # relabeled per mode below
  Caltrans_InternalExp = "Caltrans AT Count Dataset",
  CAT_Portal           = "CA Active Transportation Data Portal"
)

source_label <- function(source, mode) {
  lab <- unname(SOURCE_LABELS[source])
  lab[source == "UCB_GoldStandard" & mode == "bike"] <- "Statewide AADBT (Miah et al. 2024b)"
  lab[source == "UCB_GoldStandard" & mode == "ped"]  <- "SHS pedestrian model (Griswold et al. 2019)"
  lab[is.na(lab)] <- source[is.na(lab)]
  lab
}

MODEL_LABELS <- c(A = "Existing network (Track A)", B = "New facility (Track B)")

# One row per predictor in PREDICTORS_A (modeling.R). build_predictor_table()
# stops if a predictor is added to the model without a row here.
PREDICTOR_INFO <- tribble(
  ~code,              ~category,             ~source,                                   ~variable,                                                         ~type,
  "strava_vol_total", "Strava (on-link)",    "Strava Metro (2023)",                     "Annual trips on the link; at nodes, crossing volume of the meeting links", "Numeric",
  "amb_strava_250m",  "Ambient Strava",      "Strava Metro (2023), 100 m grid",         "Trips within 0-250 m (log)",                                      "Numeric",
  "amb_strava_500m",  "Ambient Strava",      "Strava Metro (2023), 100 m grid",         "Trips within 250-500 m ring (log)",                               "Numeric",
  "amb_strava_1000m", "Ambient Strava",      "Strava Metro (2023), 100 m grid",         "Trips within 500-1,000 m ring (log)",                             "Numeric",
  "amb_strava_2000m", "Ambient Strava",      "Strava Metro (2023), 100 m grid",         "Trips within 1,000-2,000 m ring (log)",                           "Numeric",
  "infra_type",       "Roadway / facility",  "OpenStreetMap",                           "Bicycle facility type",                                           "Categorical: separated path, buffered lane, bike lane, marked shared lane, quiet street, shared arterial, other",
  "functional",       "Roadway / facility",  "OpenStreetMap",                           "Road functional class",                                           "Categorical: Major, Minor, Local Road",
  "is_paved",         "Roadway / facility",  "OpenStreetMap",                           "Paved surface",                                                   "Categorical: yes, no",
  "speed_limit",      "Roadway / facility",  "OpenStreetMap",                           "Posted speed limit (mph; class default when untagged)",           "Numeric",
  "emp_density",      "Built environment",   "EPA Smart Location Database",             "Gross population density (D1B)",                                  "Numeric",
  "int_density",      "Built environment",   "EPA Smart Location Database",             "Street intersection density (D3B)",                               "Numeric",
  "walk_index",       "Built environment",   "EPA National Walkability Index",          "National Walkability Index",                                      "Numeric",
  "housing_total",    "Accessibility",       "PeopleForBikes BNA",                      "Housing units in census block",                                   "Numeric",
  "pop_low",          "Accessibility",       "PeopleForBikes BNA",                      "Population reachable, low-stress network",                        "Numeric",
  "pop_high",         "Accessibility",       "PeopleForBikes BNA",                      "Population reachable, all streets",                               "Numeric",
  "emp_low",          "Accessibility",       "PeopleForBikes BNA",                      "Jobs reachable, low-stress network",                              "Numeric",
  "emp_high",         "Accessibility",       "PeopleForBikes BNA",                      "Jobs reachable, all streets",                                     "Numeric",
  "schools_low",      "Accessibility",       "PeopleForBikes BNA",                      "K-12 schools reachable, low-stress network",                      "Numeric",
  "schools_high",     "Accessibility",       "PeopleForBikes BNA",                      "K-12 schools reachable, all streets",                             "Numeric",
  "colleges_low",     "Accessibility",       "PeopleForBikes BNA",                      "Colleges reachable, low-stress network",                          "Numeric",
  "colleges_high",    "Accessibility",       "PeopleForBikes BNA",                      "Colleges reachable, all streets",                                 "Numeric",
  "doctors_low",      "Accessibility",       "PeopleForBikes BNA",                      "Doctor offices reachable, low-stress network",                    "Numeric",
  "doctors_high",     "Accessibility",       "PeopleForBikes BNA",                      "Doctor offices reachable, all streets",                           "Numeric",
  "pharmacies_low",   "Accessibility",       "PeopleForBikes BNA",                      "Pharmacies reachable, low-stress network",                        "Numeric",
  "pharmacies_high",  "Accessibility",       "PeopleForBikes BNA",                      "Pharmacies reachable, all streets",                               "Numeric",
  "retail_low",       "Accessibility",       "PeopleForBikes BNA",                      "Retail centers reachable, low-stress network",                    "Numeric",
  "retail_high",      "Accessibility",       "PeopleForBikes BNA",                      "Retail centers reachable, all streets",                           "Numeric",
  "supermarket_low",  "Accessibility",       "PeopleForBikes BNA",                      "Supermarkets reachable, low-stress network",                      "Numeric",
  "supermarket_high", "Accessibility",       "PeopleForBikes BNA",                      "Supermarkets reachable, all streets",                             "Numeric",
  "parks_low",        "Accessibility",       "PeopleForBikes BNA",                      "Parks reachable, low-stress network",                             "Numeric",
  "parks_high",       "Accessibility",       "PeopleForBikes BNA",                      "Parks reachable, all streets",                                    "Numeric",
  "trails_low",       "Accessibility",       "PeopleForBikes BNA",                      "Trails reachable, low-stress network",                            "Numeric",
  "trails_high",      "Accessibility",       "PeopleForBikes BNA",                      "Trails reachable, all streets",                                   "Numeric",
  "community_low",    "Accessibility",       "PeopleForBikes BNA",                      "Community centers reachable, low-stress network",                 "Numeric",
  "community_high",   "Accessibility",       "PeopleForBikes BNA",                      "Community centers reachable, all streets",                        "Numeric",
  "transit_low",      "Accessibility",       "PeopleForBikes BNA",                      "Transit stations reachable, low-stress network",                  "Numeric",
  "transit_high",     "Accessibility",       "PeopleForBikes BNA",                      "Transit stations reachable, all streets",                         "Numeric",
  "precip_annual",    "Climate",             "PRISM 4 km (2023)",                       "Annual precipitation (mm)",                                       "Numeric",
  "temp_min",         "Climate",             "PRISM 4 km (2023)",                       "Mean daily minimum temperature (deg C)",                          "Numeric",
  "temp_max",         "Climate",             "PRISM 4 km (2023)",                       "Mean daily maximum temperature (deg C)",                          "Numeric"
)

# ============================================================================
# TABLE BUILDERS (return data frames)
# ============================================================================

# Tercile cut points, same definition as validate_lgb() (modeling.R).
tercile_breaks <- function(y) {
  y <- pmax(y[!is.na(y)], 0)
  stats::quantile(y, c(0, 1/3, 2/3, 1), names = FALSE)
}

classify_terciles <- function(x, q) {
  cls <- cut(x, q, include.lowest = TRUE, labels = c("low", "mid", "high"))
  cls[x > q[4]] <- "high"; cls[is.na(cls)] <- "high"
  cls
}

# Table 2: count data by mode x source.
build_count_table <- function(bike_train, ped_train) {
  one <- function(d, target, mode) {
    d <- sf::st_drop_geometry(d) %>% filter(!is.na(.data[[target]]))
    q <- tercile_breaks(d[[target]])
    d %>%
      mutate(y = pmax(.data[[target]], 0), low = y <= q[2]) %>%
      group_by(source) %>%
      summarise(
        Sites        = n_distinct(spatial_id),
        Observations = n(),
        Years        = if (min(year) == max(year)) as.character(min(year))
                       else paste0(min(year), "-", max(year)),
        `Median`     = round(median(y)),
        `IQR`        = paste0(round(quantile(y, 0.25)), "-", round(quantile(y, 0.75))),
        `% low tercile` = round(100 * mean(low)),
        .groups = "drop"
      ) %>%
      mutate(Mode = if (mode == "bike") "Bicycle (AADBT)" else "Pedestrian (AADPT)",
             Source = source_label(source, mode)) %>%
      arrange(desc(Sites)) %>%
      bind_rows(tibble(
        Mode = unique(.$Mode), Source = "Total",
        Sites = n_distinct(d$spatial_id), Observations = nrow(d),
        Years = paste0(min(d$year), "-", max(d$year)),
        Median = round(median(pmax(d[[target]], 0))),
        IQR = paste0(round(quantile(pmax(d[[target]], 0), 0.25)), "-",
                     round(quantile(pmax(d[[target]], 0), 0.75))),
        `% low tercile` = round(100 * mean(pmax(d[[target]], 0) <= q[2]))
      )) %>%
      select(Mode, Source, Sites, Observations, Years, Median, IQR, `% low tercile`)
  }
  bind_rows(one(bike_train, "aadb", "bike"), one(ped_train, "aadp", "ped"))
}

# Table 3: predictors, generated from the model's predictor sets.
build_predictor_table <- function(predictors_a = PREDICTORS_A,
                                  predictors_b = PREDICTORS_B) {
  missing <- setdiff(predictors_a, PREDICTOR_INFO$code)
  if (length(missing) > 0)
    stop("PREDICTOR_INFO has no row for: ", paste(missing, collapse = ", "))
  PREDICTOR_INFO %>%
    filter(code %in% predictors_a) %>%
    mutate(Models = if_else(code %in% predictors_b, "A, B", "A only")) %>%
    select(Category = category, `Data source` = source, Variable = variable,
           `Data type` = type, Models)
}

# Hyperparameters of the four models, straight from lgb_params().
build_hyperparam_table <- function() {
  nrounds <- eval(formals(train_lgb)$nrounds)
  combos <- expand.grid(track = c("A", "B"), target = c("aadb", "aadp"),
                        stringsAsFactors = FALSE)
  purrr::pmap_dfr(combos, function(track, target) {
    p <- lgb_params(target, track)
    tibble(
      Model = paste(if (target == "aadb") "Bicycle" else "Pedestrian", "-",
                    MODEL_LABELS[[track]]),
      `Tweedie power`       = p$tweedie_variance_power,
      `Leaves`              = p$num_leaves,
      `Min. obs. per leaf`  = p$min_data_in_leaf,
      `Feature fraction`    = p$feature_fraction,
      `Row fraction`        = p$bagging_fraction,
      `L1`                  = p$lambda_l1,
      `L2`                  = p$lambda_l2,
      `Learning rate`       = p$learning_rate,
      `Trees`               = nrounds
    )
  })
}

build_tercile_table <- function(bike_train, ped_train) {
  row <- function(d, target, mode) {
    q <- tercile_breaks(sf::st_drop_geometry(d)[[target]])
    tibble(Mode = mode,
           Low  = paste0("0-", round(q[2])),
           Medium = paste0(round(q[2]), "-", round(q[3])),
           High = paste0("> ", round(q[3])))
  }
  bind_rows(row(bike_train, "aadb", "Bicycle (AADBT)"),
            row(ped_train,  "aadp", "Pedestrian (AADPT)"))
}

# Performance of one mode's two models (Tables 4 and 6). Per-fold class accuracy
# gives the standard error used as the equivalence band in tuning.
build_performance_table <- function(val_A, val_B) {
  one <- function(v, track) {
    o <- v$oof
    q <- tercile_breaks(o$obs)
    fold_acc <- o %>%
      mutate(oc = classify_terciles(obs, q), pc = classify_terciles(pred, q)) %>%
      group_by(fold) %>%
      summarise(acc = mean(oc == pc), .groups = "drop") %>%
      pull(acc)
    mae <- v$median_abs_err
    tibble(
      Metric = c("Count locations", "Observations",
                 "Volume class accuracy", "Std. error of accuracy (across folds)",
                 "Severe misclassification (low <-> high)", "RMSE",
                 "Median absolute error, low tercile",
                 "Median absolute error, medium tercile",
                 "Median absolute error, high tercile"),
      value = c(
        format(n_distinct(o$spatial_id), big.mark = ","),
        format(nrow(o), big.mark = ","),
        sprintf("%.1f%%", 100 * v$class_accuracy),
        sprintf("%.1f%%", 100 * sd(fold_acc) / sqrt(length(fold_acc))),
        sprintf("%.1f%%", 100 * v$off_by_two),
        format(round(v$rmse), big.mark = ","),
        format(round(mae[["low"]], 1), big.mark = ","),
        format(round(mae[["mid"]]), big.mark = ","),
        format(round(mae[["high"]]), big.mark = ",")
      ),
      track = MODEL_LABELS[[track]]
    )
  }
  bind_rows(one(val_A, "A"), one(val_B, "B")) %>%
    pivot_wider(names_from = track, values_from = value)
}

# Error margins (Tables 5 and 7), same layout as the previous report tables.
build_error_margin_table <- function(val_A, val_B) {
  probs <- c(0.25, 0.50, 0.75, 0.99)
  one <- function(v, track) {
    ae <- abs(v$oof$pred - v$oof$obs)
    tibble(`% of locations predicted` = paste0(100 * probs, "%"),
           value = paste0("\u00B1", format(round(quantile(ae, probs, names = FALSE)),
                                           big.mark = ",", trim = TRUE)),
           track = MODEL_LABELS[[track]])
  }
  bind_rows(one(val_A, "A"), one(val_B, "B")) %>%
    pivot_wider(names_from = track, values_from = value)
}

# Confusion matrix: counts with row percentages.
build_confusion_table <- function(v, track) {
  cm <- v$confusion
  rp <- 100 * cm / rowSums(cm)
  out <- matrix(sprintf("%d (%.0f%%)", as.integer(cm), rp), nrow = nrow(cm))
  lv <- c(low = "Low", mid = "Medium", high = "High")
  tibble(Model = MODEL_LABELS[[track]], Observed = unname(lv[rownames(cm)])) %>%
    bind_cols(setNames(as.data.frame(out, stringsAsFactors = FALSE),
                       paste("Predicted", unname(lv[colnames(cm)]))))
}

# Gain importance, one-hot columns folded back to their predictor.
build_importance_table <- function(model_A, model_B, top_n = 15) {
  gain <- function(m, track) {
    imp <- lightgbm::lgb.importance(lgb_booster(m))
    preds <- m$predictors
    # longest matching predictor prefix (infra_type<level> -> infra_type)
    base <- vapply(imp$Feature, function(f) {
      hit <- preds[startsWith(f, preds)]
      if (length(hit) == 0) f else hit[which.max(nchar(hit))]
    }, character(1))
    tibble(code = base, gain = imp$Gain) %>%
      group_by(code) %>% summarise(gain = sum(gain), .groups = "drop") %>%
      mutate(gain = 100 * gain / sum(gain), track = track)
  }
  g <- bind_rows(gain(model_A, "A"), gain(model_B, "B"))
  keep <- g %>% group_by(code) %>% summarise(m = max(gain)) %>%
    slice_max(m, n = top_n) %>% pull(code)
  g %>%
    filter(code %in% keep) %>%
    mutate(gain = sprintf("%.1f", gain)) %>%
    pivot_wider(names_from = track, values_from = gain, values_fill = "-") %>%
    left_join(PREDICTOR_INFO %>% select(code, variable), by = "code") %>%
    mutate(ord = suppressWarnings(as.numeric(A))) %>%
    arrange(desc(ord)) %>%
    transmute(Predictor = coalesce(variable, code),
              `Track A gain (%)` = A, `Track B gain (%)` = B)
}

# ============================================================================
# FIGURES (return ggplot objects)
# ============================================================================
SOURCE_COLORS <- c("#0072B2", "#E69F00", "#009E73")   # Okabe-Ito, colorblind-safe

get_ca_outline <- function() {
  tryCatch(
    tigris::states(cb = TRUE, progress_bar = FALSE) %>%
      filter(NAME == "California") %>% st_transform(3310),
    error = function(e) { message("CA outline unavailable: ", conditionMessage(e)); NULL })
}

plot_count_sites <- function(train, mode, ca = NULL) {
  pts <- train %>%
    st_transform(3310) %>%
    mutate(geometry = st_point_on_surface(st_geometry(.))) %>%
    distinct(spatial_id, .keep_all = TRUE) %>%
    mutate(Source = source_label(source, mode))
  n_by <- count(st_drop_geometry(pts), Source)
  pts$Source <- factor(pts$Source, levels = n_by$Source[order(-n_by$n)],
                       labels = paste0(n_by$Source[order(-n_by$n)], " (n = ",
                                       format(sort(n_by$n, decreasing = TRUE), big.mark = ",", trim = TRUE), ")"))
  # draw the largest source first so smaller ones stay visible on top
  pts <- pts[order(pts$Source), ]
  ggplot() +
    { if (!is.null(ca)) geom_sf(data = ca, fill = "grey97", color = "grey40", linewidth = 0.3) } +
    geom_sf(data = pts, aes(color = Source), size = 0.9, alpha = 0.7) +
    scale_color_manual(values = SOURCE_COLORS, name = NULL) +
    coord_sf(datum = NA) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom", legend.direction = "vertical",
          panel.grid = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 2.5, alpha = 1)))
}

plot_obs_pred <- function(val_A, val_B, unit) {
  d <- bind_rows(val_A$oof %>% mutate(Model = MODEL_LABELS[["A"]]),
                 val_B$oof %>% mutate(Model = MODEL_LABELS[["B"]]))
  q <- tercile_breaks(val_A$oof$obs)[2:3]
  brks <- c(0, 10, 100, 1000, 10000)
  ggplot(d, aes(obs, pred)) +
    geom_vline(xintercept = q, linetype = "dashed", color = "grey60", linewidth = 0.3) +
    geom_hline(yintercept = q, linetype = "dashed", color = "grey60", linewidth = 0.3) +
    geom_abline(slope = 1, intercept = 0, color = "grey30", linewidth = 0.4) +
    geom_point(alpha = 0.25, size = 0.7, color = "#0072B2") +
    facet_wrap(~Model) +
    scale_x_continuous(trans = "log1p", breaks = brks, labels = scales::comma) +
    scale_y_continuous(trans = "log1p", breaks = brks, labels = scales::comma) +
    coord_equal() +
    labs(x = paste("Observed", unit), y = paste("Predicted", unit)) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank())
}

# ============================================================================
# DOCX ASSEMBLY
# ============================================================================
ft_plain <- function(df) {
  flextable(df) %>%
    theme_booktabs() %>%
    fontsize(size = 9, part = "all") %>%
    font(fontname = "Calibri", part = "all") %>%
    bold(part = "header") %>%
    valign(valign = "top", part = "body") %>%
    set_table_properties(layout = "autofit", width = 1)
}

add_caption <- function(doc, text) {
  body_add_fpar(doc, fpar(ftext(text, fp_text(bold = TRUE, font.size = 10))))
}

add_table <- function(doc, caption, df, merge_col = NULL, note = NULL) {
  ft <- ft_plain(df)
  if (!is.null(merge_col)) ft <- merge_v(ft, j = merge_col)
  doc <- add_caption(doc, caption)
  doc <- body_add_flextable(doc, ft)
  if (!is.null(note)) doc <- body_add_par(doc, note, style = "Normal")
  body_add_par(doc, "")
}

add_figure <- function(doc, caption, p, width = 6.5, height = 5) {
  doc <- body_add_gg(doc, p, width = width, height = height, res = 300)
  doc <- add_caption(doc, caption)
  body_add_par(doc, "")
}

#' Build the Section 4 outputs document.
#' @param bike_train,ped_train snapped training sets (targets `bike_train`, `ped_train`)
#' @param val named list with val_bike_A, val_bike_B, val_ped_A, val_ped_B
#' @param models optional named list with model_bike_A/B, model_ped_A/B (importance appendix)
#' @param out_path destination .docx
build_section4_docx <- function(bike_train, ped_train, val, models = NULL,
                                out_path = "data_processed/report/section4_outputs.docx") {
  ca <- get_ca_outline()
  # plain bold headings (the default template's heading styles auto-number,
  # which clashes with the report's section numbers)
  h <- function(doc, text, size = 12) {
    body_add_fpar(doc, fpar(ftext(text, fp_text(bold = TRUE, font.size = size, color = "#1F3864"))))
  }

  doc <- read_docx()
  doc <- h(doc, "Section 4 tables and figures", size = 14)
  doc <- body_add_par(doc, paste("Generated", format(Sys.Date()), "by src/report_outputs.R.",
                                 "Captions match the placeholders in README_Section4.md."))

  doc <- h(doc, "Section 4 (introduction)")
  doc <- add_table(doc, "Table 2: Count data used to train the active travel models",
                   build_count_table(bike_train, ped_train), merge_col = "Mode",
                   note = paste("Sites are unique count locations after de-duplication and",
                                "matching to the network; observations are location-years",
                                "(bicycle counts at intersections contribute one observation",
                                "per street axis). Median and IQR are daily volumes. Low",
                                "tercile is relative to all counts of the same mode."))

  doc <- h(doc, "4.1.1 AADBT from bicycle counts")
  doc <- add_figure(doc, "Figure 1: Bicycle count locations by data source",
                    plot_count_sites(bike_train, "bike", ca), height = 6.5)

  doc <- h(doc, "4.1.2 AADPT from pedestrian counts")
  doc <- add_figure(doc, "Figure 2: Pedestrian count locations by data source",
                    plot_count_sites(ped_train, "ped", ca), height = 6.5)

  doc <- h(doc, "4.2 Explanatory variables")
  doc <- add_table(doc, "Table 3: Explanatory variables used in the active travel models",
                   build_predictor_table(), merge_col = c("Category", "Data source"),
                   note = paste("Models: A = existing-network model (Track A), B = new-facility",
                                "model (Track B). Bicycle and pedestrian models use the same",
                                "variables. Low-stress and all-streets access are within the",
                                "BNA bike-shed of the census block."))

  doc <- h(doc, "4.3 Data processing and modeling procedure")
  doc <- add_table(doc, "Table 3a: Volume class (tercile) definitions",
                   build_tercile_table(bike_train, ped_train),
                   note = "Daily volume ranges. Terciles are computed from all training counts of each mode.")
  doc <- add_table(doc, "Table 3b: Selected model hyperparameters", build_hyperparam_table())

  doc <- h(doc, "4.4 Bicycle model validation")
  doc <- add_table(doc, "Table 4: Bicycle model performance (spatial 10-fold cross-validation)",
                   build_performance_table(val$val_bike_A, val$val_bike_B))
  doc <- add_table(doc, "Table 5: Error margins of predicted AADBT",
                   build_error_margin_table(val$val_bike_A, val$val_bike_B),
                   note = "Absolute error of out-of-fold predictions; e.g., 50% of locations are predicted within the 50% margin.")
  doc <- add_table(doc, "Table 5a: Bicycle volume class confusion matrices",
                   bind_rows(build_confusion_table(val$val_bike_A, "A"),
                             build_confusion_table(val$val_bike_B, "B")),
                   merge_col = "Model",
                   note = "Counts of locations with row percentages (share of each observed class).")
  doc <- add_figure(doc, "Figure 3: Observed vs predicted AADBT (out-of-fold predictions)",
                    plot_obs_pred(val$val_bike_A, val$val_bike_B, "AADBT"), height = 3.8)

  doc <- h(doc, "4.5 Pedestrian model validation")
  doc <- add_table(doc, "Table 6: Pedestrian model performance (spatial 10-fold cross-validation)",
                   build_performance_table(val$val_ped_A, val$val_ped_B))
  doc <- add_table(doc, "Table 7: Error margins of predicted AADPT",
                   build_error_margin_table(val$val_ped_A, val$val_ped_B),
                   note = "Absolute error of out-of-fold predictions.")
  doc <- add_table(doc, "Table 7a: Pedestrian volume class confusion matrices",
                   bind_rows(build_confusion_table(val$val_ped_A, "A"),
                             build_confusion_table(val$val_ped_B, "B")),
                   merge_col = "Model",
                   note = "Counts of locations with row percentages (share of each observed class).")
  doc <- add_figure(doc, "Figure 4: Observed vs predicted AADPT (out-of-fold predictions)",
                    plot_obs_pred(val$val_ped_A, val$val_ped_B, "AADPT"), height = 3.8)

  if (!is.null(models)) {
    doc <- h(doc, "Appendix: predictor importance")
    doc <- add_table(doc, "Table A1: Top predictors by share of model gain, bicycle models",
                     build_importance_table(models$model_bike_A, models$model_bike_B))
    doc <- add_table(doc, "Table A2: Top predictors by share of model gain, pedestrian models",
                     build_importance_table(models$model_ped_A, models$model_ped_B))
  }

  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  print(doc, target = out_path)
  message("Wrote ", out_path)
  invisible(out_path)
}

# ============================================================================
# RUN (interactive session after config.R)
# ============================================================================
if (interactive()) {
  build_section4_docx(
    bike_train = tar_read(bike_train),
    ped_train  = tar_read(ped_train),
    val = list(val_bike_A = tar_read(val_bike_A), val_bike_B = tar_read(val_bike_B),
               val_ped_A  = tar_read(val_ped_A),  val_ped_B  = tar_read(val_ped_B)),
    models = list(model_bike_A = tar_read(model_bike_A), model_bike_B = tar_read(model_bike_B),
                  model_ped_A  = tar_read(model_ped_A),  model_ped_B  = tar_read(model_ped_B))
  )
}
