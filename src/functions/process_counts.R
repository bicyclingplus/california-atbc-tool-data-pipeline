library(tidyverse)
library(lubridate)

#' Load UCB Bike Data (Miah et al. 2024b)
load_ucb_bike <- function(file_path) {
  read_csv(file_path, show_col_types = FALSE) %>%
    mutate(
      spatial_id = paste0("loc_", round(Lat, 6), "_", round(Long, 6)), # needed because raw data didn't have unique id
      source = "UCB_GoldStandard",
      mode = "bike",
      aadt = as.numeric(AADB)
    ) %>%
    select(spatial_id, lat = Lat, lon = Long, aadt, mode, source, year)
}

#' Load UCB Ped Data (Griswold et al. 2019)
load_ucb_ped <- function(file_path) {
  read_csv(file_path, show_col_types = FALSE) %>%
    select(
      spatial_id = ID,
      lat = Latitude,
      lon = Longitude,
      aadt = AnnualEst 
    ) %>%
    mutate(
      spatial_id = as.character(spatial_id),
      source = "UCB_GoldStandard",
      mode = "ped",
      year = 2018, # Fill all ped data with report year 2018 for merge with bike data
      aadt = as.numeric(aadt) / 365 
    ) %>%
    filter(!is.na(aadt), !is.na(lat), !is.na(lon))
}

#' Process & Expand Caltrans Counts (https://data.ca.gov/dataset/at-count-dataset)
#' Internal Control Method: Uses the Caltrans data itself to derive expansion factors.
process_caltrans_counts <- function(file_list, mode) {
  if (length(file_list) == 0) return(tibble())
  
  # Read and Clean Data
  # reads the list of csvs, stacks into dataframe
  # data files are mode specific
  raw <- map_dfr(file_list, read_csv, show_col_types = FALSE) %>%
    rename(lat = latitude, lon = longitude) %>%
    mutate(
      date = ymd(str_sub(date, 1, 10)),
      year = year(date),
      month = month(date)
    )
  
  # --- LOGIC BRANCH: Handle ID creation based on Mode ---
  if (mode == "bike") {
    # BIKES: Keep Direction in ID
    raw <- raw %>%
      mutate(spatial_id = paste0("loc_", round(lat, 6), "_", round(lon, 6), "_", direction))
  } else {
    # PEDS: Remove Direction from ID (so they aggregate to crossing volumes)
    raw <- raw %>%
      mutate(spatial_id = paste0("loc_", round(lat, 6), "_", round(lon, 6)))
  }
  
  # Define Grouping Columns
  # Base columns always needed
  group_cols <- c("spatial_id", "lat", "lon", "year", "month", "date")
  
  # If mode is bike, add direction
  if (mode == "bike") group_cols <- c(group_cols, "direction")
  
  # Calculate Daily Totals
  daily_vols <- raw %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(
      daily_vol = sum(count, na.rm = TRUE),
      hours_recorded = n(), 
      .groups = "drop"
    )
  
  # Calculate Monthly Averages (MADT)
  monthly_avgs <- daily_vols %>%
    group_by(spatial_id, year, month) %>%
    summarize(madt = mean(daily_vol), .groups = "drop")
  
  # Assign climate cluster to each site.
  # Inland (Central Valley) sites share a seasonality curve anchored by the
  # Sacramento-area sites, which have better coverage than sparse Fresno sites.
  # Clusters identified by visual inspection of site map:
  #   inland = Sacramento foothills (~38.7-38.9, -120.8 to -121.1)
  #            + San Joaquin valley (~36.2-36.5, -119.3 to -119.5)
  #   coast  = everything else (Bay Area, SLO, LA/OC, San Bernardino)
  site_coords <- daily_vols %>%
    distinct(spatial_id, lat, lon) %>%
    mutate(cluster = if_else(
      (lat > 38.5 & lon > -121.2 & lon < -120.7) |   # Sacramento foothill sites
      (lat > 36.0 & lat < 36.6 & lon > -119.6),      # San Joaquin valley sites
      "inland", "coast"
    ))

  # Identify control stations per cluster: all sites with >= 9 months of data.
  site_month_counts <- monthly_avgs %>%
    count(spatial_id) %>%
    left_join(site_coords %>% select(spatial_id, cluster), by = "spatial_id")

  control_coast  <- site_month_counts %>% filter(cluster == "coast",  n >= 9) %>% pull(spatial_id)
  control_inland <- site_month_counts %>% filter(cluster == "inland", n >= 9) %>% pull(spatial_id)

  message("Control sites (>=9 months) -- coast: ", length(control_coast),
          ", inland: ", length(control_inland))

  # Calculate Seasonality (one curve per cluster)
  #
  # Sites with <12 months have a biased mean: Correct this by...
  #
  # 1: build a preliminary curve from 12-month sites only (unbiased anchor).
  # 2: for each <12-month site, use the preliminary curve to impute missing
  #         months, recompute a corrected annual mean, then derive monthly factors.
  # All sites (12-month and corrected <12-month) are then averaged per month.
  make_curve <- function(control_ids) {
    data <- monthly_avgs %>% filter(spatial_id %in% control_ids)

    # 1: unbiased preliminary curve from 12-month sites
    full_sites <- site_month_counts %>%
      filter(spatial_id %in% control_ids, n == 12) %>%
      pull(spatial_id)

    prelim_curve <- data %>%
      filter(spatial_id %in% full_sites) %>%
      group_by(spatial_id) %>%
      mutate(site_mean = mean(madt)) %>%
      ungroup() %>%
      group_by(month) %>%
      summarize(rel_index = mean(madt / site_mean), .groups = "drop")

    # 2: correct annual mean for <12-month sites using imputed missing months
    all_months_tbl <- tibble(month = 1:12)

    corrected <- data %>%
      group_by(spatial_id) %>%
      group_modify(function(site_data, key) {
        present <- site_data$month
        missing <- setdiff(1:12, present)

        if (length(missing) == 0) {
          # Full site: use actual mean
          return(mutate(site_data, corrected_mean = mean(madt)))
        }

        # Impute missing months: scale prelim curve to this site's observed mean
        obs_mean   <- mean(site_data$madt)
        obs_index  <- mean(prelim_curve$rel_index[prelim_curve$month %in% present])
        site_scale <- obs_mean / obs_index

        imputed_mean <- mean(c(
          site_data$madt,
          prelim_curve$rel_index[prelim_curve$month %in% missing] * site_scale
        ))

        mutate(site_data, corrected_mean = imputed_mean)
      }) %>%
      ungroup()

    # Median relative index across all control sites per month (robust to outlier sites)
    corrected %>%
      group_by(month) %>%
      summarize(factor = median(madt / corrected_mean), .groups = "drop")
  }

  curve_coast  <- make_curve(control_coast)
  curve_inland <- make_curve(control_inland)

  # Join each site to its cluster curve; fall back to 1.0 (no adjustment) for missing months
  all_months <- tibble(month = 1:12)
  seasonality_final <- site_coords %>%
    select(spatial_id, cluster) %>%
    crossing(all_months) %>%
    left_join(curve_coast  %>% rename(factor_coast  = factor), by = "month") %>%
    left_join(curve_inland %>% rename(factor_inland = factor), by = "month") %>%
    mutate(
      factor = case_when(
        cluster == "inland" ~ replace_na(factor_inland, 1.0),
        TRUE                ~ replace_na(factor_coast,  1.0)
      )
    ) %>%
    select(spatial_id, month, factor)
  
  # Expand Counts (AADT)
  # remove the time components from group_cols
  location_cols <- setdiff(group_cols, c("year", "month", "date"))
  
  # Scale all monthly ADT by seasonality (month) factor and average to AADT
  final_aadt <- monthly_avgs %>%
    left_join(seasonality_final, by = c("spatial_id", "month")) %>%
    mutate(estimated_annual_vol = madt / factor) %>%
    group_by(spatial_id, year) %>%
    summarize(aadt = mean(estimated_annual_vol, na.rm=TRUE), .groups = "drop") %>%
    left_join(
      daily_vols %>% 
        select(all_of(location_cols)) %>% 
        distinct(),
      by = "spatial_id"
    ) %>%
    mutate(source = "Caltrans_InternalExp", mode = mode) %>%
    filter(!is.na(aadt))

  return(final_aadt)
}

#' Load & Expand CAT Data Portal Counts (catdataportal.berkeley.edu export)
#'
#' Reads the gzipped per-(year/agency/mode) CSVs, decodes them via the
#' counts_zip_metadata.csv manifest, expands hourly-interval volumes to an annual
#' average (AADT) using the same internal-seasonality method as
#' process_caltrans_counts (no external/bike-only factor files, no Strava
#' dependency), de-duplicates against the existing UCB count sites, and emits the
#' pipeline count schema plus a `location_type` flag (Trail / Mid-block /
#' Intersection / Historical) that Track B uses to identify off-street paths.
#'
#' @param catdp_dir   Folder containing the *.csv.gz files + counts_zip_metadata.csv
#' @param ucb_sites   Optional data frame of existing sites with `lat`,`lon`
#'                    (e.g. bind_rows of ucb_bike_clean/ucb_ped_clean) used to drop
#'                    CAT sites within `dedup_dist_m` of an existing UCB site.
#' @param dedup_dist_m Distance threshold (meters, CRS 3310) for treating a CAT
#'                    site as a duplicate of a UCB site. Default 30 m.
load_catportal_counts <- function(catdp_dir, ucb_sites = NULL, dedup_dist_m = 30) {
  require(dplyr); require(readr); require(lubridate); require(purrr)

  meta_path <- file.path(catdp_dir, "counts_zip_metadata.csv")
  if (!file.exists(meta_path)) stop("CAT metadata not found: ", meta_path)
  meta <- read_csv(meta_path, show_col_types = FALSE)

  # Only bike (Mode 2) and pedestrian (Mode 1); map to pipeline mode labels.
  meta <- meta %>%
    filter(Mode %in% c(1, 2)) %>%
    mutate(mode = if_else(Mode == 1, "ped", "bike"))

  # --- Read all selected files, keeping only the needed columns ----------
  read_one <- function(fn, mode_label) {
    p <- file.path(catdp_dir, fn)
    if (!file.exists(p)) return(NULL)
    d <- tryCatch(
      read_csv(p, show_col_types = FALSE, progress = FALSE,
               col_types = cols_only(
                 interval_start  = col_character(),
                 interval_length = col_double(),
                 volume          = col_double(),
                 latitude        = col_double(),
                 longitude       = col_double(),
                 bearing_dir     = col_character(),
                 location_type   = col_character(),
                 agency_name     = col_character())),
      error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d$mode <- mode_label
    d$file <- fn
    d
  }
  raw <- map2_dfr(meta$Filename, meta$mode, read_one)
  if (nrow(raw) == 0) return(tibble())

  # Drop CAT's copy of the Caltrans continuous counters. CAT ingested a 2023 copy
  # of Caltrans' permanent counters (agency_name == "Caltrans"), identical leg
  # volumes but with inverted direction labels. Those sites already enter the
  # pipeline via caltrans_*_clean (authoritative, full-year), so keeping the CAT
  # copy double-counts ~30 sites. Caltrans is kept; the CAT copies are dropped
  # here at the row level. Best guess is CATDP has the ecocounter data, and Caltrans
  # data blends ecocounter with computer vision data...need to ask Mintu and Julia.
  if ("agency_name" %in% names(raw)) {
    n0 <- nrow(raw)
    raw <- raw %>% filter(!agency_name %in% "Caltrans")
    message(sprintf("...CAT Portal: dropped %d Caltrans-agency rows (dup of caltrans_*_clean)",
                    n0 - nrow(raw)))
    if (nrow(raw) == 0) return(tibble())
  }

  # Tag each row with its collection method (permanent vs human-observation).
  raw <- raw %>%
    left_join(meta %>% select(file = Filename, method = `Method Label`), by = "file")

  # --- Parse timestamps -----------------------------------------------------
  # interval_start looks like "1/1/2023, 12:00:00 AM"
  raw <- raw %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(volume)) %>%
    mutate(
      ts      = mdy_hms(interval_start, quiet = TRUE),
      date    = as_date(ts),
      year    = year(ts),
      month   = month(ts),
      hour    = hour(ts),
      daytype = if_else(wday(ts) %in% c(1, 7), "weekend", "weekday")
    ) %>%
    filter(!is.na(date))

  # Site ID: bikes keep direction (bearing_dir) like the Caltrans loader; peds aggregate.
  raw <- raw %>%
    mutate(
      direction  = bearing_dir,
      spatial_id = if_else(
        mode == "bike",
        paste0("loc_", round(latitude, 6), "_", round(longitude, 6), "_", coalesce(direction, "NA")),
        paste0("loc_", round(latitude, 6), "_", round(longitude, 6))
      )
    )

  # --- Hour-of-day (HOD) expansion of PARTIAL-DAY counts -------------------
  # Most CATDP "Human Observation" counts cover only ~2 hours/day (NBPD peak counts),
  # Empirical HOD profile from CATDP's own PERMANENT counters (full 24 h coverage,
  # no Strava dependency) and inflate each partial site-day by the summed HOD
  # fraction of the hours it actually observed. Validated on held-out permanent
  # counters: ~unbiased (median error -4% bike / -16% ped), MAPE ~76%/49% per count.
  # HOD profile derived ONLY from permanent counters (full 24 h coverage).
  hod_profile <- raw %>%
    filter(method == "Permanent automated counter", !is.na(hour)) %>%
    group_by(mode, daytype, hour) %>%
    summarize(v = sum(volume, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(hod_frac = v / sum(v)) %>%
    ungroup() %>%
    select(mode, daytype, hour, hod_frac)

  # Coverage fraction observed per site-day = sum of HOD fractions for its hours.
  site_day_cov <- raw %>%
    distinct(spatial_id, mode, daytype, date, hour) %>%
    left_join(hod_profile, by = c("mode", "daytype", "hour")) %>%
    group_by(spatial_id, mode, date) %>%
    summarize(cov_frac = sum(hod_frac, na.rm = TRUE), n_hours = n(), .groups = "drop") %>%
    mutate(cov_frac = pmax(cov_frac, 0.02))   # floor to avoid extreme inflation

  # Carry the dominant location_type per site (for the Track B off-street path flag).
  site_loc_type <- raw %>%
    count(spatial_id, location_type) %>%
    group_by(spatial_id) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(spatial_id, location_type)

  group_cols <- c("spatial_id", "latitude", "longitude", "mode", "year", "month", "date")

  # Daily totals = observed-hours volume inflated to a full day by its coverage.
  daily_vols <- raw %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(obs_vol = sum(volume, na.rm = TRUE),
              hours_recorded = n(), .groups = "drop") %>%
    left_join(site_day_cov %>% select(spatial_id, mode, date, cov_frac),
              by = c("spatial_id", "mode", "date")) %>%
    mutate(daily_vol = obs_vol / cov_frac)   # HOD-expanded full-day estimate

  # --- Internal-seasonality expansion (mirrors process_caltrans_counts) ----
  monthly_avgs <- daily_vols %>%
    group_by(spatial_id, mode, year, month) %>%
    summarize(madt = mean(daily_vol), .groups = "drop")

  # NorCal / SoCal split at 36 degrees N.
  # The LA cluster (lat < 36) has almost no multi-month coverage so it gets its
  # own curve rather than being anchored by norcal sites.
  site_coords_cat <- daily_vols %>%
    distinct(spatial_id, latitude, longitude) %>%
    mutate(cluster = if_else(latitude >= 36, "norcal", "socal"))

  # Build bias-corrected, median-aggregated seasonality curve per (mode, cluster).
  # Mirrors process_caltrans_counts: 12-month sites anchor a preliminary curve used
  # to impute missing months for <12-month sites, correcting their annual mean bias.
  # Final factor = median relative index across all >=9-month control sites per month.
  make_cat_curve <- function(df, control_ids, full_site_ids) {
    prelim_curve <- df %>%
      filter(spatial_id %in% full_site_ids) %>%
      group_by(spatial_id) %>% mutate(site_mean = mean(madt)) %>% ungroup() %>%
      group_by(month) %>%
      summarize(rel_index = mean(madt / site_mean), .groups = "drop")

    df %>%
      filter(spatial_id %in% control_ids) %>%
      group_by(spatial_id) %>%
      group_modify(function(site_data, key) {
        present <- site_data$month
        missing <- setdiff(1:12, present)
        if (length(missing) == 0) return(mutate(site_data, corrected_mean = mean(madt)))
        obs_mean   <- mean(site_data$madt)
        obs_index  <- mean(prelim_curve$rel_index[prelim_curve$month %in% present])
        site_scale <- obs_mean / obs_index
        imputed_mean <- mean(c(
          site_data$madt,
          prelim_curve$rel_index[prelim_curve$month %in% missing] * site_scale
        ))
        mutate(site_data, corrected_mean = imputed_mean)
      }) %>%
      ungroup() %>%
      group_by(month) %>%
      summarize(factor = median(madt / corrected_mean), .groups = "drop")
  }

  curves <- monthly_avgs %>%
    left_join(site_coords_cat %>% select(spatial_id, cluster), by = "spatial_id") %>%
    group_by(mode, cluster) %>%
    group_modify(function(df, key) {
      smc <- df %>% count(spatial_id)
      full_ids    <- smc %>% filter(n == 12) %>% pull(spatial_id)
      control_ids <- smc %>% filter(n >= 9)  %>% pull(spatial_id)
      message("CAT control sites (>=9mo) -- mode: ", key$mode,
              ", ", key$cluster, ": ", length(control_ids))
      make_cat_curve(df, control_ids, full_ids)
    }) %>%
    ungroup()

  # Build per-site seasonality by joining each site to its (mode, cluster) curve
  seasonality_cat <- site_coords_cat %>%
    select(spatial_id, cluster) %>%
    crossing(tibble(month = 1:12)) %>%
    left_join(
      monthly_avgs %>% distinct(spatial_id, mode),
      by = "spatial_id"
    ) %>%
    left_join(curves, by = c("mode", "cluster", "month")) %>%
    mutate(factor = replace_na(factor, 1.0)) %>%
    select(spatial_id, mode, month, factor)

  # Keep direction with the location key so bike counts can use directed snapping
  # (peds have direction = NA, which the snapper treats as undirected).
  site_dir <- raw %>%
    filter(mode == "bike") %>%
    distinct(spatial_id, direction)

  location_cols <- c("spatial_id", "latitude", "longitude", "mode")

  # --- Per-site uncertainty -> inverse-variance weight --------------------
  # Two error sources: (a) within-day HOD-expansion noise (large for 2 h counts;
  # MAPE ~0.76 bike / 0.49 ped from the validation), shrinking as more of the day
  # is observed; (b) across-day sampling, shrinking with n_days. Combine into a
  # relative SE (CV), then weight ~ 1/CV^2 so well-measured sites dominate without 
  # a handful dominating.
  hod_mape <- c(bike = 0.76, ped = 0.49)
  site_quality <- daily_vols %>%
    group_by(spatial_id, mode) %>%
    summarize(
      n_days       = n_distinct(date),
      mean_cov     = mean(cov_frac, na.rm = TRUE),   # ~1 = full day, ~0.14 = 2 h peak
      .groups = "drop"
    ) %>%
    mutate(
      # within-day relative error scales with how little of the day was seen
      within_cv = hod_mape[mode] * (1 - pmin(mean_cov, 1)),
      # across-day error ~ 1/sqrt(n_days)
      across_cv = 1 / sqrt(pmax(n_days, 1)),
      cv        = pmax(sqrt(within_cv^2 + across_cv^2), 0.04), # assumed a floor 4% error
      weight_raw = 1 / cv^2
    )

  final_aadt <- monthly_avgs %>%
    left_join(seasonality_cat, by = c("spatial_id", "mode", "month")) %>%
    mutate(estimated_annual_vol = madt / factor) %>%
    group_by(spatial_id, mode, year) %>%
    summarize(aadt = mean(estimated_annual_vol, na.rm = TRUE), .groups = "drop") %>%
    left_join(distinct(daily_vols[location_cols]), by = c("spatial_id", "mode")) %>%
    left_join(site_loc_type, by = "spatial_id") %>%
    left_join(site_dir, by = "spatial_id") %>%
    left_join(site_quality %>% select(spatial_id, mode, n_days, cv, weight_raw),
              by = c("spatial_id", "mode")) %>%
    filter(!is.na(aadt)) %>%
    transmute(
      spatial_id,
      lat = latitude, lon = longitude,
      aadt, mode,
      source = "CAT_Portal",
      year,
      direction,
      location_type,
      n_days,
      aadt_cv = cv,
      weight  = weight_raw
    )

  # --- De-duplicate against existing UCB sites ----------------------------
  if (!is.null(ucb_sites) && nrow(final_aadt) > 0 &&
      all(c("lat", "lon") %in% names(ucb_sites))) {
    require(sf)
    cat_sf <- st_as_sf(final_aadt, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
      st_transform(3310)
    ucb_clean <- ucb_sites %>% filter(!is.na(lat), !is.na(lon)) %>% distinct(lat, lon)
    ucb_sf <- st_as_sf(ucb_clean, coords = c("lon", "lat"), crs = 4326) %>% st_transform(3310)

    nn_idx  <- st_nearest_feature(cat_sf, ucb_sf)
    nn_dist <- as.numeric(st_distance(cat_sf, ucb_sf[nn_idx, ], by_element = TRUE))
    keep <- nn_dist > dedup_dist_m

    message(sprintf("...CAT Portal: dropped %d/%d sites within %dm of a UCB site",
                    sum(!keep), length(keep), dedup_dist_m))
    final_aadt <- final_aadt[keep, ]
  }

  return(final_aadt)
}