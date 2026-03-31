library(tidyverse)
library(lubridate)

#' Load UCB Bike Data (Gold Standard)
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

#' Load UCB Ped Data (Gold Standard)
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

#' Process & Expand Caltrans Counts (Internal Control Method)
#' Uses the Caltrans data itself to derive expansion factors.
process_caltrans_counts <- function(file_list, mode) {
  if (length(file_list) == 0) return(tibble())
  
  # 1. Read and Clean Data
  raw <- map_dfr(file_list, read_csv, show_col_types = FALSE) %>%
    # RENAME UP FRONT to simplify grouping logic later
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
    # PEDS: Remove Direction from ID (so they aggregate)
    raw <- raw %>%
      mutate(spatial_id = paste0("loc_", round(lat, 6), "_", round(lon, 6)))
  }
  
  # 2. Define Grouping Columns Dynamically
  # Base columns always needed
  group_cols <- c("spatial_id", "lat", "lon", "year", "month", "date")
  
  # Conditionally add direction if mode is bike
  if (mode == "bike") group_cols <- c(group_cols, "direction")
  
  # Calculate Daily Totals
  daily_vols <- raw %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(
      daily_vol = sum(count, na.rm = TRUE),
      hours_recorded = n(), 
      .groups = "drop"
    )
  
  # 3. Calculate Monthly Averages (MADT)
  monthly_avgs <- daily_vols %>%
    group_by(spatial_id, year, month) %>%
    summarize(madt = mean(daily_vol), .groups = "drop")
  
  # 4. Identify Control Stations
  site_month_counts <- monthly_avgs %>% count(spatial_id)
  control_sites <- site_month_counts %>% filter(n == 12) %>% pull(spatial_id)
  
  if(length(control_sites) == 0) {
    control_sites <- site_month_counts %>% filter(n >= 9) %>% pull(spatial_id)
  }
  
  # 5. Calculate Seasonality
  seasonality_curve <- monthly_avgs %>%
    filter(spatial_id %in% control_sites) %>%
    group_by(spatial_id) %>%
    mutate(annual_avg = mean(madt)) %>%
    group_by(month) %>%
    summarize(factor = mean(madt / annual_avg), .groups = "drop")
  
  all_months <- tibble(month = 1:12)
  seasonality_final <- all_months %>%
    left_join(seasonality_curve, by = "month") %>%
    mutate(factor = replace_na(factor, 1.0))
  
  # 6. Expand Counts (AADT)
  # Take group_cols and remove the time components
  location_cols <- setdiff(group_cols, c("year", "month", "date"))
  
  final_aadt <- monthly_avgs %>%
    left_join(seasonality_final, by = "month") %>%
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

