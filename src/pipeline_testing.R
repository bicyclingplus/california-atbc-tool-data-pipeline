library(targets)
library(tidyverse)
library(sf)
library(ranger)

# ==============================================================================
# PHASE 1: FILE DISCOVERY & STRAVA LOADER (All Checked)
# ==============================================================================

#--- TEST 1.1: LOCATING FILES ----
tar_make(names = strava_zip_files)
files <- tar_read(strava_zip_files)
print(files)
#Notes: 12 files found. pass

#--- TEST 1.2: LOADING STRAVA GEOMETRY ----
# This extracts zips and aggregates counts. Approx 3 mins.
tar_make(names = strava_base)
strava_raw <- tar_read(strava_base)
print(head(strava_raw))

# Verification
vol_check <- summary(strava_raw$strava_vol_total)
print(vol_check)
sum(strava_raw$strava_vol_total > 0)

# Visualize Davis, CA
# (Coordinates: xmin, ymin, xmax, ymax)
davis_bbox <- st_bbox(c(xmin = -121.80, xmax = -121.68, 
                        ymin = 38.52, ymax = 38.60), 
                      crs = st_crs(strava_raw))
davis_poly <- st_as_sfc(davis_bbox)
# Filter raw data to Davis box (turning off spherical geometry for speeding up)
sf::sf_use_s2(FALSE)
davis_strava <- strava_raw[davis_poly, ]
sf::sf_use_s2(TRUE)

library(sf)
ggplot(data = davis_strava) +
  # Color lines by volume, make empty roads thinner
  geom_sf(aes(color = strava_vol_total, linewidth = strava_vol_total)) +
  scale_color_viridis_c(option = "magma", trans = "log1p") + # Log scale highlights contrast
  scale_linewidth(range = c(0.2, 1.5), guide = "none") +      # Thicker lines for higher traffic
  theme_minimal() +
  labs(title = "Strava Volume: Davis, CA", color = "Volume")

# Notes: map looks good

# ==============================================================================
# PHASE 2: OSM & NETWORK MERGE (All Checked)
# ==============================================================================

#--- Test 2.1: DOWNLOADING OSM REFERENCE ----
# NOTE: If this takes too long, ensure _targets.R has region = "Alameda County"
tar_make(names = osm_reference)

osm <- tar_read(osm_reference)
print(table(osm$highway))

# NOTES: Works!

# Check tags
osm_reclass <- process_osm_tags(osm)
print(table(osm_reclass$highway, osm_reclass$infra_type))
print(table(osm_reclass$infra_type, osm_reclass$is_paved))
table(osm_reclass$speed_limi)

osm_reclass %>% 
  filter(infra_type == "separated_path") %>% 
  head(10) %>% 
  print()

# Looks good. Removing highway and bicycle from output

#--- Test2.2: CREATING MASTER NETWORK ----
tar_make(names = master_network)

net <- tar_read(master_network)

# Verification: Did OSM attributes join to Strava lines?
missing_fac <- sum(is.na(net$facility_type))
total_rows <- nrow(net)
pct_matched <- round(100 * (1 - missing_fac/total_rows), 1)

# NOTES: NOT COMPLETED

# ==============================================================================
# PHASE 3: COUNT PROCESSING (All Checked)
# ==============================================================================

#--- Test 3.1: PROCESSING UCB bike and ped ----
tar_make(names = ucb_bike_clean)
ucb_bike <- tar_read(ucb_bike_clean)
print(head(ucb_bike))
print(summary(ucb_bike$aadt))
plot(dens(ucb_bike$aadt))
library(ggplot2)
ggplot(ucb_bike, aes(x = aadt)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  scale_x_log10() +
  labs(title = "Distribution of UCB Bike AADT", x = "Daily Volume (Log Scale)")
plot(ucb_bike$lon, ucb_bike$lat, pch=20, col=rgb(0,0,1,0.5), 
     main="UCB Bike Locations", xlab="Lon", ylab="Lat")
maps::map("state", region = "california", add = TRUE, col = "gray")


tar_make(names = ucb_ped_clean)
ucb_ped <- tar_read(ucb_ped_clean)
print(head(ucb_ped))
print(summary(ucb_ped$aadt))
plot(density(ucb_ped$aadt))
library(ggplot2)
ggplot(ucb_ped, aes(x = aadt)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  scale_x_log10() +
  labs(title = "Distribution of UCB ped AADT", x = "Daily Volume (Log Scale)")
plot(ucb_ped$lon, ucb_ped$lat, pch=20, col=rgb(0,0,1,0.5), 
     main="UCB ped Locations", xlab="Lon", ylab="Lat")
maps::map("state", region = "california", add = TRUE, col = "gray")

# NOTES: Looks good!

#--- Test 3.2: PROCESSING CALTRANS Bike COUNTS ----
tar_make(names = caltrans_bike_clean)

final_aadt <- tar_read(caltrans_bike_clean)

# 2. Load the Raw Data (The Input)
# We need to manually grab the files again to see the underlying variation
raw_files <- list.files("data_raw/atd", pattern = "caltrans_bicycle.*\\.csv", full.names = TRUE)

message("Reading raw files using exact column names...")

raw_daily_history <- map_dfr(raw_files, function(f) {
  # Read file
  d <- read_csv(f, show_col_types = FALSE) %>%
    rename_with(tolower)
  
  # Return empty if columns are missing (sanity check)
  if(!all(c("loc_id", "date", "count") %in% names(d))) return(NULL)
  
  d %>%
    mutate(
      # Parse the 'date' column. Try mdy or ymd formats.
      date_obj = lubridate::parse_date_time(date, orders = c("mdy", "ymd", "mdy HMS", "ymd HMS")),
      date_clean = as.Date(date_obj),
      
      # Rename 'loc_id' to 'site_id' to match your final dataset
      site_id = as.character(loc_id)
    ) %>%
    # Sum up all counts (e.g. 15-min intervals) to get ONE number per DAY
    group_by(site_id, date_clean) %>%
    summarize(daily_sum = sum(count, na.rm = TRUE), .groups = "drop")
})

# 3. Filter: Only plot sites that exist in our final dataset
valid_sites <- unique(final_aadt$site_id)
plot_data <- raw_daily_history %>% filter(site_id %in% valid_sites)

raw_means <- plot_data %>%
  group_by(site_id) %>%
  summarize(raw_mean = mean(daily_sum, na.rm = TRUE))

# 3. PLOT: Reality vs. Raw Mean vs. Expanded AADT
ggplot() +
  # A. The Reality (Grey Cloud of Daily Points)
  geom_jitter(data = plot_data, aes(x = daily_sum, y = site_id), 
              height = 0.1, alpha = 0.3, size = 0.8, color = "gray70") +
  
  # B. The Naive Raw Average (Black Bar)
  geom_point(data = raw_means, aes(x = raw_mean, y = site_id), 
             color = "black", shape = "|", size = 9, stroke = 2) +
  
  # C. The Smart Expanded AADT (Red Bar)
  geom_point(data = final_aadt, aes(x = aadt, y = site_id), 
             color = "red", shape = "|", size = 9, stroke = 2) +
  
  # Formatting
  scale_x_log10() + 
  labs(
    title = "Impact of Expansion: Raw Average vs. AADT",
    subtitle = "Grey = Daily Obs  |  Black = Raw Average  |  Red = Expanded AADT",
    x = "Daily Volume (Log Scale)",
    y = "Site ID"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_line(color = "gray90")
  )

# NOTES: Looks good

#--- Test 3.3: PROCESSING CALTRANS Ped COUNTS ----
# 1. Load the Final Pedestrian AADT (The Red Bar)
# Builds the target first to ensure it's up to date
tar_make(names = "caltrans_ped_clean")
final_aadt_ped <- tar_read(caltrans_ped_clean)

# 2. Load the Raw Pedestrian Data (The Grey Cloud)
# Note the changed pattern to find pedestrian files
raw_files_ped <- list.files("data_raw/atd", pattern = "caltrans_pedestrian.*\\.csv", full.names = TRUE)

message("Reading raw PEDESTRIAN files...")

raw_daily_ped <- map_dfr(raw_files_ped, function(f) {
  # Read file
  d <- read_csv(f, show_col_types = FALSE) %>%
    rename_with(tolower)
  
  # Basic Validation: Ensure columns exist
  if(!all(c("loc_id", "date", "count") %in% names(d))) return(NULL)
  
  d %>%
    mutate(
      # Parse the 'date' column (Robust to common formats)
      date_obj = lubridate::parse_date_time(date, orders = c("mdy", "ymd", "mdy HMS", "ymd HMS")),
      date_clean = as.Date(date_obj),
      
      # Standardize ID
      site_id = as.character(loc_id)
    ) %>%
    # Sum up counts to get Daily Totals
    group_by(site_id, date_clean) %>%
    summarize(daily_sum = sum(count, na.rm = TRUE), .groups = "drop")
})

# 3. Filter: Only plot sites that exist in our final dataset
valid_sites_ped <- unique(final_aadt_ped$site_id)
plot_data_ped <- raw_daily_ped %>% filter(site_id %in% valid_sites_ped)

# 4. Calculate the "Naive" Raw Average (The Black Bar)
raw_means_ped <- plot_data_ped %>%
  group_by(site_id) %>%
  summarize(raw_mean = mean(daily_sum, na.rm = TRUE))

# 5. PLOT: Pedestrian Reality vs. Expansion
message("Generating Pedestrian Validation Plot...")

ggplot() +
  # A. The Reality (Grey Cloud of Daily Points)
  geom_jitter(data = plot_data_ped, aes(x = daily_sum, y = site_id), 
              height = 0.1, alpha = 0.3, size = 0.8, color = "gray70") +
  
  # B. The Naive Raw Average (Black Bar)
  geom_point(data = raw_means_ped, aes(x = raw_mean, y = site_id), 
             color = "black", shape = "|", size = 9, stroke = 2) +
  
  # C. The Smart Expanded AADT (Red Bar)
  geom_point(data = final_aadt_ped, aes(x = aadt, y = site_id), 
             color = "red", shape = "|", size = 9, stroke = 2) +
  
  # Formatting
  scale_x_log10() + 
  labs(
    title = "Pedestrian Expansion Validation: Raw vs. AADT",
    subtitle = "Grey = Daily Obs  |  Black = Raw Average  |  Red = Expanded AADT",
    x = "Daily Pedestrian Volume (Log Scale)",
    y = "Site ID"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_line(color = "gray90")
  )

# Notes: Looks good

#--- Test 3.4: Outliers
library(dplyr)
library(stringr)

#' Find Suspicious Caltrans Data Points
#' 
inspect_volume_outliers <- function(data, threshold = 2500) {
  
  suspicious_data <- data %>%
    filter(aadt > threshold) %>%
    arrange(desc(aadt)) %>%
    select(spatial_id, aadt, source, everything()) # Put key info first
  
  return(suspicious_data)
}
# ==============================================================================
# PHASE 4: ENRICHMENT
# ==============================================================================

#--- Test 4.1: Weather Station Discovery ----

library(httr)
library(jsonlite)

# 1. Setup Parameters
# Note: sid "SFO" often needs a type suffix (e.g., "SFO 3") if using 'sid'
test_id <- "SFO" 
year <- 2023

# 2. Build according to official StnData JSON specs
payload <- list(
  sid = test_id,
  sdate = as.character(year),
  edate = as.character(year),
  elems = list(
    list(
      name = "pcpn",
      interval = list(1),  # [1] in JSON
      duration = 1,        # 1 year
      reduce = "sum"       # total for that period
    )
  )
)

# 3. POST the request
# We use auto_unbox=TRUE to ensure [1] stays as an array
res <- POST(
  url = "https://data.rcc-acis.org/StnData",
  body = toJSON(payload, auto_unbox = TRUE),
  add_headers("Content-Type" = "application/json")
)

# Data comes back in a nested list: out$data[[row]][[col]]
print(paste("Precip (inches):", out$data[[2]]))

#NOTES: Passed...finally!

#--- Test 4.2: Test Weather Data Read ----
message("\n--- STEP 4.2: WEATHER DATA FETCH ---")
tar_make(weather_data)
w_df <- tar_read(weather_data)

print(head(w_df))

# Notes: Passed

#--- Test 4.3: Test Spatial Join----
message("\n--- STEP 4.3: SPATIAL JOIN INTEGRITY ---")
# Verify that the nearest-neighbor join is assigning precip values to road links
tar_make(enriched_bike_network)
net_enriched <- tar_read(enriched_bike_network)

# Check for precipitation and expansion indices
check_vars <- c("prcp_annua", "WWI", "week_PHI")
missing <- setdiff(check_vars, names(net_enriched))

if (length(missing) == 0) {
  message("✅ PASS: Network enriched with weather and Berkeley indices.")
  print(summary(st_drop_geometry(net_enriched[, check_vars])))
} else {
  stop("❌ FAIL: Missing columns in enriched network: ", paste(missing, collapse=", "))
}

#--- Test 4.4: Test Spatial relationship of strava and weather----
test_strava <- tar_read(master_network) %>% head(50) %>% st_transform(3310)

# Find nearest station index
nearest_idx <- st_nearest_feature(test_strava, st_transform(weather, 3310))

# Pull the precipitation values
precip_test <- weather$prcp_annua[nearest_idx]

message("Weather Join Test:")
print(summary(precip_test))

# ==============================================================================
# PHASE 2.5: Test all the joins
# ==============================================================================

# 1. Load the pieces
test_data <- tar_read(master_network) %>% head(100)
test_strava <- st_as_sf(test_data, geometry = test_data$geometry, crs = 4326) %>% st_transform(3310)
sld_sf     <- tar_read(sld_data)      # Smart Location Database
weather_sf <- tar_read(weather_data)  # Weather/Precipitation data
crash_sf   <- tar_read(processed_crash) # Crash/Collision data
wi_sf      <- tar_read(wi_data)   # Walk Index Shapefile
# 2. Check Math Typo (Fixing 'total_vols' here too)
test_leisure <- test_strava %>%
  mutate(recr_prop = if_else(strava_vol_total > 0, strava_leisure / strava_vol_total, 0),
         recr_prop = pmin(recr_prop, 1)) 
max(test_leisure$recr_prop)

# 3. Test Weather Join
print("Testing Weather...")
weather_proj <- st_transform(weather_sf, 3310)
nearest_w_idx <- st_nearest_feature(test_strava, weather_proj)
test_strava$prcp_annua <- weather_proj$prcp_annua[nearest_w_idx]

# 4. Test Polygon Joins (SLD & Walk Index)
print("Testing SLD/Walk Index...")
test_centroids <- test_strava %>% 
  st_centroid() %>%
  st_join(st_transform(sld_sf, 3310), join = st_intersects) %>%
  st_join(st_transform(wi_sf, 3310), join = st_intersects) %>%
  st_drop_geometry()

# --- CRITICAL CHECK ---
# Check if the column names D1B, D3b, and NatWalkInd actually exist
print("Available columns in Join:")
print(intersect(c("D1B", "D3b", "NatWalkInd"), colnames(test_centroids)))

# 5. Test Crash Buffer (30m)
print("Testing Crash Buffers...")
crash_counts <- test_strava %>%
  st_buffer(dist = 30) %>%
  st_join(st_transform(crash_sf, 3310)) %>%
  group_by(edge_uid) %>%
  summarize(crash_count_30m = sum(!is.na(CASE_ID)), .groups = "drop") %>%
  st_drop_geometry()

# 6. Final Assembly
print("Testing Final Math (WWI/PHI)...")
final_test <- test_strava %>%
  left_join(test_centroids %>% select(edge_uid, any_of(c("D1B", "D3b", "NatWalkInd"))), by = "edge_uid") %>%
  left_join(crash_counts, by = "edge_uid") %>%
  mutate(
    crash_count_30m = replace_na(crash_count_30m, 0),
    WWI = 0.55134 - 0.04631 * log10(pmax(D1B, 1)), # Simplified check
    week_PHI = 3.97844 + 0.000085 * D1B 
  )

print("Test Complete! Head of final result:")
head(final_test)


#--- Test 4.2: Link-Level Enrichment Verification ----

# buffer overlap check
final_net_test <- final_net %>%
  mutate(edge_length = st_length(.))

cor(as.numeric(final_net_test$edge_length), final_net_test$crash_count_30m, use = "complete.obs")

# strava infra check
final_net %>%
  st_drop_geometry() %>%
  group_by(infra_type.x) %>%
  summarise(
    avg_vol = mean(strava_vol_total, na.rm = TRUE),
    max_vol = max(strava_vol_total, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(avg_vol))

# index correlation check
indices_subset <- final_net %>%
  st_drop_geometry() %>%
  select(WWI, week_PHI, NatWalkInd) %>%
  drop_na()

cor(indices_subset)

# exposure bin check
final_net %>%
  st_drop_geometry() %>%
  # Create 5 groups based on Strava volume (Quintiles)
  mutate(vol_bin = ntile(strava_vol_total, 5)) %>%
  group_by(vol_bin) %>%
  summarise(
    avg_crashes = mean(crash_count_30m, na.rm = TRUE),
    total_segments = n()
  )

# another infra check
final_net %>%
  st_drop_geometry() %>%
  group_by(infra_type.x) %>%
  summarise(
    avg_vol = mean(strava_vol_total),
    crash_rate = sum(crash_count_30m) / sum(strava_vol_total) * 1000
  ) %>%
  arrange(desc(avg_vol))

library(sf)
library(plotly)
library(tidyverse)

# 1. Prepare the data for plotting
# We sample 5,000 segments to keep the interaction smooth
plot_data <- final_net %>%
  slice_sample(n = 5000) %>%
  st_transform(4326) %>%
  mutate(
    status = ifelse(is.na(D1B), "Missing Census Data", "Enriched"),
    # Create a nice hover label
    label = paste0(
      "Status: ", status, "<br>",
      "Infra: ", infra_type.x, "<br>",
      "Strava Vol: ", round(strava_vol_total, 0)
    )
  )

# 2. Extract coordinates for Plotly
# (Plotly needs explicit Lon/Lat columns for its map markers)
coords <- st_coordinates(st_centroid(plot_data))
plot_data$lon <- coords[,1]
plot_data$lat <- coords[,2]

# 3. Generate the Interactive Map
plot_ly(plot_data) %>%
  add_trace(
    type = "scattermapbox",
    lon = ~lon,
    lat = ~lat,
    color = ~status,
    colors = c("Enriched" = "forestgreen", "Missing Census Data" = "red"),
    text = ~label,
    hoverinfo = "text",
    marker = list(size = 6)
  ) %>%
  layout(
    mapbox = list(
      style = "open-street-map", # Provides the GIS reference background
      zoom = 5,
      center = list(lon = -119.4, lat = 36.7)
    ),
    margin = list(l = 0, r = 0, t = 30, b = 0),
    title = "GIS Diagnostic: Identifying Enrichment Gaps"
  )

# Check 3: Spatial Weather Check
# Ensure Redding (RDD) area has higher precip than LAX area
rdd_sample <- final_net %>% 
  st_filter(st_buffer(st_transform(w_stations[w_stations$station_id=="RDD",], 3310), 50000))
lax_sample <- final_net %>% 
  st_filter(st_buffer(st_transform(w_stations[w_stations$station_id=="LAX",], 3310), 50000))

message(sprintf("Avg Precip Redding Area: %.1fmm", mean(rdd_sample$prcp_annua, na.rm=TRUE)))
message(sprintf("Avg Precip LA Area: %.1fmm", mean(lax_sample$prcp_annua, na.rm=TRUE)))

#--- Test 4.3: Training Data Assembly (The Final CSV Join) ----
tar_make(names = bike_train_data)
train_v4 <- tar_read(bike_train_data)

# Critical Check: Are there any NAs in the required bike_features?
# Random Forest will fail if these are NA
bike_features <- c("D1B", "D1C", "D1D", "WWI", "PHI", "prcp_annua", "strv_total")
na_counts <- colSums(is.na(st_drop_geometry(train_v4[, bike_features])))
print("Missing values in features:")
print(na_counts)

if(any(na_counts > 0)) {
  message("[!] Warning: Some training sites failed to grab SLD or Weather data.")
}


# ==============================================================================
# PHASE 5: Data Enrichment MODEL ASSEMBLY & TRAINING
# ==============================================================================

message("\n--- STEP 6: ENRICHING MODEL DATA ---")
# This joins Points (counts) to Lines (network) + Census + Crash
tar_make(names = bike_model_data)

train_data <- tar_read(bike_model_data)

# Check for NAs in predictors (Critical for Random Forest)
na_check <- colSums(is.na(st_drop_geometry(train_data)))
print(na_check[na_check > 0])

if(nrow(train_data) > 0) {
  message("✅ PASS: Training data assembled.")
} else {
  stop("❌ FAIL: Training data is empty (Spatial join might have failed).")
}

message("\n--- STEP 7: TRAINING MODEL ---")
tar_make(names = fit_bike)

model <- tar_read(fit_bike)
print(model)

if("ranger" %in% class(model)) {
  message("✅ PASS: Random Forest model trained successfully.")
}

# ==============================================================================
# PHASE 5: testing strava 2023 against past strava for simple modeling
# ==============================================================================

load_ucb_bike_wstrava <- function(file_path) {
  read_csv(file_path, show_col_types = FALSE) %>%
    filter(!is.na(Lat), !is.na(Long)) %>%
    mutate(
      spatial_id = paste0("loc_", round(Lat, 6), "_", round(Long, 6)),
      source = "UCB_GoldStandard",
      mode = "bike",
      aadt = as.numeric(AADB)
    ) %>%
    # Select and rename legacy Strava columns
    rename(
      f_trip_old = forward_trip_count,
      r_trip_old = reverse_trip_count,
      f_commute_old = forward_commute_trip_count,
      r_commute_old = reverse_commute_trip_count,
      f_leisure_old = forward_leisure_trip_count,
      r_leisure_old = reverse_leisure_trip_count
    ) %>%
    # Calculate a legacy total to compare against your new 'strava_vol_total'
    mutate(total_strava_old = f_trip_old + r_trip_old) %>%
    select(spatial_id, lat = Lat, lon = Long, aadt, mode, source, 
           total_strava_old, f_commute_old, r_commute_old, f_leisure_old, r_leisure_old)
}

compare_strava_detailed <- function(links_train, ucb_bike_legacy, master_network) {
  
  # 1. Get Commute/Leisure splits from Master Network
  # We use the columns that actually exist: strava_commute, strava_leisure
  network_features <- master_network %>%
    st_drop_geometry() %>%
    select(edge_uid, strava_commute, strava_leisure)
  
  # 2. Prepare the comparison dataframe
  comparison_df <- links_train %>%
    st_drop_geometry() %>%
    filter(source == "UCB_GoldStandard") %>%
    # Get the link between spatial_id (UCB ID) and edge_uid (Network ID)
    distinct(spatial_id, edge_uid, strava_vol_total) %>%
    # Attach the split data
    left_join(network_features, by = "edge_uid") %>%
    # Join the OLD legacy data
    inner_join(ucb_bike_legacy, by = "spatial_id") %>%
    mutate(
      # NEW Data Metrics (Already aggregated in master_network)
      commute_share_2023 = strava_commute / strava_vol_total,
      
      # OLD Data Metrics (Need to sum forward + reverse)
      commute_total_old = f_commute_old + r_commute_old,
      commute_share_old = commute_total_old / total_strava_old
    ) %>%
    filter(!is.na(strava_vol_total), !is.na(total_strava_old))
  
  # 3. Correlations
  vol_cor     <- cor(comparison_df$strava_vol_total, comparison_df$total_strava_old)
  commute_cor <- cor(comparison_df$commute_share_2023, comparison_df$commute_share_old, use = "complete.obs")
  
  # 4. Plot: Commute Share Comparison
  library(ggplot2)
  p <- ggplot(comparison_df, aes(x = commute_share_old, y = commute_share_2023)) +
    geom_point(aes(size = strava_vol_total), alpha = 0.5, color = "purple") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      title = "Commute Share Stability: Legacy vs. 2023",
      subtitle = paste0("Pearson R: ", round(commute_cor, 3), " | Point size = Volume"),
      x = "Legacy Commute %",
      y = "2023 Commute %"
    ) +
    theme_minimal()
  
  message(paste0("Volume Correlation: ", round(vol_cor, 3)))
  message(paste0("Commute Share Correlation: ", round(commute_cor, 3)))
  
  return(list(
    data = comparison_df,
    correlations = list(volume = vol_cor, commute = commute_cor),
    plot = p
  ))
}

ucb_bike_legacy <- load_ucb_bike_wstrava("data_raw/ucb_bike_AADB/Model_clean_data_july23_AADBT.csv")
partitioned_data <- tar_read(partitioned_data)
links_train <- partitioned_data[["links_train"]]
master_network <- tar_read(master_network)
compare_strava_detailed(links_train, ucb_bike_legacy, master_network)
# ==============================================================================
# PHASE 6: FINAL DATA PREP TESTING
# ==============================================================================

check_data_health <- function(train_set, predict_set, target_col) {
  
  message("--- 1. CHECKING MISSING VALUES (NAs) ---")
  # Random Forest cannot handle NAs in predictors
  na_counts <- colSums(is.na(st_drop_geometry(train_set)))
  bad_cols <- na_counts[na_counts > 0]
  
  if(length(bad_cols) > 0) {
    print(bad_cols)
    warning("Found NAs in the above columns! You must fill these (likely with 0) before training.")
  } else {
    message("✔ No NAs found in training predictors.")
  }
  
  message("\n--- 2. CHECKING TARGET DISTRIBUTION ---")
  target_vals <- train_set[[target_col]]
  message(paste("Min:", min(target_vals), "| Max:", max(target_vals), "| Median:", median(target_vals)))
  
  if(min(target_vals) < 0) warning("Target variable has negative values! Impossible for counts.")
  if(max(target_vals) == 0) warning("Target variable is all zeros!")
  
  message("\n--- 3. CHECKING FACTOR CONSISTENCY ---")
  # Check if Prediction set has factor levels (e.g., "Bridge") that Training set never saw
  factor_cols <- names(select(st_drop_geometry(train_set), where(is.factor)))
  
  for(col in factor_cols) {
    train_levels <- levels(train_set[[col]])
    pred_levels  <- levels(predict_set[[col]])
    
    new_levels <- setdiff(pred_levels, train_levels)
    
    if(length(new_levels) > 0) {
      warning(paste("Column", col, "has levels in PREDICT set not found in TRAIN set:", paste(new_levels, collapse=", ")))
    } else {
      message(paste("✔", col, "levels match."))
    }
  }
}

p_data <- tar_read(partitioned_data)
check_data_health(p_data$links_train, p_data$links_predict, "aadb")
check_data_health(p_data$nodes_train, p_data$nodes_predict, "aadp")

# ==============================================================================
# PHASE 7: Test Model Building
# ==============================================================================
library(corrplot)
library(tidyverse)
library(sf)

# 1. Clean the data for correlation
# - Drop geometry (it's not a predictor)
# - Drop IDs (they are noise)
# - Select the target (aadb) and predictors
cor_data <- bike_train %>%
  st_drop_geometry() %>%
  select(
    aadb,                # Keep target to see what predicts it!
    starts_with("infra"), # Grab infra_type
    is_paved,             # Grab is_paved
    everything()          # Grab the rest
  ) %>%
  select(-edge_uid, -spatial_id, -source, -from, -to, -year) %>% # Remove IDs
  na.omit() # Correlation cannot handle NAs

# 2. Convert Factors to Dummy Variables (One-Hot Encoding)
# The formula "~ . - 1" means "use all columns, but don't add an Intercept column"
model_mat <- model.matrix(~ . - 1, data = cor_data)

# Check what happened (you will see new columns like 'infra_typeseparated_path')
head(colnames(model_mat))
# 3. Calculate Correlation Matrix
M <- cor(model_mat, use = "pairwise.complete.obs")

# 4. Plot
# method = "color": makes it easier to read than circles for dense matrices
# type = "upper": only shows the top half (since bottom is duplicate)
# tl.cex = 0.6: shrinks text size so labels fit
corrplot(M, 
         method = "color", 
         type = "upper", 
         order = "hclust",   # Groups correlated variables together
         tl.col = "black", 
         tl.cex = 0.6,
         addCoef.col = NULL, # Turn this on if you want numbers (might be messy)
         title = "Predictor Correlations (Factors One-Hot Encoded)",
         mar = c(0,0,1,0))

# 2. Flatten the matrix into a paired list
# logic: filter(Var1 < Var2) removes duplicates (A-B vs B-A) and self-correlations (A-A)
flattened_cor <- M %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation") %>%
  filter(Var1 < Var2) %>%
  arrange(desc(Correlation)) # Optional: Sort highest to lowest

# 4. Print correlations above 0.8
cat("\n--- Correlations above 0.8 ---\n")
print(flattened_cor %>% filter(Correlation > 0.8))


# Testing hGLM work------------------------------
library(targets)
library(tidyverse)
library(sf)
library(ggplot2)
library(patchwork) # For combining plots easily, install if needed: install.packages("patchwork")

# 1. Load the data
# We'll use the enriched bike data as our test case
hglm_bike <- tar_read(hglm_data_bike)

# ============================================================================
# CHART 1: The Impact of the Log Transformation (Histograms)
# ============================================================================
# Let's look at the raw distribution vs the log1p distribution

p1 <- ggplot(hglm_bike, aes(x = crash_density)) +
  geom_histogram(bins = 50, fill = "darkred", color = "black") +
  theme_minimal() +
  labs(title = "Raw Crash Density", 
       subtitle = "Notice the extreme right skew",
       x = "Raw Crash Density", y = "Count of Segments")

p2 <- ggplot(hglm_bike, aes(x = log1p(crash_density))) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Log1p(Crash Density)", 
       subtitle = "Much better for GLM modeling",
       x = "Log1p(Crash Density)", y = "Count of Segments")

# Display side-by-side
p1 + p2

# ============================================================================
# CHART 2: Do high crash areas correlate with high bike volumes? (Scatter)
# ============================================================================
# If these outliers are real, they should generally correlate with areas 
# that have higher bike volumes (more bikes = more exposure = more crashes).

ggplot(hglm_bike, aes(x = log1p(crash_density), y = log1p(aadb))) +
  geom_point(alpha = 0.4, color = "navy") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Bike Volume vs. Crash Density",
       subtitle = "Does crash density track with ridership?",
       x = "Log1p(Crash Density)", 
       y = "Log1p(AADB)")

# ============================================================================
# CHART 3: Where are the top 1% of outliers? (Spatial Map)
# ============================================================================
# If the outliers are correct, they should be tightly clustered in 
# dense urban cores (San Francisco, Downtown LA, Berkeley, etc.).

library(leaflet)
library(sf)
library(dplyr)
library(targets)
library(purrr)

# 1. Load the Base Data
context_map <- tar_read(web_context_map)
raw_crashes <- tar_read(processed_crash)
all_links_list <- tar_read(strava_base) # The FULL road network (comes as a list of chunks)

# 2. Define Thresholds & Sample Polygons (5 of each to prevent map freezing)
top_1_percent <- quantile(context_map$crash_density, 0.99)
mod_lower <- quantile(context_map$crash_density, 0.50)
mod_upper <- quantile(context_map$crash_density, 0.75)

set.seed(123) # For reproducibility

# Grab 5 random EXTREME polygons
extreme_polys <- context_map %>%
  filter(crash_density > top_1_percent) %>%
  sample_n(5) 

# Grab 5 random MODERATE polygons
moderate_polys <- context_map %>%
  filter(crash_density > 0 & crash_density < top_1_percent) %>%
  sample_n(5)

# Transform to 3310 for accurate spatial filtering
ext_poly_3310 <- st_transform(extreme_polys, 3310)
mod_poly_3310 <- st_transform(moderate_polys, 3310)

# 3. Safely filter the FULL network links to only those inside our 10 polygons
# We map over the list of strava chunks to keep memory usage low
get_links_in_polys <- function(chunk, polys) {
  chunk_3310 <- st_transform(chunk, 3310)
  st_filter(chunk_3310, polys) # Keep links that intersect the polygon
}

extreme_links <- map_dfr(all_links_list, ~get_links_in_polys(.x, ext_poly_3310)) %>% st_transform(4326)
moderate_links <- map_dfr(all_links_list, ~get_links_in_polys(.x, mod_poly_3310)) %>% st_transform(4326)

# 4. Filter the Crashes to only those inside our 10 polygons
crashes_3310 <- st_transform(raw_crashes, 3310)

extreme_crashes <- st_filter(crashes_3310, ext_poly_3310) %>% st_transform(4326)
moderate_crashes <- st_filter(crashes_3310, mod_poly_3310) %>% st_transform(4326)

# 5. Transform polygons for Leaflet
extreme_polys_wgs84 <- st_transform(extreme_polys, 4326)
moderate_polys_wgs84 <- st_transform(moderate_polys, 4326)

# 6. Build the Comparative Interactive Map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # ==========================================
# LAYER 1: EXTREME (Red / Black)
# ==========================================
addPolygons(
  data = extreme_polys_wgs84,
  color = "red", weight = 2, fillOpacity = 0.1,
  popup = ~paste("<b>EXTREME BLOCK</b><br>Crash Density:", round(crash_density, 1)),
  group = "Extreme Risk"
) %>%
  addPolylines(
    data = extreme_links,
    color = "darkred", weight = 3, opacity = 0.8,
    popup = "Extreme Network Link",
    group = "Extreme Risk"
  ) %>%
  addCircleMarkers(
    data = extreme_crashes,
    radius = 3, color = "black", stroke = FALSE, fillOpacity = 0.8,
    group = "Extreme Risk"
  ) %>%
  
  # ==========================================
# LAYER 2: MODERATE (Blue / Gray)
# ==========================================
addPolygons(
  data = moderate_polys_wgs84,
  color = "blue", weight = 2, fillOpacity = 0.1,
  popup = ~paste("<b>MODERATE BLOCK</b><br>Crash Density:", round(crash_density, 1)),
  group = "Moderate Risk"
) %>%
  addPolylines(
    data = moderate_links,
    color = "darkblue", weight = 3, opacity = 0.8,
    popup = "Moderate Network Link",
    group = "Moderate Risk"
  ) %>%
  addCircleMarkers(
    data = moderate_crashes,
    radius = 3, color = "gray", stroke = FALSE, fillOpacity = 0.8,
    group = "Moderate Risk"
  ) %>%
  
  # Add Toggle Controls
  addLayersControl(
    overlayGroups = c("Extreme Risk", "Moderate Risk"),
    options = layersControlOptions(collapsed = FALSE)
  )



# ==============================================================================
# PHASE 8: FINAL PREDICTION
# ==============================================================================

library(targets)
library(dplyr)
library(sf)

# 1. Load the predictions
cat("Loading final_predictions target...\n")
final_predictions <- tar_read(final_predictions)

# ==============================================================================
# TEST 1: BIKE VOLUMES (LINKS)
# ==============================================================================
links <- final_predictions$links
n_links <- nrow(links)

# Check for NAs
na_bike <- sum(is.na(links$pred_bike_vol))

# Check for Negatives
neg_bike <- sum(links$pred_bike_vol < 0, na.rm = TRUE)

# Summary Stats
summary(links$pred_bike_vol)


# ==============================================================================
# TEST 2: PEDESTRIAN VOLUMES (NODES)
# ==============================================================================
nodes <- final_predictions$nodes
n_nodes <- nrow(nodes)

# Check for NAs
na_ped <- sum(is.na(nodes$pred_ped_vol))

# Check for Negatives
neg_ped <- sum(nodes$pred_ped_vol < 0, na.rm = TRUE)

# Summary Stats
summary(nodes$pred_ped_vol)

# max = 40K is quite large.
# 2. Extract Top 10 Pedestrian Nodes
top_10_peds <- nodes %>%
  arrange(desc(pred_ped_vol)) %>%
  # Select ID, Prediction, and Key Drivers to sanity check
  select(
    node_id, 
    pred_ped_vol, 
    strava_vol_total, 
    transit_high,     # Is it near a major transit stop?
    emp_density,      # Is it in a job center?
    walk_index,       # Is it a walkable area?
    schools_high      # Is it near a university?
  ) %>%
  head(10)

# 3. Print as a clean table (dropping geometry for readability)
print(top_10_peds %>% st_drop_geometry())

# map them
library(leaflet)
library(sf)

# Ensure the data has valid coordinates (WGS84 is best for web maps)
map_data <- st_transform(top_10_peds, 4326)

leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% # Clean background map
  addCircleMarkers(
    radius = 8,
    color = "red",
    fillOpacity = 0.8,
    # Click a dot to see the Volume and ID
    popup = ~paste0(
      "<b>Node ID:</b> ", node_id, "<br>",
      "<b>Ped Volume:</b> ", round(pred_ped_vol, 0), "<br>",
      "<b>Emp Density:</b> ", round(emp_density, 1)
    )
  )
# top 10 all on or near Market street. 

# ==============================================================================
# PHASE 9: HGLM testing
# ==============================================================================
df_ped <- tar_read(ped_train)

# Let's look at the counts for the categorical columns
message("--- infra_type ---")
table(df_ped$infra_type, useNA = "always")

message("--- is_paved ---")
table(df_ped$is_paved, useNA = "always")

message("--- speed_limit ---")
table(df_ped$speed_limit, useNA = "always")

full_peek <- st_read("data_raw/census/blocks_optimized.gpkg", 
                     layer = "neighborhood_census_blocks_CA_full",
                     as_tibble = TRUE,
                     stringsAsFactors = FALSE,
                     quiet = TRUE,
                     # Using a dummy query to force it into a non-spatial return
                     query = "SELECT * FROM neighborhood_census_blocks_CA_full LIMIT 1000")
# ==============================================================================
# PHASE 10: Data export
# ==============================================================================

tar_load(final_web_network)
links <- final_web_network$links

#Check NAs
table(links$functional, useNA = "always")
table(links$bicycle_exposure_class, useNA = "always")
table(links$pedestrian_exposure_class, useNA = "always")



library(readr)
library(dplyr)

# Read the generated CSV
app_a_links <- read_csv("/data_processed/appendix_a_links.csv")

# 1. Check the distribution of Alphas
# (Expect mostly negative numbers, roughly between -3 and -12)
summary(app_a_links$`α Crash`)

# 2. Make sure there are no infinite values or NAs hiding in the math
sum(is.na(app_a_links$`α Crash`))
sum(is.infinite(app_a_links$`α Crash`))

# 3. Verify all permutations exist (You should see rows for every combination)
table(app_a_links$Mode, app_a_links$`Exposure Class`)
