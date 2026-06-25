# Tests for src/functions/process_counts.R -- count loaders + AADT expansion.

test_that("load_ucb_bike maps columns and aadt", {
  csv <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(Lat = 38.5, Long = -121.7, AADB = 100, year = 2023), csv)
  out <- load_ucb_bike(csv)
  expect_equal(out$aadt, 100)
  expect_equal(out$mode, "bike")
  expect_equal(out$source, "UCB_GoldStandard")
  expect_true(all(c("spatial_id", "lat", "lon", "aadt", "mode", "source", "year") %in% names(out)))
})

test_that("load_ucb_ped divides annual estimate by 365", {
  csv <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(ID = "p1", Latitude = 37.8, Longitude = -122.3,
                              AnnualEst = 36500), csv)
  out <- load_ucb_ped(csv)
  expect_equal(out$aadt, 100)          # 36500 / 365
  expect_equal(out$mode, "ped")
  expect_equal(out$year, 2018)
})

test_that("load_ucb_ped drops rows with missing coords or aadt", {
  csv <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(
    ID = c("a", "b"),
    Latitude = c(37.8, NA), Longitude = c(-122.3, -122.4),
    AnnualEst = c(365, 730)), csv)
  out <- load_ucb_ped(csv)
  expect_equal(nrow(out), 1)
  expect_equal(out$aadt, 1)
})

test_that("process_caltrans_counts expands a flat-seasonality site to its daily mean", {
  # One site, counted one day per month all 12 months, 50 trips/day every time.
  # With no seasonal variation the expansion factor is 1, so AADT == 50.
  dates <- as.Date(paste0("2023-", sprintf("%02d", 1:12), "-15"))
  raw <- data.frame(
    latitude = 38.5, longitude = -121.7,
    date = as.character(dates),
    direction = "Northbound",
    count = 50, mode = "bike"
  )
  f <- tempfile(fileext = ".csv"); readr::write_csv(raw, f)
  out <- process_caltrans_counts(f, mode = "bike")
  expect_equal(nrow(out), 1)
  expect_equal(round(out$aadt), 50)
  expect_equal(out$source, "Caltrans_InternalExp")
})

test_that("load_catportal_counts HOD-expands a 2-hour count to a full day", {
  # Build a metadata + one PERMANENT (24h) site and one 2-hour HUMAN-OBS site.
  # The permanent site defines the HOD profile; the 2-hour site must be inflated.
  dir <- tempfile(); dir.create(dir)
  meta <- data.frame(
    Filename = c("perm.csv.gz", "short.csv.gz"),
    Year = 2023, `Agency ID` = c(1, 2), Agency = c("P", "S"),
    Mode = 2, `Mode Label` = "Bicycle",
    Method = c(3, 1),
    `Method Label` = c("Permanent automated counter", "Human Observation"),
    check.names = FALSE
  )
  readr::write_csv(meta, file.path(dir, "counts_zip_metadata.csv"))

  # Permanent site: 24 hourly rows, 10/hr, over many days -> defines HOD shape (flat).
  perm_rows <- do.call(rbind, lapply(1:20, function(d) {
    data.frame(
      interval_start = sprintf("1/%d/2023, %d:00:00 %s", d, ((0:23) %% 12) + 1,
                               ifelse(0:23 < 12, "AM", "PM")),
      interval_length = 60, volume = 10,
      latitude = 38.0, longitude = -121.0, bearing_dir = "North",
      location_type = "Trail"
    )
  }))
  readr::write_csv(perm_rows, gzfile(file.path(dir, "perm.csv.gz")))

  # Short site: a single day, 2 hours only (hours 8 and 9), 10/hr = 20 observed.
  short_rows <- data.frame(
    interval_start = c("6/1/2023, 8:00:00 AM", "6/1/2023, 9:00:00 AM"),
    interval_length = 60, volume = 10,
    latitude = 39.0, longitude = -122.0, bearing_dir = "North",
    location_type = "Trail"
  )
  readr::write_csv(short_rows, gzfile(file.path(dir, "short.csv.gz")))

  out <- load_catportal_counts(dir, ucb_sites = NULL)
  short <- out[out$lat == 39.0, ]
  expect_equal(nrow(short), 1)
  # The 2-hour count (20 trips) must be inflated well above 20 toward a full day.
  # With a ~flat 24h profile, 2 of 24 hours ~ 1/12 of the day -> ~240.
  expect_gt(short$aadt, 20)
  expect_true("weight" %in% names(out))
  expect_true("location_type" %in% names(out))
})

test_that("load_catportal_counts de-duplicates sites near existing UCB sites", {
  dir <- tempfile(); dir.create(dir)
  meta <- data.frame(
    Filename = "s.csv.gz", Year = 2023, `Agency ID` = 1, Agency = "S",
    Mode = 1, `Mode Label` = "Pedestrian", Method = 1,
    `Method Label` = "Human Observation", check.names = FALSE)
  readr::write_csv(meta, file.path(dir, "counts_zip_metadata.csv"))
  rows <- data.frame(
    interval_start = "6/1/2023, 8:00:00 AM", interval_length = 60, volume = 5,
    latitude = 38.0, longitude = -121.0, bearing_dir = NA, location_type = "Mid-block")
  readr::write_csv(rows, gzfile(file.path(dir, "s.csv.gz")))

  # A UCB site at the exact same coordinate -> the CAT site should be dropped.
  ucb <- data.frame(lat = 38.0, lon = -121.0)
  out <- load_catportal_counts(dir, ucb_sites = ucb, dedup_dist_m = 30)
  expect_equal(nrow(out), 0)
})
