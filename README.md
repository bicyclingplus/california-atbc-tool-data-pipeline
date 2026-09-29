# caltrans-bc-tool-data-pipeline

Data pipeline for all background data and models that support the Caltrans
Active Transportation Benefit–Cost (ATBC) Tool. Built use R package
[`targets`](https://books.ropensci.org/targets/).

## Repository layout: code here, data on cloud drive

This git repository holds **only the code** (`src/`). The **data and the
`targets` cache live on a private cloud share, available on request**:

```
[Cloud Root PATH]     <- run the pipeline from HERE
├── data_raw/         <- pipeline inputs
├── data_processed/   <- pipeline outputs (web-tool assets)
├── _targets/         <- the targets cache
└── _targets.yaml     <- points `script` at this repo's _targets.R on a local drive
```

1. Cloud's `_targets.yaml` → `script:` points at this repo's `src/_targets.R`.
2. Update and Run `config.R` → `setwd()` and loads the function library.

## Running the pipeline

Run with R 4.2.3 (the install with `targets` 1.11.4 + the model packages).
Update and Run `config.R` → `setwd()` and loads the function library.
Run `_targets.R` target by target or `tar_make()` for full pipeline
Approximately 6 hours on a Ryzen 7 1800X 8-core, 64GB RAM machine

## Simplified pipeline map

Raw counts + Strava + context layers → enriched statewide network → ambient Strava demand field → LightGBM volume models → predicted bike/ped volumes → web-tool outputs.

## Data vintages

**Crash data (SWITRS via TIMS): 2019–2023** (`data_raw/switrs_2019_2023/`, extract
2024-09-12), used for the Appendix A safety tables. A newer 2020–2025 extract
(`data_raw/SWITRS_PEDBIKE_2020-2025_20260318/`) is on the drive but deliberately
**not** used:

- **Matches the exposure data.** Strava Metro volumes are 2023 and the count data
  are mostly 2018–2023, so 2019–2023 crashes line up with the years the volume
  estimates represent.
- **Avoids provisional data.** The most recent SWITRS year is incomplete when
  extracted: 2025 in the newer file is about 9% below 2024, and recent years grew
  by about 3% between extracts as late reports arrived.

Revisit when the exposure data (Strava, counts) move to later years.

## Modeling bike and pedestrian demand

See standalone README_modeleing.md for details

### Function files

| File | Responsibility |
|---|---|
| `src/functions/network_utils.R` | Strava loader, OSM download/reclass, topology builder, link↔node volume mapping, web-network finalization |
| `src/functions/enrichment.R` | SWITRS/weather loaders, base/crash/census enrichment, feature math, count snapping, web-block enrichment |
| `src/functions/process_counts.R` | UCB + Caltrans + CAT Portal count loaders; HOD + seasonality AADT expansion |
| `src/functions/ambient.R` | Ambient Strava raster (`build_strava_grid` → `.tif`, `extract_ambient` lookup) |
| `src/functions/modeling.R` | LightGBM Tweedie train / predict / spatial-CV validate; predictor sets; network prediction |
| `src/functions/export.R` | Web-tool exports (Appendix A; context blocks with precomputed Track B new-path volumes) |
