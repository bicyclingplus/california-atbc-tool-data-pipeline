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
2. Run `config.R` → `setwd()` and loads the function library.

## Running the pipeline

Run with R 4.2.3 (the install with `targets` 1.11.4 + the model packages).
Run `config.R` → `setwd()` and loads the function library.
Run `_targets.R` target by target or `tar_make()` for full pipeline
Approximately 6 hours on a Ryzen 7 1800X 8-core, 64GB RAM machine

## Simplified pipeline map

Raw counts + Strava + context layers → enriched statewide network → ambient Strava demand field → LightGBM volume models → predicted bike/ped volumes → web-tool outputs.

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
| `src/functions/export.R` | Web-tool exports (Appendix A, context blocks, `export_models_for_node`) |
| `src/convert_to_onnx.py` | One-time LightGBM → ONNX conversion for `onnxruntime-node` (run outside the pipeline) |
