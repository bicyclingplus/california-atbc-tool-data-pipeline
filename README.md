# caltrans-bc-tool-data-pipeline

Data pipeline for all background data and models that support the Caltrans
Active Transportation Benefit–Cost (ATBC) Tool. Built on
[`targets`](https://books.ropensci.org/targets/).

## Repository layout: code here, data on Box

This git repository holds **only the code** (`src/`). The **data and the
`targets` cache live on cloud share (Box)**:

```
C:\Users\Dillon\Box\_Projects\Caltrans_BC2   <- run the pipeline from HERE
├── data_raw/         <- pipeline inputs
├── data_processed/   <- pipeline outputs (web-tool assets)
├── _targets/         <- the targets cache
└── _targets.yaml     <- points `script` at this repo's _targets.R
```

### How code and data are kept separate

The pipeline is run with the **Cloud project root as the working directory**, so
relative paths (`data_raw/...`, `data_processed/...`) and the `_targets` store
resolve to Box. The **code** is pulled from this local repo via two absolute
pointers:

1. Cloud's `_targets.yaml` → `script:` points at this repo's `src/_targets.R`.
2. Run `config.R` → `setwd()` and loads the function library.

## Running the pipeline

Run with **R 4.2.3** (the install with `targets` 1.11.4 + the model packages).
Run `config.R` → `setwd()` and loads the function library.
Run `_targets.R` target by target or `tar_make()` for full pipeline
---

## Pipeline map

Raw counts + Strava + context layers → enriched statewide network → ambient
Strava demand field → LightGBM volume models → predicted bike/ped volumes →
web-tool outputs. Heavy/cached targets marked ⏳.

```
INPUTS                         ENRICHMENT                      MODELING & OUTPUTS
──────                         ──────────                      ──────────────────

Strava zips ─┐
             ├─► strava_raw ⏳ ─► master_network ─► strava_chunks (×8)
OSM (1.2GB) ─┘     ▲                    │                │
  osm_reference ⏳ │                    │                ▼ (mapped, parallel, crew)
  osm_processed ───┘                    │          strava_base ⏳ → _with_crashes ⏳
                                        │                │      → _with_census
SLD/WI/NOAA/SWITRS ──► context layers ──┤                ▼
                                        │       enriched_strava_chunks
                                        │                ▼
                                        │       enriched_bike_network ⏳
                                        │                ▼
                                        │       network_topology ⏳ (cached)
                                        │                ▼
counts (UCB + Caltrans + CAT Portal) ──►│       partitioned_data  (snap counts; deployment="main")
   └─► ground_truth_counts ─────────────┘                │
                                                         ▼
              strava_grid ⏳ ──(ambient lookup)──► bike_train / ped_train
            (raster .tif, built once)                    │
                                          ┌──────────────┴───────────────┐
                                          ▼ Track A (with on-link Strava) ▼ Track B (Strava-free)
                                     model_bike_A / model_ped_A     model_bike_B / model_ped_B
                                     val_bike_A / val_ped_A         val_bike_B / val_ped_B
                                          │                              │
                                          ▼                              ▼
                            final_predictions (on network)    web_model_assets_file
                                          │                  (LightGBM .txt + JSON spec
                                          ▼                   → ONNX for Node web tool)
                            web_ready_network ─► final_web_network ─► geojson + Appendix A

CONTEXT (web tool):
 census_blocks ─► census_chunks (×32) ─► web_blocks_raw_chunks ⏳ ─► web_context_map
```

### Two model tracks (one engine: LightGBM Tweedie)

Both tracks are **LightGBM Tweedie** regressors (`tweedie_variance_power = 1.7`),
trained on the same data. They differ by **exactly one predictor**: on-link
Strava. Both use **ambient Strava** (demand summed in rings around each site),
which is available even for a not-yet-built path.

| Track | Predictors | Predicts | Output |
|---|---|---|---|
| **A** (`model_bike_A`, `model_ped_A`) | base **+ `strava_vol_total`** | the **existing** network | `links.geojson` / `nodes.geojson` (volumes baked in) |
| **B** (`model_bike_B`, `model_ped_B`) | base only (Strava-free) | **new off-street paths** (no Strava yet) | `web_models/` → ONNX for the Node.js web tool |

Validation (`val_*`) uses spatial 10-fold CV and reports **class accuracy +
absolute error** (low/mid/high volume), not percent bias (which explodes near
zero on low-volume sites).

**Dropped from the old design:** the Random-Forest (ranger) track, the GBM
comparison, and the Hierarchical-GLM (lme4) track. The Strava-free LightGBM Track
B replaces the HGLM for new-path prediction — it is far more accurate and exports
to the Node tool via ONNX. Also dropped as predictors: `crash_count_30m`
(endogenous: demand causes crashes, and circular with the tool's safety
analysis), per-site `WWI` (Strava-derived; ambient captures the recreational
signal), and `year` (constant).

### Predictors (single source of truth, `modeling.R`)

`PREDICTORS_BASE` = network attributes (`infra_type`, `is_paved`, `speed_limit`),
density/population/employment, 20 BNA accessibility buffers, `precip_annual`, and
4 **ambient Strava annuli** (`amb_strava_{250,500,1000,2000}m`).
`PREDICTORS_A = base + strava_vol_total`; `PREDICTORS_B = base`.

### Ambient Strava (raster focal-sum)

A site's demand is better predicted by the Strava activity of the **surrounding
network** than by the (often zero) Strava on the facility itself. Computing ring
sums point-by-point for the 8 M-link prediction network is infeasible (~1,250 hr).
Instead `strava_grid` rasterizes Strava to a 100 m grid and uses focal (moving-
window) sums — the whole field in ~7 min, validated near-identical to exact
point-buffers for the 250 m+ rings. It is a `format = "file"` `.tif` target
(terra rasters don't serialize), built once; all ambient extraction is a fast
lookup. See `src/functions/ambient.R`.

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

---

## Deploying the Track B models to the Node.js web tool

The pipeline writes, per mode, to `data_processed/web_models/`:
`<mode>_model.txt` (LightGBM), `<mode>_model.json` (`lgb.dump`), and
`<mode>_feature_spec.json` (the exact one-hot column order + transforms).

ONNX conversion is a **one-time step outside the pipeline** (keeps the R rebuild
free of a Python dependency):

```bash
pip install lightgbm onnxmltools skl2onnx onnx
python src/convert_to_onnx.py --models-dir ".../data_processed/web_models"
```

In Node, load the `.onnx` with `onnxruntime-node`, build the input vector in the
order given by `feature_spec.onehot_columns` (ambient features arrive already
`log1p`-scaled; numeric NA → 0; factors one-hot). The model output is the
**count-scale** volume (Tweedie — do **not** exponentiate).

---

## Model performance (spatial 10-fold CV)

LightGBM Tweedie, judged by **class accuracy** (low/mid/high volume) and absolute
error — the metrics that matter for the tool. (Percent bias looks alarming on
low-volume sites only because the denominator is tiny; absolute errors there are
small — e.g. a low site predicted 35 vs observed 22.)

| Model | class accuracy | off-by-2 (low↔high) |
|---|---|---|
| A_bike / B_bike | ~63% / ~61% | 2% |
| A_ped  / B_ped  | ~64% / ~64% | 2% |

Track B (Strava-free) is nearly as accurate as Track A — ambient Strava largely
substitutes for on-link Strava, which is why the new-path model is strong. The
low-volume "ranking floor" (covariates can't perfectly order quiet sites) is
inherent, but class accuracy and absolute error are good and catastrophic
(low↔high) errors are rare.
