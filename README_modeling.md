# Methods: Bicycle and Pedestrian Volume Estimation

## Overview

The basis for the estimates of annual average daily bicycle and pedestrian volumes for
every segment and intersection of the California network, providing demand and exposure
inputs for the Active Transportation Benefit–Cost Tool are described below. Models are trained on
observed counts at several thousand locations and applied to the full network of
approximately eight million links. The approach follows the direct-demand
modeling tradition reviewed by Miah, Hyun, and Mattingly (2024a), in which volumes
are predicted from local network, land-use, and crowd-sourced activity covariates.

## Count data and sources

Count data are assembled from four sources.

**Statewide bicycle AADT (Miah et al. 2024b).** Annual average daily bicycle
traffic estimates produced by the UC Berkeley large-scale AADBT methodology.
These form the primary bicycle training set and are concentrated at on-street
urban locations.

**California SHS pedestrian exposure model (Griswold et al. 2019).** Annual
pedestrian volume estimates for intersections on the California State Highway
System, produced under a direct-demand framework. These form the primary
pedestrian training set.

**Caltrans Active Transportation count dataset
([data.ca.gov](https://data.ca.gov/dataset/at-count-dataset)).** Continuous
counts from the Caltrans computer-vision program on state highways. These sites
are low-volume but continuously observed, extending the sample into the
low-demand regime that conventional count programs under-represent.

**California Active Transportation Data Portal
([catdataportal.berkeley.edu](https://catdataportal.berkeley.edu/)).** Additional
bicycle and pedestrian count sites contributed by agencies statewide, the
majority short-duration manual counts.

### Inclusion, exclusion, and de-duplication

All sources are pooled into a single bicycle training set and a single
pedestrian training set. Data Portal sites falling within a short distance of an
existing training site are removed to avoid duplicate observations of the same
location; records lacking coordinates or a usable volume are dropped. The
Caltrans and Data Portal sites are retained deliberately: being predominantly
low-volume, they broaden the training distribution toward the quiet majority of
the network—the regime that count programs sited at high-demand locations
typically miss, and a recognized source of over-prediction error in the
literature.

### Temporal expansion of short counts

Count durations range from a few hours to a full year and must be placed on a
common annual-average footing. Short counts are expanded in two stages. A count
spanning only part of a day is first scaled to a full-day total using an
hour-of-day activity profile estimated from the continuously operating counters,
weighted by the specific hours each short count observed. The resulting daily
estimates are then adjusted for seasonality so that every site expresses an annual
average daily volume. Sources already supplied as annual estimates (the bicycle
AADT and pedestrian exposure datasets) enter directly without further expansion.

### Snapping counts to the network

Each count site is matched to its nearest network feature—bicycle counts to
links, pedestrian counts to intersection nodes—by nearest-feature matching in a
projected coordinate system.

## Predictors

Each location is characterized by network attributes (facility type, surface
type, speed limit), population and employment density, accessibility measures
(counts of destinations such as schools, retail, and transit within fixed
buffers), and annual precipitation.

Crash counts are excluded from the predictor set. Bicycle and pedestrian volume
is a determinant of crash frequency rather than a consequence of it, so crashes
are a downstream effect of the quantity being modeled; furthermore, crash exposure
is itself an output of the Benefit–Cost Tool, and predicting volume from crashes
would make that exposure calculation circular.

### Ambient crowd-sourced activity

On-facility Strava activity is a weak predictor—a small and variable share of
riders use the application—and is undefined for infrastructure that does not yet
exist. The models therefore use *ambient* Strava activity: the total Strava
volume within concentric rings (annuli at 250, 500, 1000, and 2000 meters) around
each location, characterizing the latent cycling demand of the surrounding area.
Ambient activity is available even for a proposed facility with no record of its
own, and across the network is a substantially stronger correlate of observed
volume than on-facility activity.

Computing ambient sums point-by-point across the full prediction network is
computationally intractable. Strava volume is instead rasterized to a 100-meter
grid, and ambient sums are obtained by focal (moving-window) summation, which
evaluates the entire grid at once; values are read off at count sites and network
features by lookup. Validated against exact point-buffer computation, the
raster-based annuli are near-identical at the 250-meter scale and larger.

## Models

Volumes are modeled with gradient-boosted decision trees (LightGBM) under a
Tweedie objective, which is appropriate for the heavily right-skewed,
non-negative, semi-continuous distribution of count data. Two models are
estimated per mode:

- **Existing-network model.** Includes on-link Strava among its predictors. Its
  fitted volumes are written to the network output layers.
- **New-facility model.** Excludes on-link Strava and relies on ambient activity
  and contextual covariates, enabling prediction for proposed off-street paths
  that have no prior activity record. This model is exported for on-demand
  prediction in the web tool.

The two models differ only in the inclusion of on-link Strava; predictive
performance is comparable, indicating that ambient activity substitutes
effectively for on-facility activity.

## Validation and performance metrics

Models are validated by spatial ten-fold cross-validation, with folds grouped by
location so that nearby observations do not appear in both training and test sets.
Performance is assessed by classification accuracy across low, medium, and high
volume terciles and by absolute prediction error, rather than by percentage error,
which is undefined or unstable at low volumes. Across modes and model variants,
volume-class accuracy is approximately 62–64 percent, and severe misclassification
(low predicted as high, or the reverse) occurs in approximately two percent of
cases.

Discrimination among individual low-volume sites is limited by the available
covariates; the models reliably recover the volume class and approximate
magnitude required by the tool but do not resolve fine distinctions within the
low-volume regime.

## Hyperparameter selection

All model hyperparameters were selected by grid search under spatial five-fold
cross-validation with early stopping, optimizing volume-class accuracy. Tuning
proceeded in three stages: the Tweedie variance power together with tree-
complexity parameters (number of leaves, minimum observations per leaf, feature
subsampling); a finer sweep of the Tweedie variance power; and regularization
(L1 and L2 penalties, row subsampling, and tree-depth limits).

Optimal settings differ by both mode and model variant. The bicycle models favor
a heavier-tailed Tweedie power and feature subsampling; the pedestrian models a
lighter Tweedie power and no subsampling. Regularization produced small,
consistently non-negative gains (an L2 penalty benefiting the bicycle new-
facility model most); row subsampling and depth limits did not improve cross-
validated accuracy and are left off. Overall, performance is insensitive to these
parameters across a broad range, indicating the models are not overfitting. All
parameters are specified explicitly in `lgb_params()` (`src/functions/modeling.R`)
rather than relying on library defaults.

## Deployment

The existing-network models are applied within the pipeline and their predictions
are written to GeoJSON network layers. The new-facility models are exported as
LightGBM text models together with a feature specification and converted to ONNX
for execution in the Node.js web application via `onnxruntime-node`.

---

## References

- Griswold, J. B., Medury, A., Schneider, R. J., Amos, D., Li, A., & Grembek, O.
  (2019). A Pedestrian Exposure Model for the California State Highway System.
  *Transportation Research Record,* 0361198119837235. (California SHS pedestrian
  exposure model; the pedestrian training data.
  [SafeTREC](https://safetrec.berkeley.edu/research/exposure-modeling/california-shs-pedestrian-exposure-model).)
- Miah, M. M., Hyun, K. K., & Mattingly, S. P. (2024a). A review of bike volume
  prediction studies. *Transportation Letters,* 16(10), 1406–1433.
- Miah, M. M., Griswold, J., Proulx, F., Bigham, J., Banerjee, I., & Grembek, O.
  (2024b). Methodology of Large-Scale Annual Average Daily Bicycle Traffic
  Estimation. *Journal of Transportation Engineering, Part A: Systems,* 151(11),
  04025088. (Statewide bicycle AADT training data.)
- Caltrans Active Transportation Count Dataset.
  [data.ca.gov/dataset/at-count-dataset](https://data.ca.gov/dataset/at-count-dataset).
- California Active Transportation Data Portal.
  [catdataportal.berkeley.edu](https://catdataportal.berkeley.edu/).

*Full technical notes, the literature review, and detailed tuning results are in
`docs/`.*
