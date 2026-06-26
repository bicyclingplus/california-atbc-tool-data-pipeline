/* =============================================================================
 * predict.js — feed the ATBC Track B models for a user-created link or node.
 * -----------------------------------------------------------------------------
 * The pipeline exports, per mode, to data_processed/web_models/:
 *   <mode>_model.txt           LightGBM text model
 *   <mode>_model.json          lgb.dump
 *   <mode>_feature_spec.json   the feature contract used below
 * and (one-time, outside the pipeline) src/convert_to_onnx.py converts the
 * model to <mode>_model.onnx for onnxruntime-node.
 *
 *   npm install onnxruntime-node
 *
 * WHAT THE MODEL NEEDS (Track B, "Strava-free" new paths). Per feature:
 *   - From the UI (the planner specifies the new path/intersection):
 *       infra_type   (string: "separated_path" | "bike_lane" | "buffered_lane"
 *                     | "shared_lane_marked" | "quiet_street" | "shared_arterial"
 *                     | "other"  — see feature_spec.categorical.infra_type)
 *       is_paved     (0 | 1)
 *       speed_limit  (number, mph)
 *   - From the spatial join of the feature's location to context_blocks.geojson:
 *       emp_density, int_density, walk_index, housing_total,
 *       pop_low/high, emp_low/high, schools_low/high, colleges_low/high,
 *       doctors_low/high, pharmacies_low/high, retail_low/high,
 *       supermarket_low/high, parks_low/high, trails_low/high,
 *       community_low/high, transit_low/high,
 *       precip_annual, temp_min, temp_max,
 *       amb_strava_250m, amb_strava_500m, amb_strava_1000m, amb_strava_2000m
 *     (ambient is already log1p-scaled in context_blocks.geojson — pass as-is.)
 *
 * Assignment (matches the pipeline):
 *   - a LINK (path segment)  -> BIKE model -> predicted bike AADT
 *   - a NODE (intersection)  -> PED  model -> predicted ped  AADT
 * The model output is COUNT-scale (Tweedie). DO NOT exponentiate.
 * ===========================================================================*/

const fs = require("fs");
const path = require("path");
const ort = require("onnxruntime-node");

const MODELS_DIR = path.join(__dirname, "..", "data_processed", "web_models");

// --- Load a mode's session + feature spec ----------------------------------
async function loadModel(mode) {
  const spec = JSON.parse(
    fs.readFileSync(path.join(MODELS_DIR, `${mode}_feature_spec.json`), "utf8")
  );
  const session = await ort.InferenceSession.create(
    path.join(MODELS_DIR, `${mode}_model.onnx`)
  );
  return { spec, session, inputName: session.inputNames[0] };
}

/**
 * Build the model input vector (Float32Array) in the exact order the model
 * expects (spec.onehot_columns), from a flat object of raw predictor values.
 *
 *  - categorical predictors (spec.categorical = { infra_type:[...], is_paved:[...] }):
 *      set the single matching <predictor><level> column to 1; all other levels 0.
 *      An unrecognized level leaves every level column at 0 (model treats it as
 *      unseen) — log it so bad UI inputs are caught.
 *  - numeric predictors: copied straight in; missing / non-finite -> 0.
 */
function buildFeatureVector(raw, spec) {
  const cols = spec.onehot_columns;
  const index = new Map(cols.map((c, i) => [c, i]));
  const vec = new Float32Array(cols.length); // initialized to 0
  const categorical = spec.categorical || {};

  for (const p of spec.raw_predictors) {
    if (p in categorical) {
      const level = raw[p] == null ? null : String(raw[p]);
      const col = `${p}${level}`;
      if (index.has(col)) {
        vec[index.get(col)] = 1;
      } else if (level !== null) {
        console.warn(`[predict] unknown ${p} level "${level}" -> all-zero one-hot`);
      }
    } else {
      const v = Number(raw[p]);
      if (index.has(p)) vec[index.get(p)] = Number.isFinite(v) ? v : 0;
    }
  }
  return vec;
}

// --- Run one prediction (returns the count-scale AADT) ---------------------
async function predictOne({ session, spec, inputName }, raw) {
  const vec = buildFeatureVector(raw, spec);
  const tensor = new ort.Tensor("float32", vec, [1, vec.length]);
  const out = await session.run({ [inputName]: tensor });
  const value = out[session.outputNames[0]].data[0];
  return Math.max(0, value); // volumes are non-negative; do NOT exponentiate
}

// --- Public API ------------------------------------------------------------
// Combine the UI inputs with the spatially-joined block attributes into one flat object, then predict.

async function predictLinkBikeAADT(bikeModel, uiInputs, blockAttrs) {
  return predictOne(bikeModel, { ...blockAttrs, ...uiInputs });
}

async function predictNodePedAADT(pedModel, uiInputs, blockAttrs) {
  return predictOne(pedModel, { ...blockAttrs, ...uiInputs });
}

// --- Example usage ---------------------------------------------------------
async function main() {
  const bike = await loadModel("bike");
  const ped = await loadModel("ped");

  // blockAttrs = the row from context_blocks.geojson the feature falls in
  // (these are illustrative numbers; real values come from the spatial join).
  const blockAttrs = {
    emp_density: 120, int_density: 80, walk_index: 14, housing_total: 900,
    pop_low: 300, pop_high: 60, emp_low: 200, emp_high: 40,
    schools_low: 2, schools_high: 0, colleges_low: 0, colleges_high: 0,
    doctors_low: 1, doctors_high: 0, pharmacies_low: 1, pharmacies_high: 0,
    retail_low: 5, retail_high: 1, supermarket_low: 1, supermarket_high: 0,
    parks_low: 1, parks_high: 0, trails_low: 0, trails_high: 0,
    community_low: 1, community_high: 0, transit_low: 3, transit_high: 1,
    precip_annual: 500, temp_min: 8, temp_max: 24,
    // ambient: already log1p-scaled in context_blocks.geojson — pass as-is
    amb_strava_250m: 7.1, amb_strava_500m: 8.0, amb_strava_1000m: 9.2, amb_strava_2000m: 10.4
  };

  // A NEW LINK (the path the planner draws) -> bike volume
  // always use these values for a new link (off-street paved path, low speed,
  // local-road hierarchy)
  const linkUI = { infra_type: "separated_path", functional: "Local Road", is_paved: 1, speed_limit: 15 };
  const bikeAADT = await predictLinkBikeAADT(bike, linkUI, blockAttrs);
  console.log("predicted bike AADT on new link:", bikeAADT.toFixed(1));

  // A NEW NODE (an intersection) -> ped volume
  // get these values from a near join to existing network (infra_type,
  // functional, is_paved, speed_limit of the nearest existing node/links).
  const nodeUI = { infra_type: "shared_arterial", functional: "Minor Road", is_paved: 1, speed_limit: 30 };
  const pedAADT = await predictNodePedAADT(ped, nodeUI, blockAttrs);
  console.log("predicted ped AADT on new node:", pedAADT.toFixed(1));
}

if (require.main === module) main().catch((e) => { console.error(e); process.exit(1); });

module.exports = {
  loadModel, buildFeatureVector, predictOne,
  predictLinkBikeAADT, predictNodePedAADT
};

/* -----------------------------------------------------------------------------
 * NOTE — cross-mode volumes (optional, mirrors map_volumes_across_network):
 *   The pipeline also spreads bike volume to nodes (max over connected links)
 *   and ped volume to links (avg of the two endpoint nodes). If the web tool
 *   needs both volumes on every feature, run the model above per feature, then
 *   apply that topology step across the user's drawn network — graph
 *   operation, not a model call.
 * ---------------------------------------------------------------------------*/
