#!/usr/bin/env python3
"""
Convert the pipeline's Track B LightGBM models to ONNX for onnxruntime-node.

The R pipeline (export_models_for_node) writes, per mode, into
data_processed/web_models/:
    <mode>_model.txt          LightGBM text model
    <mode>_feature_spec.json  feature contract (onehot_columns order, transforms)

This is a ONE-TIME packaging step, run OUTSIDE the targets pipeline (keeps the R
rebuild free of a Python dependency). Run it after a pipeline build whenever the
Track B models change.

Setup (once):
    pip install lightgbm onnxmltools onnxconverter-common skl2onnx onnx

Usage:
    python src/convert_to_onnx.py \
        --models-dir "C:/Users/Dillon/Box/_Projects/Caltrans_BC2/data_processed/web_models"

Produces <mode>_model.onnx alongside the inputs. In Node:
    const ort = require('onnxruntime-node');
    const session = await ort.InferenceSession.create('bike_model.onnx');
    // Build the input vector in the EXACT order of feature_spec.onehot_columns,
    // applying the transforms noted in the spec (ambient already log1p'd, NA->0,
    // factors one-hot). The model output is the COUNT-scale volume (Tweedie;
    // do NOT exponentiate).
"""
import argparse
import json
import os

import lightgbm as lgb
from onnxmltools.convert import convert_lightgbm
from onnxmltools.convert.common.data_types import FloatTensorType


def convert_one(models_dir: str, mode: str) -> str:
    txt = os.path.join(models_dir, f"{mode}_model.txt")
    spec_path = os.path.join(models_dir, f"{mode}_feature_spec.json")
    out = os.path.join(models_dir, f"{mode}_model.onnx")

    with open(spec_path) as f:
        spec = json.load(f)
    n_features = len(spec["onehot_columns"])

    booster = lgb.Booster(model_file=txt)
    onnx_model = convert_lightgbm(
        booster,
        initial_types=[("input", FloatTensorType([None, n_features]))],
        target_opset=12,
    )
    with open(out, "wb") as f:
        f.write(onnx_model.SerializeToString())
    print(f"  {mode}: {n_features} features -> {out}")
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--models-dir", required=True,
                    help="data_processed/web_models directory from the pipeline")
    args = ap.parse_args()

    print("Converting Track B LightGBM models to ONNX...")
    for mode in ("bike", "ped"):
        convert_one(args.models_dir, mode)
    print("Done. Load the .onnx files in Node with onnxruntime-node.")


if __name__ == "__main__":
    main()
