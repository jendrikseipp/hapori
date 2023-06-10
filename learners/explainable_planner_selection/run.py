#!/usr/bin/env python3
import argparse
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

import joblib
import numpy as np

parser = argparse.ArgumentParser()
parser.add_argument("model")
parser.add_argument("domain")
parser.add_argument("problem")
parser.add_argument("plan")

DIR_CURR_FILE = Path(__file__).parent
FILE_PLAN_PY = DIR_CURR_FILE / "plan.py" if (DIR_CURR_FILE / "plan.py").exists() else DIR_CURR_FILE.parent.parent
FILE_FEATURE_EXTRACTOR = DIR_CURR_FILE / "feature_extractor" / "extract_planning_features.py"
FILE_FEATURE_ORDER = DIR_CURR_FILE / "models" / "feature_order.json"
FILE_PLANNER_ORDER_OPT = DIR_CURR_FILE / "models" / "opt_planners.json"
FILE_PLANNER_ORDER_SAT = DIR_CURR_FILE / "models" / "sat_planners.json"


def generate_features(d, f_domain, f_problem):
    f_features = os.path.join(d, "tmp_features")
    try:
        subprocess.call([
            sys.executable, FILE_FEATURE_EXTRACTOR,
            "--domain-file", f_domain,
            "--instance-file", f_problem,
            "--json-output-file", f_features,
            "--no-extract-sas", "--no-extract-lpg-probing",
            "--no-extract-fd-probing", "--no-extract-sat", "--no-extract-torchlight",
        ], cwd=d, stdout=subprocess.DEVNULL)
    except subprocess.SubprocessError:
        return None

    if os.path.exists(f_features):
        with open(f_features) as f:
            features = json.load(f)
        tmp = list(features["instance_features"].values())
        assert len(tmp) == 1
        features = tmp[0]

        with open(FILE_FEATURE_ORDER, "r") as f:
            feature_order = json.load(f)
        return [features[feature_name] for feature_name in feature_order]
    return None


def query_model(f_model, features):
    base_name = os.path.basename(f_model)
    is_opt = base_name.startswith("opt_")
    is_sat = base_name.startswith("sat_")
    assert is_opt ^ is_sat
    file_planner_order = FILE_PLANNER_ORDER_OPT if is_opt else FILE_PLANNER_ORDER_SAT
    with open(file_planner_order, "r") as f:
        planner_order = json.load(f)
    if features is None:
        print("WARNING: Features not generated. Using fallback planner.")
        return "ipc2018-opt-scorpion:default" if is_opt else 'ipc2018-agl-saarplan:default'

    is_linear_regression = base_name.find("linear_regression") > -1
    is_decision_tree = base_name.find("decision_tree") > -1
    assert is_linear_regression ^ is_decision_tree

    model, _ = joblib.load(f_model)
    prediction = model.predict([features])[0]

    if is_linear_regression:
        assert len(prediction) == len(planner_order)
        fastest_planner = np.argmin(prediction)
        return planner_order[fastest_planner]
    else:
        return planner_order[prediction]


def execute_planner(d, f_domain, f_problem, f_plan, planner):
    planner_name, planner_config = planner.rsplit(":", 1)
    subprocess.call([
        sys.executable, FILE_PLAN_PY,
        planner_name, "--configs", planner_config,
        f_domain, f_problem, f_plan
    ], cwd=d)


def main(args):
    assert os.path.exists(args.domain), args.domain
    assert os.path.exists(args.problem), args.problem
    assert os.path.exists(args.model), args.model
    assert not os.path.exists(args.plan), args.plan
    args.domain = os.path.abspath(args.domain)
    args.problem = os.path.abspath(args.problem)
    args.model = os.path.abspath(args.model)
    args.plan = os.path.abspath(args.plan)

    with tempfile.TemporaryDirectory() as d:
        features = generate_features(d, args.domain, args.problem)
        planner = query_model(args.model, features)
        print(f"Selected Planner: {planner}")
        execute_planner(d, args.domain, args.problem, args.plan, planner)



if __name__ == "__main__":
    main(parser.parse_args())
