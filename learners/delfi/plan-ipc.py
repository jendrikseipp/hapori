#! /usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import json
import os
import sys

from keras.models import model_from_json
import numpy as np
from PIL import Image



if (sys.version_info > (3, 0)):
    import subprocess
else:
    import subprocess32 as subprocess


GRAPH_CREATION_TIME_LIMIT = 60 # seconds
IMAGE_CREATION_TIME_LIMIT = 180 # seconds

DIR_SCRIPT = os.path.dirname(os.path.abspath(__file__))
FILE_PLAN_PY = os.path.join(DIR_SCRIPT, "plan.py") if os.path.exists(os.path.join(DIR_SCRIPT, "plan.py")) else os.path.join(DIR_SCRIPT, "..", "..", "plan.py")
assert os.path.exists(FILE_PLAN_PY)


def compute_graph_for_task(pwd, domain, problem):
    command = [sys.executable, os.path.join(DIR_SCRIPT, 'translate/abstract_structure_module.py'), '--only-functions-from-initial-state', domain, problem]
    graph_file = os.path.join(pwd, 'abstract-structure-graph.txt')
    try:
        subprocess.check_call(command, timeout=GRAPH_CREATION_TIME_LIMIT)
    except subprocess.TimeoutExpired:
        sys.stdout.flush()
        print("Graph computation reached the time limit!")
        return None
    except subprocess.CalledProcessError as err:
        sys.stdout.flush()
        print("Graph computation returned nonzero exitcode {}".format(err.returncode))
        return None
    except:
        # We catch all exceptions (this unfortunately includes signals) to make
        # sure that if we cannot automatically select a planner, we still run
        # our fallback planner.
        return None
        #raise
    return os.path.abspath(graph_file)


def compute_image(pwd, graph_file):
    if graph_file is None:
        return None
    assert os.path.exists(graph_file)
    try:
        # Create an image from the abstract structure for the given domain and problem.
        subprocess.check_call([sys.executable, os.path.join(DIR_SCRIPT, 'create-image-from-graph.py'), '--write-abstract-structure-image-reg', '--bolding-abstract-structure-image', '--abstract-structure-image-target-size', '128', graph_file, pwd], timeout=IMAGE_CREATION_TIME_LIMIT)
    except subprocess.TimeoutExpired:
        sys.stdout.flush()
        print("Image computation reached the time limit!")
        return None
    except subprocess.CalledProcessError as err:
        sys.stdout.flush()
        print("Image computation returned nonzero exitcode {}".format(err.returncode))
        return None
    except:
        # We catch all exceptions (this unfortunately includes signals) to make
        # sure that if we cannot automatically select a planner, we still run
        # our fallback planner.
        return None
        #raise

    # TODO: we should be able to not hard-code the file name
    image_file_name = 'graph-gs-L-bolded-cs.png'
    image_path = os.path.join(pwd, image_file_name)
    assert os.path.exists(image_path)
    return os.path.abspath(image_path)


def select_algorithm_from_model(json_model, h5_model, image, planner_names):
    print("Using json model file {}".format(json_model))
    print("Using h5 model file {}".format(h5_model))

    # suppress unwanted output
    os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'

    # load json and create model
    with open(json_model, "r") as f:
        loaded_model_json = f.read()
    model = model_from_json(loaded_model_json)
    # load weights into new model
    model.load_weights(h5_model)
    print("Loaded model from disk")

    #print(str(len(list_solver_names)) + " Solvers: ")
    #print(list_solver_names)

    list_x = []
    img = Image.open(image)
    list_x.append(np.array(img))

    #print("\nNumber of total data points: " + str(len(list_x)))
    # Normalize feature image values to 0..1 range (assumes gray scale)
    data = np.array(list_x, dtype="float") / 255.0
    data  = data.reshape( data.shape[0], 128, 128, 1 )

    # For each test data point compute predictions for each of the solvers
    preds = model.predict(data)[0]
    assert len(preds) == len(planner_names)
    priorities = (preds * np.array([1,2,3,4])).sum(axis=1)
    assert len(priorities) == len(planner_names)
    best_planner_idx = np.argmin(priorities)
    best_planner = planner_names[best_planner_idx]
    print("Model chose %s" % best_planner)
    return best_planner


def execute_planner(d, f_domain, f_problem, f_plan, planner):
    planner_name, planner_config = planner.rsplit(":", 1)
    subprocess.call([
        "python3", FILE_PLAN_PY,
        planner_name, "--configs", planner_config,
        f_domain, f_problem, f_plan
    ], cwd=d)

parser = argparse.ArgumentParser()
parser.add_argument("domain_file")
parser.add_argument("problem_file")
parser.add_argument("plan_file")
parser.add_argument("model_name")


def main(args):
    domain = args.domain_file
    problem = args.problem_file
    plan = args.plan_file
    model = args.model_name

    # TODO: adapt this to three models, one per track
    # assert model in ["optimal", "satisficing", "agile"]
    assert model in ["fullbinary", "fulldiscrete", "fullsatisficing", "hardestbinary", "hardestdiscrete", "hardestsatisficing"]
    is_opt = "binary" in model or "discrete" in model
    file_planner_names = os.path.join(DIR_SCRIPT, "opt_planners.json" if is_opt else "sat_planners.json")
    with open(file_planner_names, "r") as f:
        planner_names = json.load(f)

    pwd = os.path.abspath(".")
    print("Computing an abstract structure graph from the lifted task description...")
    sys.stdout.flush()
    graph_file = compute_graph_for_task(pwd, domain, problem)
    sys.stdout.flush()
    image_file = compute_image(pwd, graph_file)
    sys.stdout.flush()

    if image_file is None:
        # TODO: use sensible default planners based on training data
        # planner = "ipc2018-opt-scorpion:default" if is_opt else 'ipc2018-agl-saarplan:agl-config01'
        # optimal and satisficing configs only
        planner = "ipc2018-opt-scorpion:default" if is_opt else "ipc2018-decstar+sat-config03"
        print("Could not compute image. Use default planner.")
    else:
        json_model = os.path.join(DIR_SCRIPT, 'models2024', args.model_name + ".json")
        h5_model = os.path.join(DIR_SCRIPT, "models2024",  args.model_name + ".h5")
        planner = select_algorithm_from_model(json_model, h5_model, image_file, planner_names)
    print("Selected Planner: %s" % planner)
    sys.stdout.flush()
    execute_planner(pwd, domain, problem, plan, planner)

if __name__ == "__main__":
    main(parser.parse_args())
