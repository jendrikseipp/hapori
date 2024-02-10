#! /usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import os
import sys
import shutil

import glob


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


def compute_image(pwd, graph_file, domain, problem):
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
    dom_path = os.path.join(pwd, "images", domain)

    if not os.path.exists(dom_path):
       os.makedirs(dom_path)

    new_image_name = "%s.png" % problem
    new_image_path = os.path.join(dom_path, new_image_name)
    shutil.move(image_path, new_image_path)
    assert os.path.exists(new_image_path)
    return os.path.abspath(new_image_path)



parser = argparse.ArgumentParser()
parser.add_argument("benchmark")

def main(args):


    pwd = os.path.abspath(".")
    #print("Computing an abstract structure graph from the lifted task description...")

    benchmarkdir = args.benchmark
    for domain in glob.glob(os.path.join(benchmarkdir,"*","domain-*")):
        problem = domain.replace("domain-","")

        sys.stdout.flush()
        graph_file = compute_graph_for_task(pwd, domain, problem)
        sys.stdout.flush()
        domain_name = os.path.basename(os.path.dirname(domain))
        # print(domain_name)
        problem_name = os.path.basename(problem)
        # print(problem_name)

        image_file = compute_image(pwd, graph_file, domain_name, problem_name)
        sys.stdout.flush()
        print(image_file)

if __name__ == "__main__":
    main(parser.parse_args())
