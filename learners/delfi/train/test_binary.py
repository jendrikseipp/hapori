#! /usr/bin/env python
# -*- coding: utf-8 -*-

from keras.models import model_from_json

import random
import argparse

import numpy as np
from PIL import Image
import os
import pandas as pd
from operator import add


def load_model(json_model, h5_model, verbose=False):
    if verbose:
        print("Using json model file {}".format(json_model))
        print("Using h5 model file {}".format(h5_model))

    # suppress unwanted output
    os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'

    # load json and create model
    json_file = open(json_model, 'r')
    loaded_model_json = json_file.read()
    json_file.close()
    model = model_from_json(loaded_model_json)
    # load weights into new model
    model.load_weights(h5_model)
    if verbose:
        print("Loaded model from disk")
    return model


def select_algorithm_from_model(args, model, image):
    list_x = []

    img = Image.open(image)
    list_x.append(np.array(img))

    #print("\nNumber of total data points: " + str(len(list_x)))
    # Normalize feature image values to 0..1 range (assumes gray scale)
    data = np.array(list_x, dtype="float") / 255.0
    data  = data.reshape( data.shape[0], 128, 128, 1 )

    # For each test data point compute predictions for each of the solvers
    preds = model.predict(data)
    # if args.verbose:
    #     print(preds)
    return np.argmax(preds[0])


def readCSV(path_csv, verbose=False):
    # Parse performance data and load images to array
    if verbose:
        print("Load CSV File: %s" % path_csv)
    df = pd.read_csv(path_csv)

    # Runtimes
    list_y = []
    # Instance names
    list_instance_names = []
    # Solver names
    list_solver_names = []

    for n in range(1, len(df.columns)):
        list_solver_names.append(df.columns[n].strip())

    if verbose:
        print("%i Solvers: " % len(list_solver_names))
        print(list_solver_names)
    # Set index as filename
    df2 = df.set_index('filename')
    # Loop over all instance names
    for name in df['filename']:
        # Solver name
        list_instance_names.append(name)
        # Runtimes
        list_y.append(df2.loc[name])

    # Labels as runtimes
    labels = np.array(list_y)

    return labels, list_solver_names, list_instance_names

def get_image_path(args, problem):
    dom = problem.split(":")[0]
    prob = problem.split(":")[1]
    return os.path.join( args.images, dom ,"%s.png" % prob)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument("--model", type=str, action="store", default=None)
    parser.add_argument("--images", type=str, action="store", default=None)
    parser.add_argument("--runtimes", type=str, action="store", default=None)
    parser.add_argument("--problem", type=str, action="store", default=None)
    parser.add_argument("--problems", type=str, action="store", default=None)
    
    parser.add_argument("--verbose", action="store_true")


    args = parser.parse_args()
    json_model = os.path.join(args.model,"model.json")
    h5_model = os.path.join(args.model,"model.h5")

    problems = []
    if args.problems:
        with open(args.problems) as input_file:
            problems = [line.strip() for line in input_file]
    elif args.problem:
        problems = [ args.problem ]
    else:
        print("Either a problem or problems file should be provided")
        exit(1)
    labels, list_solver_names, list_instance_names = readCSV(args.runtimes, args.verbose)
    if args.verbose:
        print("Number of problems read: %s" % len(problems))
    model = load_model(json_model, h5_model, args.verbose)
    coverage = 0
    potential = 0
    random_coverage = 0
    all_solving = 0
    coverages = [0] * len(list_solver_names)
    for problem in problems:
        # if args.verbose:
        #     print(problem)
        if problem not in list_instance_names:
            if args.verbose:
                print("skipped", problem)
            continue
        image = get_image_path(args, problem)
        if not os.path.exists(image):
            if args.verbose:
                print("Image file %s does not exist, skipped" % image)
            continue
        selected_index = select_algorithm_from_model(args, model, image)
        times = labels[list_instance_names.index(problem)]
        for index, x in enumerate(times):
            if x == '-':
                times[index] = 10000
            else:
                times[index] = float(times[index])
        if times[selected_index] < 10000:
            coverage += 1
        # else: 
            # print(problem, selected_index, times[selected_index], min(times), max(times))
            # print(times)
        if args.verbose:
            curr_coverage = [ int(t < 10000) for t in times]
            coverages = map(add, coverages, curr_coverage)
            if min(times) < 10000:
                potential += 1
            if max(times) < 10000:
                all_solving += 1
            if random.choice(times) < 10000:
                random_coverage += 1
            # print(problem, times[selected_index] < 10000, selected_index, times[selected_index], min(times), max(times))


    if args.verbose:
        print("Coverage: %s (out of %s, random %s, trivial %s)" % (coverage, potential, random_coverage, all_solving))
        print([c for c in coverages])
        # print(list_solver_names)
        # print(list_instance_names)
    else:
        print(coverage)
