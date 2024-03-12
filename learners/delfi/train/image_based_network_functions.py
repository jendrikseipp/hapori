from __future__ import print_function, division
import math
import random

from image_based_network_classes import HPOCallback, SubmodelCheckpoint


from keras.callbacks import ModelCheckpoint

import io
import numpy as np
import os
import pandas as pd
from PIL import Image
import zipfile

# LABEL_TYPES
MIN_VALUE_LOG_LABELS = 0.01
DISCRETE_INTERVALS = 3
LABEL_TYPE_TIME = "time"
LABEL_TYPE_LOG = "log"
LABEL_TYPE_NORMALIZED  = "normalized"
LABEL_TYPE_DISCRETE = "discrete"
LABEL_TYPE_BINARY = "binary"
LABEL_TYPE_AGILE = "agile"
LABEL_TYPE_SATISFICING = "sat"
LABEL_TYPES = [LABEL_TYPE_TIME, LABEL_TYPE_LOG, LABEL_TYPE_NORMALIZED, LABEL_TYPE_DISCRETE, LABEL_TYPE_BINARY, LABEL_TYPE_AGILE, LABEL_TYPE_SATISFICING]



# Save Model Choices
SAVE_MODEL_BEST_TRAIN = "best_train"
SAVE_MODEL_BEST_VALID = "best_valid"
#SAVE_MODEL_FINAL = "final_model"
SAVE_MODEL_CHOICES = [SAVE_MODEL_BEST_TRAIN, SAVE_MODEL_BEST_VALID]
SAVE_MODEL_CHOICES_TO_METRIC = {"best_train": "loss", "best_valid": "val_loss"}

SAVE_METRIC_BEST_LOSS = "loss"
SAVE_METRIC_BEST_VAL_LOSS = "val_loss"
SAVE_METRIC_CHOICES = [SAVE_METRIC_BEST_LOSS, SAVE_METRIC_BEST_VAL_LOSS]
SAVE_METRIC_TO_MODEL_CHOICE = dict([(y, x) for x, y in SAVE_MODEL_CHOICES_TO_METRIC.items()])
SAVE_METRIC_TO_DATA_INDEX = {"loss": 0, "val_loss": 1}  # Same index as the data arrays are sorted in fold_resources




def clean_instance_name(name):
    for prefix in ["processing"]:
        if name.startswith(prefix):
            name = name[len(prefix):]
            name = name.strip()

    idx = name.find(".pddl")
    if idx == -1:
        raise ValueError("Unable to clean name of instance without '.pddl' to mark the end of the instance name: %s" % name)
    return name[:idx+5]


def readCSV(path_csv):
    # Parse performance data and load images to array
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


def readCSVS(options, timeout, timeout_value):
    data = []
    solvers = None
    instances = []

    # Load data of each CSV file
    for csv in options.csv:
        path_data = os.path.join(options.input, csv)
        new_runtimes, new_solvers, new_instances = readCSV(path_data)
        if solvers is None:
            solvers = new_solvers
        if new_solvers != solvers:
            raise ValueError("Given CSV files contain different planners: %s" % csv)
        instances.extend([clean_instance_name(x) for x in new_instances])
        data.append(new_runtimes)

    # Merged loaded data into numpy arrays
    data = np.concatenate(data)
    data[data == "-"] = timeout_value
    data = data.astype(float)
    data[data > timeout] = timeout_value
    data[np.isnan(data)] = timeout_value
    solvers, instances = np.array(solvers), np.array(instances)

    # Sort arrays
    idx_sort = np.argsort(instances)
    data = data[idx_sort,:]
    instances = instances[idx_sort]

    # Modify planner list/labels
    if options.planner_selection is not None:
        solvers = solvers[options.planner_selection]
        data = data[:, options.planner_selection]
    return data, solvers, instances


def readSplits(options):
    splits = []
    for file in options.split:
        training, validation, test = set(), set(), set()
        modes = [("train", training), ("training", training),
                 ("valid", validation), ("validation", validation),
                 ("test", test)]

        path = os.path.join(options.input, file)
        mode_set = None

        with open(path, "r") as f:
            for line in f.readlines():
                line = line.strip()
                # Test whether the set to which the problems are added has to be changed
                new_set = False
                for pos_mode, pos_set in modes:
                    if line in [pos_mode, "#%s" % pos_mode, "| %s |" % pos_mode]:
                        mode_set = pos_set
                        new_set = True
                        break
                # Skip line
                if line == "" or new_set or line.startswith("+") or line.startswith("#"):
                    continue
                # Actual problem name
                mode_set.add(clean_instance_name(line))

        # Warnings if instances are in multiple sets
        for s1 in [training, validation, test]:
            for s2 in [training, validation, test]:
                if s1 is s2:
                    continue
                if len(s1 & s2) > 0:
                    print("Two of the problem sets (training, validation, test) have an intersecting set of problems.", file=sys.stderr)

        splits.append((training, validation, test))

    return splits


def readData(options, list_instance_names):
    data = []# np.ndarray(shape=(len(list_instance_names)))
    path_image = None
    zip_archive = None
    if os.path.isfile(os.path.join(options.input, options.data)) and options.data.endswith(".zip"):
        zip_archive = zipfile.ZipFile(os.path.join(options.input, options.data), "r")
        zip_archive_elements = set(zip_archive.namelist())
        zip_dirs = [x for x in zip_archive_elements if x.endswith("/")]
        if len(zip_dirs) > 1:
            raise ValueError("The given zip archive contains multiple directories. The data is only allowed to be "
                             "directly in the archive or in a single directory on the root level.")
        zip_root = "" if len(zip_dirs) == 0 else zip_dirs[0]


    filter_exists = np.full(len(list_instance_names), True)
    for no, name in enumerate(list_instance_names):
        # Get right file path...The second suffix should only be in the data set name not in the file names...
        for suffix in [".png", "-bolded-cs.png", None]:
            if suffix is None:
                filter_exists[no] = False
                break
                #raise ValueError("Unable to locate the input data for a required instance: %s" % name)

            if zip_archive is None:
                path_image = os.path.join(options.input, options.data, name + suffix)
                if os.path.exists(path_image):
                    break
                domain_name, problem_name = name.rsplit(":", 1)
                path_image = os.path.join(options.input, options.data,
                                          domain_name, problem_name + suffix)
                if os.path.exists(path_image):
                    break
            else:
                path_image = zip_root + name + suffix
                if path_image in zip_archive_elements:
                    break
        if filter_exists[no]:
            if zip_archive is None:
                data.append(np.array(Image.open(path_image)))
            else:
                zip_open = zip_archive.open(path_image)
                bio = io.BytesIO(zip_open.read())
                zip_open.close()
                data.append(np.array(Image.open(bio)))

    data = np.array(data, dtype="float") / 255.0

    if zip_archive is not None:
        zip_archive.close()

    return data, filter_exists


def convert_labels(type, labels, timeout):

    MIN_VALUE = 0  # np.nanmin(labels)
    RANGE_VALUES = timeout  # np.nanmax(labels) - MIN_VALUE

    if type == LABEL_TYPE_TIME:
        pass
    elif type == LABEL_TYPE_LOG:
        labels[labels < MIN_VALUE_LOG_LABELS] = MIN_VALUE_LOG_LABELS
        labels = np.log10(labels)
    elif type == LABEL_TYPE_NORMALIZED:
        labels = (labels - MIN_VALUE)/RANGE_VALUES
    elif type == LABEL_TYPE_DISCRETE:
        assert DISCRETE_INTERVALS >= 1
        nb_classes = DISCRETE_INTERVALS + 1
        interval_size = timeout / float(DISCRETE_INTERVALS)
        new_labels = np.full(labels.shape + (nb_classes,), 0)
        for c in range(DISCRETE_INTERVALS):
            filter_lower = labels > c * interval_size
            filter_upper = labels <= (c + 1) * interval_size
            filter_class = np.logical_and(filter_lower, filter_upper)
            new_labels[:,:, c][filter_class] = 1
        filter_timeout = labels > timeout
        new_labels[:,:, nb_classes - 1][filter_timeout] = 1
        labels = new_labels

    elif type == LABEL_TYPE_BINARY:
        labels[labels <= timeout] = 1
        labels[labels > timeout] = 0
    elif type == LABEL_TYPE_AGILE:
        for row_id in range(0, labels.shape[0]):
           min_value = np.nanmin(labels[row_id,:])
           filter = labels[row_id] < min(min_value * 5, timeout)
           labels[row_id][:] = 0
           labels[row_id][filter] = 1
    elif type == LABEL_TYPE_SATISFICING:
        # Similar to normalize
        labels = (1.0 - labels)
    else:
        assert False

    return labels

def get_sample_weight(labels, weighting, timeout):
    nan_labels = labels.copy()
    nan_labels[nan_labels > timeout] = np.nan
    clazz = (~np.isnan(nan_labels)).sum(axis=1)
    values, counts = np.unique(clazz, return_counts=True)
    clazz_weights = np.ndarray(shape=(labels.shape[1] + 1))  # 0..N Planners case weights
    min_weight = labels.shape[0]
    for v, c in zip(values, counts):
        clazz_weights[v] = labels.shape[0]/c
        min_weight = min(min_weight, clazz_weights[v])
    clazz_weights /= min_weight

    if weighting is not None:
        filter_zero = clazz_weights == 0
        clazz_weights = ((clazz_weights - 1) / ((clazz_weights.max() - 1)/(weighting - 1))) + 1
        clazz_weights[filter_zero] = 0  # the first -1 causes the 0 to change, this is now fixed (but zeros happen is
                                        # no sample exists, thus this has no effect, but for completness

    weights = clazz_weights[clazz]
    return weights



def get_callbacks(fold_resources, current_fold, rounds_hpo_accuracies, save_best, save_models, output_dir, _get_path_model_checkpoint, _get_path_model_weights):
    """

    :param fold_resources:
    :param current_fold: if index given, make callback for this fold
                         else if None make Callbacks for all folds executed in parallel
                         else assert False
    :return:
    """
    assert current_fold is None or 0 <= current_fold < len(fold_resources), "Invalid current_fold value: %i/%i" %(current_fold, len(fold_resources))
    callbacks = []

    def _add_checkpoint(idx_fold, path, metric, model):
        if current_fold is not None:
            callbacks.append(ModelCheckpoint(path, monitor=metric, save_best_only=True, save_weights_only=True))
        else:
            idx_data = SAVE_METRIC_TO_DATA_INDEX.get(metric, None)
            assert idx_data is not None, "Invalid save_best value: %s" % save_best
            data = fold_resources[idx_fold][1][idx_data]
            callbacks.append(SubmodelCheckpoint(model, data[0], data[1], path))

    def _add_checkpoints(idx_fold):
        round_prefix = fold_resources[idx_fold][0]
        model = fold_resources[idx_fold][2]

        if save_best is not None:
            _add_checkpoint(idx_fold, _get_path_model_checkpoint(round_prefix), save_best, model)

        for save_model in save_models:
            metric = SAVE_MODEL_CHOICES_TO_METRIC.get(save_model, None)
            assert metric is not None, "Invalid --save-model arguments: %s" % save_model
            _add_checkpoint(idx_fold, _get_path_model_weights(round_prefix, save_model), metric, model)

    evaluate_hpo = []
    for idx_fold in (range(len(fold_resources)) if current_fold is None else [current_fold]):
        _add_checkpoints(idx_fold)
        (x_valid, y_valid, _) = fold_resources[idx_fold][1][1]
        if len(x_valid) > 0:
            evaluate_hpo.append((fold_resources[idx_fold][2], x_valid, y_valid))

    # Set HPO Callback
    if len(evaluate_hpo) > 0:
        rounds_hpo_accuracies.append([])
        if current_fold is not None and len(fold_resources) == 1:
            # We have only a single fold => Evaluate HPO on validation data and store it directly
            callbacks.append(HPOCallback(output_dir, rounds_hpo_accuracies[-1], (evaluate_hpo[0][1], evaluate_hpo[0][2])))
        elif current_fold is not None and len(fold_resources) > 1:
            # We have multiple folds and currently process a single one => store performances and average later
            callbacks.append(HPOCallback(None, rounds_hpo_accuracies[-1], (evaluate_hpo[0][1], evaluate_hpo[0][2])))
        elif current_fold is None:
            # We process all folds at once
            callbacks.append(HPOCallback(output_dir, rounds_hpo_accuracies[-1], None, evaluate_hpo))
        else:
            assert False
    return callbacks



"""------------------------------------ Get Data Sets for Training --------------------------------------------------"""
def get_data_folds(options, list_instance_names, list_solver_names, features, labels, original_labels, train, valid, test, timeout):
    filter = np.isin(list_instance_names, np.array([x for x in (train | valid | test)]))
    labels, original_labels, list_instance_names, features = labels[filter], original_labels[filter], list_instance_names[filter], features[filter]

    # In[ ]:

    # Create data sets for keras
    # Merge all data
    if options.full_training:
        train = train | valid | test
        valid, test = set(), set()

    # Create new random split between training and validation data
    if options.random_validation is not None:
        train = train | valid
        size_valid = int(len(train) * options.random_validation[1] / (options.random_validation[0] + options.random_validation[1]))
        valid = set(random.sample(train, size_valid))
        train -= valid

    if len(train) == 0:
        raise ValueError("Training set is empty.")

    # Distribute data over sets
    x_datas = []
    y_datas = []
    y_times = []
    train_sample_weight = None
    for instance_set, is_train in [(train, True), (valid, False), (test, False)]:
        filter_set = np.isin(list_instance_names, np.array([x for x in instance_set]))
        # mask_upscaling = get_mask_upscaling(options, labels[filter_set]) if allow_upscaling else np.full(filter_set.sum(), True)
        if is_train and options.data_weighting is not False:
            train_sample_weight = get_sample_weight(original_labels[filter_set], options.data_weighting, timeout)
        x_datas.append(features[filter_set])
        y_datas.append(labels[filter_set])
        y_times.append(original_labels[filter_set])

    # Shape arrays
    for idx in range(len(x_datas)):
        x_datas[idx] = x_datas[idx].reshape(x_datas[idx].shape[0], 128, 128, 1)

    x_train, x_valid, x_test = x_datas
    y_train, y_valid, y_test = y_datas
    y_train_times, y_valid_times, y_test_times = y_times

    # Compute best single solver in terms of instances solved and average runtime for current fold
    list_solver_unsolved = []
    list_solver_average = []
    for n in range(0, y_train_times.shape[1]):
        list_solver_unsolved.append(0)
        for k in range(0, y_train_times.shape[0]):
            if options.label_type == LABEL_TYPE_SATISFICING:
                if (y_train_times[k, n] == 0.0):
                    list_solver_unsolved[n] += 1
            else:
                if (y_train_times[k, n] > timeout):
                    list_solver_unsolved[n] += 1
        list_solver_average.append(np.average(y_train_times[:, n]))
    print('\nSolver Average ' + "Quality" if options.label_type == LABEL_TYPE_SATISFICING else "Runtimes" + ': ')
    print(list_solver_average)
    print("\nSolver Unsolved Instances: ")
    print(list_solver_unsolved)

    best_solver_solved = np.argmin(list_solver_unsolved)
    print("\nBest Solver-ID in terms of instances solved: " + str(best_solver_solved) + " = " + list_solver_names[
        best_solver_solved])

    if options.label_type == LABEL_TYPE_SATISFICING:
        best_solver_average = np.argmax(list_solver_average)
        print("Best Solver-ID in terms of best average quality: " + str(best_solver_average) + " = " + list_solver_names[best_solver_average])
    else:
        best_solver_average = np.argmin(list_solver_average)
        print("Best Solver-ID in terms of best average runtime: " + str(best_solver_average) + " = " + list_solver_names[best_solver_average])

    return (x_train, y_train, y_train_times), (x_valid, y_valid, y_valid_times), (x_test, y_test, y_test_times), train_sample_weight

