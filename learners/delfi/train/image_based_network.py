#! /usr/bin/env python

from __future__ import print_function, division

# coding: utf-8

# In[ ]:


# Executed on Python3
# Requires Keras

import time
start_time = time.time()
print("START TIME", start_time)

from image_based_network_classes import HPOCallback, SubmodelCheckpoint, DataGenerator, dump_accuracy
from image_based_network_functions import clean_instance_name, readCSV, readCSVS, readSplits, readData, convert_labels, get_sample_weight
from image_based_network_functions import get_callbacks, get_data_folds
from image_based_network_functions import LABEL_TYPE_TIME, LABEL_TYPE_LOG, LABEL_TYPE_NORMALIZED, LABEL_TYPE_DISCRETE, LABEL_TYPE_BINARY, LABEL_TYPE_AGILE, LABEL_TYPES
from image_based_network_functions import SAVE_MODEL_CHOICES, SAVE_METRIC_CHOICES

from keras import regularizers
from keras.models import Sequential
from keras.layers import Dense, Dropout, Activation
from keras.optimizers import SGD, Adam
from keras.engine.topology import Input
from keras.engine.training import Model
from keras.layers import InputLayer, Reshape
from keras.layers.convolutional import Conv2D, SeparableConv2D
from keras.layers.core import Activation, Dropout, Flatten, Dense
from keras.layers.normalization import BatchNormalization
from keras.layers.pooling import MaxPooling2D
from keras.regularizers import l2
from keras.preprocessing.image import ImageDataGenerator
import keras

# Requires Pandas, Numpy, matplotlib etc.
import numpy as np
import pandas as pd
from PIL import Image

import argparse
import copy
import io
import json
import math
import os
import random
import sys
import zipfile

TIMEOUT = 1800
CUTOFF_VALUE_RATIO = 2
CUTOFF_VALUE = TIMEOUT * CUTOFF_VALUE_RATIO



# DEFAULT ACTIVATIONS FOR LABEL TYPES
DEFAULT_ACTIVATION_FUNCTIONS = {
    LABEL_TYPE_TIME: "linear",
    LABEL_TYPE_LOG: "linear",
    LABEL_TYPE_NORMALIZED: "linear",
    LABEL_TYPE_DISCRETE: "sigmoid",
    LABEL_TYPE_BINARY: "sigmoid",
    LABEL_TYPE_AGILE: "sigmoid"
}
DEFAULT_LOSS_FUNCTIONS = {
    LABEL_TYPE_TIME: "mean_squared_error",
    LABEL_TYPE_LOG: "mean_squared_error",
    LABEL_TYPE_NORMALIZED: "mean_squared_error",
    LABEL_TYPE_DISCRETE: "binary_crossentropy",
    LABEL_TYPE_BINARY: "binary_crossentropy",
    LABEL_TYPE_AGILE: "binary_crossentropy"
}

OPTIMIZERS = ["adam", "sgd"]
DEFAULT_OPTIMIZERS = {
    LABEL_TYPE_TIME: "adam",
    LABEL_TYPE_LOG: "adam",
    LABEL_TYPE_NORMALIZED: "adam",
    LABEL_TYPE_DISCRETE: "sgd",
    LABEL_TYPE_BINARY: "sgd",
    LABEL_TYPE_AGILE: "sgd"
}
MINIMIZING_LABELS = set([LABEL_TYPE_TIME, LABEL_TYPE_LOG, LABEL_TYPE_NORMALIZED, LABEL_TYPE_DISCRETE])



parser = argparse.ArgumentParser()
# Define input sources
parser.add_argument("-i", "--input", type=str, action="store", default=None,
                    help="Path to the directory containing ALL required input data. If not specified, the directory in"
                         "$DATA_DIR will be used.")
parser.add_argument("-d", "--data", type=str, action="store", default=".",
                    help="Subdirectory path within '--input' to the image files. If not given, the base directory will "
                         "be used. Alternatively, this can be a zip archive containing the data.")
parser.add_argument("--csv", type=str, action="append", required=True,
                    help="Relative path to a CSV file within the base directory. The CSV file describes"
                         " the runtimes of planners for the input data. Multiple CSV "
                         "files can be provided, no duplicate pruning is done on the given instances and the planners "
                         "described have to match.")
parser.add_argument("--split", type=str, action="append", required=True,
                    help="Relative path within the base directory."
                         " The a file describing the the names of the problems in the training, validation, and "
                         "test set. This argument can be given multiple times causing the network to be trained once "
                         "for each given split and at the end report an average performance over the runs (use case:"
                         "HPO)")
parser.add_argument("--parallelize-folds", action="store_true",
                    help="If set, then all folds specified with '--split' are merged to a single network model consisting"
                         "of multiple independent models. This allows training them in parallel on a single GPU. The "
                         "models are still differently initialized and their batches are independently shuffled.")
parser.add_argument("--multi-gpu", action="store", type=int, default=None,
                    help="Applies conversion to multi gpu model. Requires '--parallelize-folds")

# Define output properties
parser.add_argument("-o", "--output", type=str, action="store", default=None,
                    help="Path to the directory for the output data. If not specified, the directory in ${RESULT_DIR} "
                         "will be used.")
parser.add_argument("-op", "--output-prefix", type=str, action="store", default="",
                    help="Prefix to add before every output item's name generated by this script."
                         "(unless the file name has to be fixed for another purpose or is temporary)")
parser.add_argument("-os", "--output-suffix", type=str, action="store", default="",
                    help="Suffix to add behind every output item's name (before the file type) generated by this script "
                         "(unless the file name has to be fixed for another purpose or is temporary).")
parser.add_argument("--save-model", choices=SAVE_MODEL_CHOICES, action="append", default=[],
                    help="Save some additional models during training.")

# Manage loaded data
parser.add_argument("-ft", "--full-training", action="store_true",
                    help="Merges all sets together and trains on the whole data. If this is used with "
                         "'--random-validation', then this is executed first.")
parser.add_argument("-rv", "--random-validation", action="store", nargs=2, default=None, type=float,
                    help="Merges train and validation set and does a new random split by the ratio given as parameters"
                         "('--random-validation [training weight] [validation weight]'). If this is used with "
                         "'--full-training', then '--full-training' is executed first.")
parser.add_argument("-lt", "--label-type", choices=LABEL_TYPES, default=LABEL_TYPE_BINARY, #default="binary",
                    help="How to modify the data labels for training:\n"
                         "    time: uses the original runtimes as labels\n"
                         "    log: uses the log10 of the original time. Times < 0.01s are clipped to 0.01s"
                         "    normalized: normalizes all times in the range 0 till 1\n"
                         "    binary: converts the labels to 0 unsolved and 1 solves task\n"
                         "    agile: 1 if solves problem within 5 * minimum solving time else 0")
parser.add_argument("--planner-selection", type=str, action="store", nargs="*", default=None,
                    help="Modify the planners used in the label. Provide any number of additional arguments. Additional"
                         "arguments can be:\n"
                         "    - an integer in 0..Max_Num_Planners\n"
                         "    - i:j defines a range of integers from i (included) and j (excluded)\n"
                         "    - i:j:k like i:j, but with step size k"
                         "The label is modified with the planners in the order defined by this argument! A planner"
                         "can be named multiple times, then it appears multiple times in the label")
parser.add_argument("--data-weighting", action="store", type=float, nargs="?", default=False,
                    help="Scales the occurrences of problems solve by X planners up such that those cases are equally"
                         "probable to the case of solved by Y planners. If additionally a maximum weight is given (as "
                         "float value), then the minimum weight all weights are scaled between 1 and Max Weight")

parser.add_argument("--timelimit", type=float, default=TIMEOUT, help="Timelimit for executing a planner")

# Manage training parameters
parser.add_argument("--epochs", type=int, action="store", default=250)
parser.add_argument("--best", action="store", choices=SAVE_METRIC_CHOICES, default=None,
                    help="Does not return the last during training encountered network, but the best one.")
parser.add_argument("--convolution-filters", type=int, action="store", default=128,
                    help="Number of convolution filters in the first layer (if 0, then the pooling is also skipped)[>=0]")
parser.add_argument("--dense-layers", type=int, action="store", default=1,
                    help="Number of dense layers AFTER the convolution layers [>= 1")
parser.add_argument("--dense-kernel-regularizer", type=float, action="store", default=None)
parser.add_argument("--dense-activity-regularizer", type=float, action="store", default=None)
parser.add_argument("--loss-function", type=str, action="store", default=None,
                    help="Name of a Keras loss function")
parser.add_argument("--activation-last", choices=["linear", "relu", "sigmoid", "tanh", "softmax"], default=None,
                    help="Change the activation function in the final layer. If not given, the activation function is"
                         "automatically adapted to given label type:\n"
                         "  time -> None, log -> none normalized -> sigmoid, binary -> sigmoid, agile -> sigmoid")
parser.add_argument("--optimizer", action="store", choices=OPTIMIZERS, default=None,
                    help="Optimizer to use")


def parse_argv(argv):
    options = parser.parse_args(argv)

    # Check Input Directory
    if options.input is None:
        if "DATA_DIR" not in os.environ:
            raise ValueError("Either an input directory has to be specified or the ${DATA_DIR} variable has to be set.")
        else:
            options.input = os.environ["DATA_DIR"]
    if not os.path.isdir(options.input):
        raise ValueError("The provided input directory is not a valid directory: %s" % options.input)

    # Check Output Directory
    if options.output is None:
        if "RESULT_DIR" not in os.environ:
            raise ValueError("Either an output directory has to be specified or the ${RESULT_DIR} variable has to be set.")
        else:
            options.output = os.environ["RESULT_DIR"]
    if not os.path.isdir(options.output):
        os.makedirs(options.output)
        #raise ValueError("The provided output directory is not a valid directory: %s" % options.output)

    # Check Data Directory Within Input Directory
    if not os.path.exists(os.path.join(options.input, options.data)) or (os.path.isfile(os.path.join(options.input, options.data)) and not options.data.endswith(".zip")):
        raise ValueError("The provided data directory within the base directory does not exist or is neither a directory nor a zip archive: %s" % options.data)

    # Check CSV Files
    for csv in options.csv:
        if not os.path.exists(os.path.join(options.input, csv)):
            raise ValueError("A given CSV file does not exist: %s" % csv)

    # Check Split Files
    for split in options.split:
        if not os.path.exists(os.path.join(options.input, split)):
            raise ValueError("A given split file does not exist: %s" % split)

    # With a single split nothing can be parallelized
    if len(options.split) == 1:
        options.parallelize_folds = False

    if options.planner_selection is not None:
        planners = []
        for item in options.planner_selection:
            double_point_count = item.count(":")
            if double_point_count == 0:
                try:
                    planners.append(int(item))
                except ValueError:
                    raise ValueError("Unable to convert a given planner index to an integer: %s" %item)
            if double_point_count > 0:
                parts = item.split(":")
                if len(parts) < 2 or len(parts) > 3:
                    raise ValueError("Invalid planner index sequence. Required format start:end[:step]. Given %s" % item)
                try:
                    for idx, part in enumerate(parts):
                        parts[idx] = int(part)
                except ValueError:
                    raise ValueError("Unable to convert a given item in a planner range to an integer: %s" % item)
                start = parts[0]
                end = parts[1]
                step = 1 if len(parts) == 2 else parts[2]

                if (start < end and step <= 0) or (start > end and step >= 0):
                    raise ValueError("The given planner index sequence is infinite: %s" % item)

                current = start
                while (current < end and step > 0) or (current > end and step < 0):
                    planners.append(current)
                    current += step
        options.planner_selection = np.array(planners)

    if options.parallelize_folds:
        if options.data_weighting is not False:
            raise ValueError("Cannot use option '--parallelize-folds' and '--data-weighting' together. Please train"
                             "them consecurively.")
    return options
# In[ ]:


# In[ ]:

print("Call string:", sys.argv)
options = parse_argv(sys.argv[1:])

TIMEOUT = options.timelimit
CUTOFF_VALUE_RATIO = 2
CUTOFF_VALUE = TIMEOUT * CUTOFF_VALUE_RATIO


def _get_path_model_checkpoint(round_prefix=None, suffix=None):
    return os.path.join(options.output,"tmp_%smodel_checkpoint%s.h5" % (round_prefix, "" if suffix is None else suffix))


def _get_path_model(round_prefix=None, specializer=None):
    return os.path.join(options.output, options.output_prefix + round_prefix + ("" if specializer is None else (specializer + "_")) + "model%s" % options.output_suffix)


def _get_path_model_architecture(round_prefix=None, specializer=None):
    return _get_path_model(round_prefix, specializer) + ".json"


def _get_path_model_weights(round_prefix=None, specializer=None):
    return _get_path_model(round_prefix, specializer) + ".h5"



"""------------------------------------- Define Network Parameters --------------------------------------------------"""
# In[ ]:

## Default Parameters
#Adam Optimizer
adam_learning_rate = 0.001
adam_beta_1 = 0.99
adam_beta_2 = 0.999
adam_epsilon = 1e-8

#SGD Optimizer
decay = 0.005
learning_rate = 0.098
momentum = 0.95
nesterov = True

#Network Architecture
assert options.activation_last is not None or options.label_type in DEFAULT_ACTIVATION_FUNCTIONS, "Internal error: No default activation registered, but required for the label type: %s" % options.label_type
activation_last = options.activation_last if options.activation_last is not None else DEFAULT_ACTIVATION_FUNCTIONS[options.label_type]
conv_filter_count = options.convolution_filters
conv_filter_size1 = 5
dense_activity_regularizer = options.dense_activity_regularizer
dense_kernel_regularizer = options.dense_kernel_regularizer
dense_layers = options.dense_layers
dropout_rate = 0.5
pool_filter_size = 2

#Training
batch_size = 100
epochs = options.epochs
loss_function = DEFAULT_LOSS_FUNCTIONS[options.label_type] if options.loss_function is None else options.loss_function
optimizer = DEFAULT_OPTIMIZERS[options.label_type] if options.optimizer is None else options.optimizer
save_best = options.best


config_file_name = "config.json"
# Check if file exists
if os.path.isfile(config_file_name):
    with open(config_file_name, 'r') as f:
        json_obj = json.load(f)
        # Adam Optimizer
        adam_learning_rate = json_obj.get("adam_learning_rate", adam_learning_rate)
        adam_beta_1 = json_obj.get("adam_beta_1", adam_beta_1)
        adam_beta_2 = json_obj.get("adam_beta_2", adam_beta_1)
        adam_epsilon = json_obj.get("adam_epsilon", adam_epsilon)

        # SGD Optimizer
        decay = json_obj.get("decay", decay)
        learning_rate = json_obj.get("learning_rate", learning_rate)
        momentum = json_obj.get("momentum", momentum)
        int_nesterov = json_obj.get("nesterov", nesterov)
        if int_nesterov == 1:
            nesterov = True
        else:
            nesterov = False

        # Network Architecutre
        activation_last = json_obj.get("activation_last", activation_last)
        conv_filter_count = json_obj.get("conv_filter_count", conv_filter_count)
        conv_filter_size1 = json_obj.get("conv_filter_size1", conv_filter_size1)
        dense_activity_regularizer = json_obj.get("dense_activity_regularizer", dense_activity_regularizer)
        dense_kernel_regularizer = json_obj.get("dense_kernel_regularizer", dense_kernel_regularizer)
        dropout_rate = json_obj["dropout_rate"]
        pool_filter_size = json_obj.get("pool_filter_size", pool_filter_size)

        # Training
        batch_size = json_obj.get("batch_size", batch_size)
        epochs = json_obj.get("epochs", epochs)
        loss_function = json_obj.get("loss_function", save_best)
        optimizer = json_obj.get("obtimizer", optimizer)
        save_best = json_obj.get("best", save_best)






        dense_layers = json_obj.get("dense_layers", dense_layers)


        print("learning rate is ", learning_rate)
        print("conv_filter_size1 is ", conv_filter_size1)
        print("pool_filter_size is ", pool_filter_size)
        print("batch_size is ", batch_size)
        print("momentum is ", momentum)
        print("nesterov is ", nesterov)
        print("dropout rate is ", dropout_rate)

assert conv_filter_count >= 0
assert dense_layers >= 1

if optimizer == "sgd":
    optimizer = SGD(lr=learning_rate, decay=decay, momentum=momentum, nesterov=nesterov)
elif optimizer == "adam":
    optimizer = Adam(lr=adam_learning_rate, beta_1=adam_beta_1, beta_2=adam_beta_2, epsilon=adam_epsilon)


"""----------------------------------------- Define Network Architecture --------------------------------------------"""
def get_model(x_shape, y_shape, hidden_layer_size, label_type):
    output_size = 1
    for x in y_shape:
        output_size *= x

    input_layer = Input(shape=x_shape)
    layer = input_layer
    if conv_filter_count > 0:
        layer = Conv2D(conv_filter_count, kernel_size=(conv_filter_size1, conv_filter_size1), strides=(1, 1), activation='relu')(layer)
        layer = MaxPooling2D(pool_size=(pool_filter_size, pool_filter_size), strides=(2, 2))(layer)

    if conv_filter_count == 0:
        layer = Flatten(input_shape=x_shape)(layer)
    else:
        layer = Flatten()(layer)

    for i in range(dense_layers):
        final_layer = i == dense_layers - 1
        layer = Dropout(dropout_rate)(layer)
        layer = Dense(output_size if final_layer else hidden_layer_size,
                      activation=activation_last if final_layer else "relu",
                      kernel_regularizer=None if dense_kernel_regularizer is None else regularizers.l2(dense_kernel_regularizer),
                      activity_regularizer=None if dense_activity_regularizer is None else regularizers.l1(dense_activity_regularizer))(layer)

    if label_type == LABEL_TYPE_DISCRETE:
        layer = Reshape(y_shape)(layer)

    model = Model(inputs=input_layer, outputs=layer)
    return model, input_layer, layer


"""------------------------------------------------ Load Data -------------------------------------------------------"""

# Read runtimes (a Problems X Solvers matrix), list of solver names, list of problem names
# '--planner-selection' can modify the size of Solvers!
labels, list_solver_names, list_instance_names = readCSVS(options, timeout=TIMEOUT, timeout_value=CUTOFF_VALUE)
# Read instances in training, validation, test set
rounds_splits = readSplits(options)
# Read feature data
data, filter_exists = readData(options, list_instance_names)
labels, list_instance_names = labels[filter_exists], list_instance_names[filter_exists]

# Convert labels for training
original_labels = labels.copy()
labels = convert_labels(options.label_type, labels.copy(), TIMEOUT)


"""---------------------------------- Generate Models and Data for each Fold ----------------------------------------"""
fold_resources = []
for no, (training_set_instances, validation_set_instances, test_set_instances) in enumerate(rounds_splits):
    #print("Training Round %i/%i" % (no, len(rounds_splits)))
    round_prefix = "" if len(rounds_splits) == 1 else ("round-%i-%i_" % (no + 1, len(rounds_splits)))
    round_data = get_data_folds(options, list_instance_names, list_solver_names, data, labels, original_labels,
                                training_set_instances, validation_set_instances, test_set_instances, TIMEOUT)
    # In[ ]:
    model, input_layer, output_layer = get_model(round_data[0][0].shape[1:], round_data[0][1].shape[1:], int(round_data[0][0].shape[1] ** (1.7)), options.label_type)
    model.compile(loss=loss_function, optimizer=optimizer)

    fold_resources.append((round_prefix, round_data, model, input_layer, output_layer))


if not options.parallelize_folds:
    rounds_hpo_accuracies = []
    for idx_fold, (round_prefix, data, model, _, _) in enumerate(fold_resources):
        """-------------------------------------- Execute next fold  ------------------------------------------------"""
        (x_train, y_train, y_train_times), (x_valid, y_valid, y_valid_times), (x_test, y_test, y_test_times), train_sample_weight = data
        callbacks = get_callbacks(fold_resources, idx_fold, rounds_hpo_accuracies, save_best, options.save_model,
                                  options.output, _get_path_model_checkpoint, _get_path_model_weights)
        original_model = model
        #if options.multi_gpu is not None:
        #    model = keras.utils.multi_gpu_model(model, options.multi_gpu)
        model.compile(loss=loss_function, optimizer=optimizer)
        if len(x_valid) == 0:
            model.fit(x_train, y_train, epochs=epochs, batch_size=batch_size, verbose=1,
                      callbacks=callbacks, shuffle=True, sample_weight=train_sample_weight)
        else:
            model.fit(x_train, y_train, epochs=epochs, batch_size=batch_size, verbose=1, validation_data=(x_valid, y_valid),
                      callbacks=callbacks, shuffle=True, sample_weight=train_sample_weight)

        # In[ ]:
        model = original_model
        if save_best is not None:
            model.load_weights(_get_path_model_checkpoint(round_prefix))

        if len(test_set_instances) > 0:

            # For each test data point compute predictions for each of the solvers
            preds = model.predict(x_test)
            print(preds)

        """------------------------------------- Store Fold Results -------------------------------------------------"""
        # serialize model to JSON
        model_json = model.to_json()
        with open(_get_path_model_architecture(round_prefix), "w") as json_file:
            json_file.write(model_json)
        # serialize weights to HDF5
        model.save_weights(_get_path_model_weights(round_prefix))
        print("Saved model to disk")

    """------------------------------------ Store Average HPO Value -------------------------------------------------"""
    if len(options.split) > 1:
        assert all(len(round_hpo_accuracies) == len(rounds_hpo_accuracies[0]) for round_hpo_accuracies in rounds_hpo_accuracies), "Different fold rounds have different number of epochs. Cannot merge hpo accuracies"
        averages = [sum([rounds_hpo_accuracies[idx_round][idx_epoch]["accuracy"] for idx_round in range(len(rounds_hpo_accuracies))]) / len(rounds_hpo_accuracies)
                    for idx_epoch in range(len(rounds_hpo_accuracies[0]))]
        average_hpo_accuracy = [{"steps": epoch + 1, "accuracy": avg} for epoch, avg in enumerate(averages)]
        dump_accuracy(options.output, average_hpo_accuracy, average_hpo_accuracy)

else:

    input_layers = [x[3] for x in fold_resources]
    output_layers = [x[4] for x in fold_resources]
    model = Model(inputs=input_layers, outputs=output_layers)
    if options.multi_gpu is not None:
        model = keras.utils.multi_gpu_model(model, options.multi_gpu)
    model.compile(loss=loss_function, optimizer=optimizer)

    x_trains, y_trains = zip(*[(train[0], train[1]) for _, (train, _, _, _), _, _, _ in fold_resources])
    x_valids, y_valids = zip(*[(valid[0], valid[1]) for _, (_, valid, _, _), _, _, _ in fold_resources])
    x_tests, y_test = zip(*[(test[0], test[1]) for _, (_, _, test, _), _, _, _ in fold_resources])
    min_valid_size = min([len(x) for x in x_valids])

    callbacks = get_callbacks(fold_resources, None, [], save_best, options.save_model,
                              options.output, _get_path_model_checkpoint, _get_path_model_weights)

    generator_train = DataGenerator(x_trains, y_trains, batch_size, shuffle=True)
    if min_valid_size == 0:
        model.fit_generator(generator_train, epochs=epochs, verbose=1, callbacks=callbacks, validation_data=None,
                            validation_steps=1, class_weight=None, workers=8, max_queue_size = 50, shuffle=True)
    else:
        generator_valid = DataGenerator(x_valids, y_valids, min_valid_size, shuffle=True)
        model.fit_generator(generator_train, epochs=epochs, verbose=1, callbacks=callbacks, validation_data=generator_valid,
                            validation_steps=1, class_weight=None, workers=8, max_queue_size = 50, shuffle=True)

    # In[ ]:
    if save_best is not None:
        for round_prefix, _, sub_model, _, _ in fold_resources:
            sub_model.load_weights(_get_path_model_checkpoint(round_prefix))

    for idx_fold in range(len(fold_resources)):
        if len(x_tests[idx_fold]) > 0:
            preds = fold_resources[idx_fold][2].predict(x_tests[idx_fold])
            print(fold_resources[idx_fold][0], ":",preds)


    # Store model
    for round_prefix, _, sub_model, _, _ in fold_resources:
        # serialize model to JSON
        model_json = model.to_json()
        with open(_get_path_model_architecture(round_prefix), "w") as json_file:
            json_file.write(sub_model.to_json())
        # serialize weights to HDF5
        sub_model.save_weights(_get_path_model_weights(round_prefix))
        print("%s saved model to disk" % round_prefix)

end_time = time.time()
print("Total Time", end_time - start_time)
# jbsub -cores 8+1 -mem 20g -out out_local.txt python PlannerPrediction.py --dataDir ${DATA_DIR} 1 full_lifted.csv 2018-02-19-images-lifted





'''
# Formerly after training on the predictions of the test data
random.seed(1234777)

# Selecting strategies
instances_unsolved_prediction = 0
instances_unsolved_random = 0
instances_unsolved_oracle = 0

instances_time_prediction = 0
instances_time_random = 0
instances_time_oracle = 0

instances = x_test.shape[0]
print("Number of instances: " + str(instances))

for k in [1, 2, 3, 5, 10]:
    # Loop over rows in x_test
    for row_id in range(0, x_test.shape[0]):
        assert (x_test.shape[0] == preds.shape[0])

        # Predicted selection
        # Select solver with highest predicted probability of solving
        selected_solver_id_predicted = np.nanargmin(preds[row_id]) if options.label_type in MINIMIZING_LABELS else np.nanargmax(preds[row_id])
        runtime = y_test_times[row_id][selected_solver_id_predicted]

        if runtime >= CUTOFF_VALUE:
            instances_unsolved_prediction += 1
        instances_time_prediction += runtime

        # Random selection
        # -- One should do 10 rounds and average
        selected_solver_id_random = random.randint(0, len(list_solver_names) - 1)
        runtime = y_test_times[row_id][selected_solver_id_random]
        if runtime >= CUTOFF_VALUE:
            instances_unsolved_random += 1
        instances_time_random += runtime

        # Oracle selection
        selected_solver_id_oracle = np.argmin(y_test_times[row_id])
        runtime = y_test_times[row_id][selected_solver_id_oracle]
        if runtime >= CUTOFF_VALUE:
            instances_unsolved_oracle += 1
        instances_time_oracle += runtime

    print()
    print("[PREDICTED] Instance unsolved in %: " + str(instances_unsolved_prediction / float(instances) * 100))
    print("[SINGLEBEST] Instance unsolved in %: " + str(list_solver_unsolved[best_solver_solved] / float(instances) * 100))
    print("[RANDOM] Instance unsolved in %: " + str(instances_unsolved_random / float(instances) * 100))
    print("[ORACLE] Instance unsolved in %: " + str(instances_unsolved_oracle / float(instances) * 100))
    print()
    print("[PREDICTED] Average Runtime: " + str(instances_time_prediction / float(instances)))
    print("[SINGLEBEST] Average Runtime: " + str(list_solver_average[best_solver_average]))
    print("[RANDOM] Average Runtime: " + str(instances_time_random / float(instances)))
    print("[ORACLE] Average Runtime: " + str(instances_time_oracle / float(instances)))
'''
