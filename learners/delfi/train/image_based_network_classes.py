import json
import keras
import numpy as np
import os

def dump_accuracy(output_path, train_dict_list, val_dict_list):
    print("\nDumping accuracy")
    with open(os.path.join(output_path, "train_dict_list.json"), "w") as f:
        json.dump(train_dict_list, f)
    with open(os.path.join(output_path, "val_dict_list.json"), "w") as f:
        json.dump(val_dict_list, f)


class HPOCallback(keras.callbacks.Callback):
    def __init__(self, output_path=None, performance_history=None, main_model_test_data=None, evaluate_models=None):
        """
        Evaluates network performance(s) and writes the output for IBM HPO. If multiple models can be
        tested (main_model_test_data is given for the main_model and models in evaluate_models), then
        the final performance is their average.
        :param output_path: If not None, then the path where to write the performance.
        :param performance_history: List to append the performance assessments of None for a fresh list
        :param main_model_test_data: data on which the main model (the model which Keras sets as self.model
                                     for this call back) shall be tested
        :param evaluate_models: List of triple (model, features, labels) to check performance on
        """
        self.performance_history = [] if performance_history is None else performance_history
        self.output_path = output_path
        self.main_model_test_data = main_model_test_data
        self.evaluate_models = [] if evaluate_models is None else evaluate_models

    def eval_performance(self):
        performances = []
        for model, x, y in self.evaluate_models + (
                [] if self.main_model_test_data is None else [(self.model, self.main_model_test_data[0], self.main_model_test_data[1])]):
            performances.append(model.evaluate(x, y, verbose=0))
        return sum(performances)/len(performances)

    def on_epoch_end(self, epoch, logs={}):
        performance = self.eval_performance()
        self.performance_history.append({"steps": str(epoch), "accuracy": performance})
        if self.output_path is not None:
            dump_accuracy(self.output_path, self.performance_history, self.performance_history)
        print('\nTesting loss: {}\n'.format(performance))


class SubmodelCheckpoint(keras.callbacks.Callback):
    def __init__(self, eval_model, x_data, y_data, path_weights=None, path_architecture=None, minimize=True, evaluate_index=None):
        """
        Like Model Checkpoint, just that the model to checkpoint can be given as parameter (and some other features).
        This is useful, when a submodel of the mode lto train shall be checkpointed.
        :param eval_model: model to checkpoint (not the model which keras will set to every checkpoint when training
                           starts)
        :param x_data: feature data for the check
        :param y_data: labels for the check
        :param path_weights: if not None, then the path, where the weights of the best model are stored
        :param save_architecture: if not None, then the path, where the architecture of the best model is stored, save_architecture=False
        :param minimize: the evaluation metric shall be minimized (default True)
        :param evaluate_index: index in the evaluation results of the metric to compare (if no additional metrics are
                               specified during model compilation, then this should be None else loss has index 0,
                                additional metrics defined follow afterwards)
        """
        self._eval_model = eval_model
        self._x_data = x_data
        self._y_data = y_data
        self._path_weights = path_weights
        self._path_architecture = path_architecture
        self._minimize = minimize
        self._index = evaluate_index
        self._history = []
        self._best = None

    def _save_state(self):
        if self._path_weights is not None:
            self._eval_model.save_weights(self._path_weights)

        if self._path_architecture is not None:
            with open(self._path_architecture) as f:
                f.write(self._eval_model.to_json())

    def on_epoch_end(self, epoch, logs={}):
        r = self._eval_model.evaluate(self._x_data, self._y_data)
        if self._index is not None:
            r = r[self._index]
        self._history.append(r)
        if self._best is None or (self._minimize and r < self._best) or ((not self._minimize) and r < self._best):
            self._save_state()
            self._best = r




class DataGenerator(keras.utils.Sequence):
    'Generates data for Keras'
    def __init__(self, feature_arrays, label_arrays, batch_size=80, shuffle=True):
        'Initialization'
        self._feature_arrays = feature_arrays
        self._label_arrays = label_arrays
        assert len(self._feature_arrays) == len(self._label_arrays), "Different numbers of feature and label arrays given"
        all(len(x) == len(y) for x, y in zip(self._feature_arrays, self._label_arrays)), "A feature-label array pair is of different size"
        self._min_common_size = min([len(x) for x in self._feature_arrays])
        self._indices = [np.arange(self._min_common_size) for _ in range(len(self._feature_arrays))]

        self._batch_size = batch_size
        assert 0 < self._batch_size, "Batch size has to be positive"
        self._length = int(np.floor(self._min_common_size/self._batch_size))
        self._shuffle = shuffle
        self._do_shuffle()

    def _do_shuffle(self):
        if self._shuffle:
            for indices in self._indices:
                np.random.shuffle(indices)

    def __len__(self):
        'Denotes the number of batches per epoch'
        return self._length

    def __getitem__(self, index):
        'Generate one batch of data'
        if index >= len(self):
            raise IndexError("index %i out of range for generator of size %i" % (index, len(self)))

        start, end = index * self._batch_size, (index + 1) * self._batch_size

        features = [feature_array[indicies[start:end]] for feature_array, indicies in zip(self._feature_arrays, self._indices)]
        labels = [label_array[indicies[start:end]] for label_array, indicies in zip(self._label_arrays, self._indices)]
        return features, labels

    def on_epoch_end(self):
        'Updates indexes after each epoch'
        self._do_shuffle()
