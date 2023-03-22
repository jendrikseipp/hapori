import argparse

import joblib
import numpy as np
from keras.layers import Input, Dense
from keras.models import Model
from keras.callbacks import EarlyStopping

from .ml_technique import BaseMLFactory, BaseML, \
    summarize_per_planner_prediction, evaluate_per_planner_prediction

from ..label_transformations import choose_max_label
from .. import sample_weights

parser = argparse.ArgumentParser()
parser.add_argument("--hidden-layers", type=int, default=3)
parser.add_argument("--hidden-layer-size", type=int, default=30)
parser.add_argument("--activation", default="relu")
parser.add_argument("--epochs", default=2000, type=int)
parser.add_argument("--loss", default="mse")
parser.add_argument("--regularization", default=None)


class MLP(BaseML):
    def __init__(self, nb_hidden, size_hidden, act, loss, regularization,
                 epochs, label_transformation):
        super().__init__()
        self.nb_hidden = nb_hidden
        self.size_hidden = size_hidden
        self.act = act
        self.loss = loss
        self.regularization = (
            None if regularization is None or regularization.lower() == "none"
            else regularization)
        self.epochs = epochs
        self._label_transformation = label_transformation
        self._model = None

    def _construct_model(self, x_shape, y_shape):
        assert self._model is None
        input_layer = Input(shape=(x_shape[1:]))
        h = input_layer
        for i in range(self.nb_hidden):
            h = Dense(self.size_hidden, activation=self.act,
                      kernel_regularizer=self.regularization)(h)
        h = Dense(y_shape[1], activation=self.act,
                  kernel_regularizer=self.regularization)(h)
        self._model = Model(inputs=input_layer, outputs=h)

    def train(self, x_train, y_train, x_valid, y_valid, **kwargs):
        assert ((x_valid is None and y_valid is None) or
                (x_valid is not None and y_valid is not None))
        weight_train = kwargs.pop(sample_weights.KEYWORD, None)
        assert len(kwargs) == 0
        assert self._model is None

        self._construct_model(x_train.shape, y_train.shape)
        self._model.compile("adam", loss=self.loss)

        early_stopping_metric = "loss" if x_valid is None else "val_loss"

        self._model.fit(
            x_train, y_train,
            epochs=self.epochs,
            callbacks=[EarlyStopping(early_stopping_metric, patience=300)],
            validation_data=None if x_valid is None else (x_valid, y_valid),
            sample_weight=weight_train)

    def predict_per_planner(self, x_test):
        return self._model.predict(x_test)

    def predict(self, x_test: np.ndarray):
        raw_predictions = self.predict_per_planner(x_test)
        return summarize_per_planner_prediction(
            raw_predictions, self._label_transformation)

    def evaluate(self, x_data: np.ndarray, y_data : np.ndarray, **kwargs) -> dict:
        return evaluate_per_planner_prediction(
            self.predict_per_planner(x_data), y_data)

    def store(self, filename):
        self._model.save_weights(filename)
        joblib.dump(self._label_transformation, filename + ".extra")

    def load(self, filename, x_shape, y_shape, **kwargs):
        self._construct_model(x_shape, y_shape)
        self._model.load_weights(filename)
        self._label_transformation = joblib.load(filename + ".extra")

    def describe(self):
        return ("layers{}_size{}_activation{}_loss{}_regularization{}_epochs{}"
                ).format(
                    self.nb_hidden,
                    self.size_hidden,
                    self.act,
                    self.loss,
                    self.regularization,
                    self.epochs)


class MLPFactory(BaseMLFactory):
    def parse(self, args):
        return parser.parse_args(args)

    def setup(self, main_options, learner_options):
        return MLP(
            learner_options.hidden_layers,
            learner_options.hidden_layer_size,
            learner_options.activation,
            learner_options.loss,
            learner_options.regularization,
            learner_options.epochs,
            label_transformation=main_options.label_transformation
        )
