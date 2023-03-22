import argparse
import json
import numpy as np
from ..label_transformations import label_transformations, LabelTransformations
from .ml_technique import BaseMLFactory, BaseML

parser = argparse.ArgumentParser()

class Best(BaseML):
    def __init__(self, label_transformation, timeout):
        super().__init__()
        self._label_transformation = label_transformation
        self._timeout = timeout
        if self._label_transformation in [
            LabelTransformations.NONE,
            LabelTransformations.TIME,
            LabelTransformations.LOG
        ]:
            self._timeout = label_transformations[self._label_transformation](
                self._timeout, None)
        else:
            assert self._label_transformation == LabelTransformations.BINARY

        self._nb_planners = None
        self._model = None

    def train(self, x_train, y_train, x_valid, y_valid, **kwargs):
        assert len(kwargs) == 0
        assert self._model is None
        assert y_valid is None and x_valid is None, \
            "Validation data not supported."

        if self._label_transformation in [
            LabelTransformations.LOG, LabelTransformations.TIME,
            LabelTransformations.NONE
        ]:
            y_train = y_train <= self._timeout
        elif self._label_transformation == LabelTransformations.BINARY:
            pass
        else:
            assert False, self._label_transformation

        self._nb_planners = y_train.shape[1]
        self._model = int(np.argmax(y_train.sum(axis=0)))

    def predict(self, x_test):
        new_predictions = np.ndarray(
            shape=(len(x_test)), dtype=int)
        new_predictions[:] = self._model
        return new_predictions.tolist()

    def evaluate(self, x_data, y_data, **kwargs):
        return {}

    def store(self, filename):
        with open(filename, "w") as f:
            json.dump({"model": self._model, "nb_planners": self._nb_planners}, f)

    def load(self, filename, x_shape, y_shape, **kwargs):
        with open(filename, "r") as f:
            data = json.load(f)
            self._nb_planners = data["nb_planners"]
            self._model = data["model"]

    def describe(self):
        return ""


class BestFactory(BaseMLFactory):
    def parse(self, args):
        return parser.parse_args(args)

    def setup(self, main_options, learner_options):
        return Best(
            main_options.label_transformation,
            main_options.timeout,
        )
