import argparse

import numpy as np
from sklearn import svm

from ..label_transformations import LabelTransformations

from .ml_technique import BaseMLFactory, SKLearnPerPlannerML

parser = argparse.ArgumentParser()
parser.add_argument("--kernel", help="linear, rbf, polynomial, sigmoid",
                    default="linear")


class SVR(SKLearnPerPlannerML):
    def __init__(self, kernel, **kwargs):
        super().__init__(**kwargs)
        self.kernel = kernel

        assert self._label_transformation in [
            LabelTransformations.NONE,
            LabelTransformations.TIME,
            LabelTransformations.LOG
        ]

    def train(self, x_train, y_train, x_valid, y_valid, **kwargs):
        assert len(kwargs) == 0
        assert x_valid is None and y_valid is None, \
            "Validation data not supported."
        assert self._model is None
        self._model = []
        for i in range(y_train.shape[1]):
            print("{}/{}".format(i, y_train.shape[1]))
            self._model.append(svm.SVR(kernel=self.kernel))
            self._model[-1].fit(x_train, y_train[:, i])

    def predict_per_planner(self, x_test):
        return [m.predict(x_test) for m in self._model]

    def describe(self,):
        return "kernel{}".format(self.kernel)


class SVRFactory(BaseMLFactory):
    def parse(self, args):
        return parser.parse_args(args)

    def setup(self, main_options, learner_options):
        return SVR(learner_options.kernel,
                   label_transformation=main_options.label_transformation)
