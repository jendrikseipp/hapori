import argparse

import numpy as np
from sklearn.linear_model import Lasso, LinearRegression, Ridge

from .ml_technique import BaseMLFactory, SKLearnPerPlannerML

from .. import sample_weights

parser = argparse.ArgumentParser()
parser.add_argument("--l1", type=float, default=0.0)
parser.add_argument("--l2", type=float, default=0.0)
parser.add_argument("--iter", type=int, default=1000)


class MLinearRegression(SKLearnPerPlannerML):
    def __init__(self, l1, l2, iterations, **kwargs):
        super().__init__(**kwargs)
        self.l1 = l1
        self.l2 = l2
        self.iterations = iterations
        assert self.l1 == 0 or self.l2 == 0

    def train(self, x_train, y_train, x_valid, y_valid, **kwargs):
        weight_train = kwargs.pop(sample_weights.KEYWORD, None)
        assert len(kwargs) == 0
        assert self._model is None
        assert x_valid is None and y_valid is None, \
            "Validation data not supported."

        if self.l1 != 0 and self.l2 != 0:
            assert False
        elif self.l1 != 0:
            self._model = Lasso(alpha=self.l1, max_iter=self.iterations)
        elif self.l2 != 0:
            self._model = Ridge(alpha=self.l2, max_iter=self.iterations)
        else:
            self._model = LinearRegression()

        self._model.fit(x_train, y_train, sample_weight=weight_train)

    def predict_per_planner(self, x_test):
        return self._model.predict(x_test)

    def describe(self):
        return "l1_{}_l2_{}_iter{}".format(
            self.l1,
            self.l2,
            self.iterations,
        )


class LinearRegressionFactory(BaseMLFactory):
    def parse(self, args):
        return parser.parse_args(args)

    def setup(self, main_options, learner_options):
        return MLinearRegression(
            learner_options.l1,
            learner_options.l2,
            learner_options.iter,
            label_transformation=main_options.label_transformation,
        )
