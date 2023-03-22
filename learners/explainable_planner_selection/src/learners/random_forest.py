import argparse

import numpy as np
from sklearn.ensemble import RandomForestRegressor

from .ml_technique import BaseMLFactory, SKLearnPerPlannerML

from .. import sample_weights

parser = argparse.ArgumentParser()
parser.add_argument("--trees", action="store", type=int, default=10,
                    help="different tree numbers per forest to test via"
                         "CV.")
parser.add_argument("--independent-regressors", action="store_true",
                    help="creates a new forest per output.")


class RandomForest(SKLearnPerPlannerML):
    def __init__(self, independent, trees, **kwargs):
        super().__init__(**kwargs)
        self._independent = independent
        self._trees = trees
        self._models = None

    def train(self, x_train, y_train, x_valid, y_valid, **kwargs):
        weight_train = kwargs.pop(sample_weights.KEYWORD, None)
        assert len(kwargs) == 0
        assert x_valid is None and y_valid is None, \
            "Validation data not supported."
        assert self._model is None

        if self._independent:
            regressors = []
            for i in range(y_train.shape[1]):
                regressor = RandomForestRegressor(n_estimators=self._trees)
                regressor.fit(x_train, y_train[:, i], sample_weight=weight_train)
                regressors.append(regressor)
            self._models = regressors
        else:
            regressor = RandomForestRegressor(n_estimators=self._trees)
            regressor.fit(x_train, y_train, sample_weight=weight_train)
            self._model = regressor

    def predict_per_planner(self, x_test):
        if self._independent:
            return np.stack([r.predict(x_test) for r in self._models], axis=1)
        else:
            return self._model.predict(x_test)

    def describe(self):
        return "trees{}_independent{}".format(
            self._trees,
            1 if self._independent else 0)


class RandomForestFactory(BaseMLFactory):
    def parse(self, args):
        return parser.parse_args(args)

    def setup(self, main_options, learner_options):
        return RandomForest(
            learner_options.independent_regressors,
            learner_options.trees,
            label_transformation=main_options.label_transformation,
        )
