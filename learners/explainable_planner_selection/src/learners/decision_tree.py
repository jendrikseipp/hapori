import argparse
import sys

import numpy as np
from sklearn.tree import DecisionTreeClassifier

from .ml_technique import BaseMLFactory, SKLearnML
from ..label_transformations import LabelTransformations, label_transformations


parser = argparse.ArgumentParser()
parser.add_argument("--criterion", default="gini",
                    help="The impurity criterion to evaluate the tree nodes.")
parser.add_argument("--max-depth", type=int, default=None,
                    help="maximum depth of the tree")
parser.add_argument("--min-weight-fraction-leaf", type=float, default=0.0,
                    help="Minimum summed weights of samples associated to a "
                         "tree leaf (if samples weights are not given, "
                         "then they are all weighted by 1)")
# parser.add_argument("--minimum-cost-complexity-pruning",
#                     type=float, default=0.0,
#                     help="Alpha (0 <= alpha <= 1) value for minimal "
#                          "cost-complexity pruning of a learned tree.")


class DecisionTree(SKLearnML):

    def __init__(self,
                 criterion: str,
                 max_depth: int,
                 min_weight_fraction_leaf: float,
                 # minimum_cost_complexity_pruning: float,
                 timeout: float,
                 **kwargs):
        super().__init__(**kwargs)
        self._nb_planners = None

        self._criterion = criterion
        self._max_depth = max_depth
        self._min_weight_fraction_leaf = min_weight_fraction_leaf
        # self._minimum_cost_complexity_pruning = minimum_cost_complexity_pruning

        self._timeout = timeout
        if self._label_transformation in [
            LabelTransformations.NONE,
            LabelTransformations.TIME,
            LabelTransformations.LOG
        ]:
            self._timeout = label_transformations[self._label_transformation](
                self._timeout, None)

        assert self._label_transformation in [
            LabelTransformations.BINARY,
            LabelTransformations.LOG,
            LabelTransformations.TIME,
            LabelTransformations.NONE,
        ], "The decision tree is not implemented for this label transformation."

    def preprocess_training_labels(self, features, labels):
        assert len(features) == len(labels)
        self._nb_planners = labels.shape[1]

        # See which planners solve the given tasks
        if self._label_transformation == LabelTransformations.BINARY:
            coverages = labels > 0
        elif self._label_transformation in [
            LabelTransformations.NONE,
            LabelTransformations.TIME,
            LabelTransformations.LOG
        ]:
            coverages = labels <= self._timeout
        else:
            assert False, "Internal Error: Unknown label transformation"
        new_features = []
        new_labels = []
        sample_weights = []
        for feats, lbls, cov in zip(features, labels, coverages):
            idx_planners = np.arange(len(cov))[cov]
            new_labels.append(idx_planners)
            new_features.append(np.tile(feats, (len(idx_planners), 1)))
            if self._label_transformation == LabelTransformations.BINARY:
                sample_weights.append(
                    np.ones(len(idx_planners))/len(idx_planners))
            elif self._label_transformation in [
                LabelTransformations.NONE,
                LabelTransformations.TIME,
                LabelTransformations.LOG
            ]:
                lbls = lbls[idx_planners]
                lbls = np.maximum(lbls, sys.float_info.epsilon)
                new_lbls = np.max(lbls)/lbls
                sample_weights.append(new_lbls/new_lbls.sum())
            assert len(new_labels) == len(new_features) == len(sample_weights)
            assert (len(new_labels[-1]) == len(new_features[-1]) ==
                    len(sample_weights[-1])), "L {}, F {}, SW {}".format(
                new_labels[-1].shape, new_features[-1].shape,
                sample_weights[-1].shape)
        return (np.concatenate(new_features), np.concatenate(new_labels),
                np.concatenate(sample_weights))

    def postprocess_predicted_labels(self, predictions):
        new_predictions = np.ndarray(
            shape=(len(predictions), self._nb_planners))
        if self._label_transformation == LabelTransformations.BINARY:
            base, choice = 0, 1
        elif self._label_transformation in [
            LabelTransformations.NONE,
            LabelTransformations.TIME,
            LabelTransformations.LOG
        ]:
            base, choice = 1, 0
        else:
            assert False, "Internal Error: Unknown label transformation"
        new_predictions[:, :] = base
        new_predictions[np.arange(len(predictions)), predictions] = choice
        return new_predictions

    def train(self, x_train, y_train, x_valid, y_valid, **kwargs):
        assert len(kwargs) == 0
        assert self._model is None
        assert x_valid is None and y_valid is None, \
            "Validation data not supported."

        x_train, y_train, sample_weights = self.preprocess_training_labels(
            x_train, y_train)

        self._model = DecisionTreeClassifier(
            criterion=self._criterion,
            max_depth=self._max_depth,
            min_weight_fraction_leaf=self._min_weight_fraction_leaf,
            # ccp_alpha=self._minimum_cost_complexity_pruning,
        )
        self._model.fit(x_train, y_train, sample_weight=sample_weights)

    def predict(self, x_test):
        return self._model.predict(x_test).tolist()

    def evaluate(self, x_data, y_data, **kwargs):
        return {}

    def describe(self):
        return "crit{}_maxDepth{}_minLeaf{}".format(
            self._criterion,
            self._max_depth,
            self._min_weight_fraction_leaf,
            # self._minimum_cost_complexity_pruning,
        )

    def load(self, filename, **kwargs):
        y_shape = kwargs["y_shape"]
        super().load(filename, **kwargs)

        self._nb_planners = y_shape[1]


class DecisionTreeFactory(BaseMLFactory):
    def parse(self, args):
        return parser.parse_args(args)

    def setup(self, main_options, learner_options):
        return DecisionTree(
            learner_options.criterion,
            learner_options.max_depth,
            learner_options.min_weight_fraction_leaf,
            # learner_options.minimum_cost_complexity_pruning,
            main_options.timeout,
            label_transformation=main_options.label_transformation)
