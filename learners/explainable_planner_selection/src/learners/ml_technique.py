import argparse
from abc import ABC, abstractmethod
from typing import List, Union, Tuple

import joblib
import numpy as np
from sklearn import metrics

from ..label_transformations import LabelTransformations, choose_max_label


Prediction = List[Union[int, List[Tuple]]]


def evaluate_coverage(prediction: Prediction, y_time: np.ndarray, timeout: float):
    assert len(prediction) == len(y_time), f"{len(prediction)}, {y_time.shape}"
    coverage = []
    for pred, ys in zip(prediction, y_time):
        if isinstance(pred, int):
            coverage.append(int(ys[pred] <= timeout))
        elif isinstance(pred, list):
            assert all(isinstance(x, tuple) for x in pred)
            all_weights = float(sum(weight for _, weight in pred))
            coverage.append(int(any(ys[idx] <= (timeout * weight/all_weights)
                                for idx, weight in pred)))
    return coverage


def summarize_per_planner_prediction(
        raw_predictions: np.ndarray,
        label_transformation: LabelTransformations) -> Prediction:
    best = (np.argmax if choose_max_label[label_transformation] else np.argmin)
    return best(raw_predictions, axis=1).tolist()


def evaluate_per_planner_prediction(raw_predictions: np.ndarray, y_data: np.ndarray) -> dict:
    mse = metrics.mean_squared_error(y_data, raw_predictions),
    irmse = [np.sqrt(metrics.mean_squared_error(
        y_data[:, i], raw_predictions[:, i]))
        for i in range(y_data.shape[1])]
    return {
        "mse": mse,
        "rmse": int(np.sqrt(mse)),
        "irmse": irmse
    }


class BaseML(ABC):
    def __init__(self):
        pass

    @abstractmethod
    def train(self,
              x_train: np.ndarray, y_train: np.ndarray,
              x_valid: np.ndarray, y_valid: np.ndarray,
              **kwargs) -> None:
        """
        Train the machine learning method.
        :param x_train: features of the training data (NxF)
        :param y_train: labels of the training data (Nx1)
        :param x_valid: features of the validation data (MxF)
        :param y_valid: labels of the validation data (Mx1)
        :return: None
        """
        raise NotImplementedError("Todo by subclass")

    @abstractmethod
    def predict(self, x_test: np.ndarray) -> Prediction:
        """
        Predict for a list of tasks (each tasks is described by its features)
        the planner to use
        :param x_test: features of tasks (NxF)
        :return: [Prediction, ...] per task a prediction
                 Prediction := idx: int | [(idx: int, weight: float)+]
                 idx := planner index
                 weight := how much time to assign to it (weight/sum(all weights))
        """
        raise NotImplementedError("Todo by subclass")

    @abstractmethod
    def evaluate(self, x_data: np.ndarray, y_data : np.ndarray, **kwargs) -> dict:
        """
        Returns some statistics that can be used to evaluate the learned
        model.
        :param x_data: the features of the test tasks (NxF)
        :param y_data: the labels to predict (per task and planner) (NxP)
        :return: {Property: value}
        """
        raise NotImplementedError("Todo by subclass")

    @abstractmethod
    def store(self, filename: str) -> None:
        """
        Store the trained model
        :param filename: file location for storing
        :return: None
        """
        raise NotImplementedError("Todo by subclass")

    @abstractmethod
    def load(self, filename: str, **kwargs) -> None:
        """
        Load a trained model into this class object
        :param filename: file location for storing
        :return: None
        """
        raise NotImplementedError("Todo by subclass")

    @abstractmethod
    def describe(self) -> str:
        """
        Return a textual description of the model to construct from the
        given parameters
        :return: string description (normally containing the parameters of
        the model)
        """
        raise NotImplementedError("Todo by subclass")


class SKLearnML(BaseML, ABC):
    def __init__(self, label_transformation):
        super().__init__()
        self._label_transformation = label_transformation
        self._model = None

    def store(self, filename):
        joblib.dump((self._model, self._label_transformation), filename)

    def load(self, filename, **kwargs):
        self._model, self._label_transformation = joblib.load(filename)


class SKLearnPerPlannerML(SKLearnML, ABC):
    def __init__(self, label_transformation):
        super().__init__(
            label_transformation=label_transformation)

    @abstractmethod
    def predict_per_planner(self, x_test: np.ndarray) -> np.ndarray:
        """
        Predict for a list of tasks (each tasks is described by its features)
        the trained label for each planner
        :param x_test: features of tasks (NxF)
        :return: prediction per task and planner(NxP)
        """
        raise NotImplementedError("Todo by subclass")

    def predict(self, x_test: np.ndarray) -> Prediction:
        return summarize_per_planner_prediction(
            self.predict_per_planner(x_test),
            self._label_transformation)

    def evaluate(self, x_data: np.ndarray, y_data : np.ndarray, **kwargs) -> dict:
        raw_predictions = self.predict_per_planner(x_data)
        return evaluate_per_planner_prediction(raw_predictions, y_data)


class BaseMLFactory(ABC):
    def __init__(self):
        pass

    @abstractmethod
    def parse(self, args: List[str]) -> argparse.Namespace:
        """
        Parse the command line arguments and return an argparse.Namespace
        object
        :param args: list of command line arguments
        :return: argparse.Namespace
        """
        raise NotImplementedError("Todo by subclass")

    @abstractmethod
    def setup(self,
              main_options: argparse.Namespace,
              learner_options: argparse.Namespace) -> BaseML:
        """
        Build the model to train from the given arguments
        :param main_options: options for the main training process
        :param learner_options: options for the model
        :return: BaseML object
        """
        raise NotImplementedError("Todo by subclass")
