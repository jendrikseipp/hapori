import enum

from .best import Best, BestFactory
from .decision_tree import DecisionTreeFactory
from .linear_regression import LinearRegressionFactory
# from .mlp import MLPFactory
from .ml_technique import BaseML, BaseMLFactory, evaluate_coverage
from .random_forest import RandomForestFactory
from .svm import SVMFactory
from .svr import SVRFactory


class Learners(enum.Enum):
    RANDOM_FOREST = "random_forest"
    # MLP = "mlp"
    LINEAR_REGRESSION = "linear_regression"
    SVM = "svm"
    SVR = "svr"
    DECISION_TREE = "decision_tree"
    BEST = "best"


learner_factories = {
    Learners.RANDOM_FOREST: RandomForestFactory(),
    # Learners.MLP: MLPFactory(),
    Learners.LINEAR_REGRESSION: LinearRegressionFactory(),
    Learners.SVM: SVMFactory(),
    Learners.SVR: SVRFactory(),
    Learners.DECISION_TREE: DecisionTreeFactory(),
    Learners.BEST: BestFactory(),
}
assert all(_x in learner_factories for _x in Learners)


if __name__ == "__main__":
    print("Thou shall not call me directly.")
