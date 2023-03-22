"""
Script for constant values and default configurations
"""
import os
import random


R = random.Random(2019)

DIR_OUT = os.path.join(os.path.dirname(__file__), "..", "performance")
DIR_DATA = os.path.join(os.path.dirname(__file__), "..", "data")

DIR_PROBLEM_SPLITS = os.path.join(DIR_DATA, "problems")
TRAIN_SET = os.path.join(DIR_PROBLEM_SPLITS, "problem-names-train.txt")
VALIDATION_SET = os.path.join(DIR_PROBLEM_SPLITS, "problem-names-valid.txt")
TEST_SET = os.path.join(DIR_PROBLEM_SPLITS, "problem-names-test.txt")
DOMAIN_GROUPS = os.path.join(DIR_PROBLEM_SPLITS, "domain_groups.txt")

DIR_PROBLEMS = os.path.join(DIR_DATA, "problems")

FILE_RUNTIMES_CSV = os.path.join(DIR_DATA, "labels_runtimes.csv")
FILE_FEATURES_CSV_TASK = os.path.join(DIR_DATA, "features_fawcett.csv")
FILE_FEATURES_CSV_LIFTED = os.path.join(DIR_DATA, "features_lifted.csv")
FILE_FEATURES_CSV_GROUNDED = os.path.join(DIR_DATA, "features_grounded.csv")
FILE_DELFI_PLANNERS = os.path.join(DIR_DATA, "delfi_planners.txt")

if __name__ == "__main__":
    print("Thou shall not call me directly.")
