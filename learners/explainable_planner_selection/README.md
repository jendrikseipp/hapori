# Training:
Used code from `https://zenodo.org/record/5767692`
Apply diff from `modifications.diff`

Training Call:
 ./train_ml.py linear_regression --runtimes TRAINING_LABELS --features TRAINING_FEATURES SOME_NAME --merge-validation-data --split training --output-dir ipc
 ./train_ml.py decision_tree --runtimes TRANING_LABELS --features TRAINING_FEATURES SOME_NAME --merge-validation-data --split training --output-dir ipc --max-depth 5


Training labels:
/experiments/data/01-opt-planners-eval/runtimes.csv
/experiments/data/02-sat-planners-eval/runtime.csv

Training features:
/learners/explainable_planner_selection/features_opt.csv
/learners/explainable_planner_selection/features_sat.csv

Same models are used for sat and agl.