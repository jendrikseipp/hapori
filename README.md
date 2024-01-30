# Run a single planner

    ./plan.py images/ipc2018-opt-complementary2.img benchmarks/driverlog-strips/{domain-,}0-p01.pddl sas_plan
    ./plan.py images/ipc2018-lapkt-bfws.img --config poly-bfws benchmarks/driverlog-strips/{domain-,}0-p04.pddl sas_plan

# Run a sequential portfolio

    ./run-portfolio.py --portfolio portfolio_driver/portfolios/hapori-stonesoup-opt.py --overall-time-limit 30m benchmarks/gripper-strips/{domain-,}0-p01.pddl

Please follow the naming and path convention for new portfolios. This also allows calling portfolios via an alias:

    ./run-portfolio.py --alias hapori-stonesoup-opt --overall-time-limit 30m benchmarks/gripper-strips/{domain-,}0-p01.pddl

`run-portfolio.py` detects from the portfolio filename whether the portfolio should stop after finding the first plan ("\*-agl.py" and "\*-opt.py") or continue with subsequent planners ("\*-sat.py").

# Planner data

* optimal (30m time limit): experiments/data/01-opt-planners-eval/properties-hardest.json.xz
* agile/satisficing (5m time limit): experiments/data/02-sat-planners-eval/properties-hardest.json.xz

# Learn Stone Soup portfolios

    cd learners/stonesoup
    ./learn-portfolios.sh

# Adding a new learner

* Add code to `learners/mylearner`.
* If mylearner produces a sequential portfolio, add it to `portfolio_driver/portfolios/hapori-mylearner-{agl,opt,sat}` using the same format as `hapori-stonesoup-opt.py`.
* Otherwise, write your own script for executing your planner (under `learners/mylearner`). This script should use `plan.py` to execute planners.
* Add Apptainer.hapori-mylearner-{agl,opt,sat} files for your planner.
* Install Apptainer following the instructions in the existing Apptainer.* files.
* Build and test your Apptainer image with `./build-hapori-image.sh Apptainer.hapori-myleaner-{agl,opt,sat} hapori-mylearner-{agl,opt,sat}.img`.

# Build and test all Hapori Apptainer images

    ./build-all-hapori-images.sh

This can take a while since the Scorpion planner always uses 200s of preprocessing time.





# Notes from Silvan
to build apptainer images:
* apptainer build <image-name>.sif <recipe-name>

to generate training data, use the following scripts in experiments:
* 2023-11-27+*py, 2024-01-17-ipc2014-jasper.py and 2024-01-19-ipc2018-cerberus.py to run each component planner, using the image built with Apptainer.hapori_components
* training-data-collect.py collects all properties of component planners, performs some analysis, filters problematic runs and creates separate properties files for each track, both the full training data and the "reduced" one (at most 30 tasks per domain)
* training-data-generate-csv.py generates one csv file for each attribute of interest from the training data generated with training-data-collect.py

to test the portfolios:
* built portfolio images using the recipes (except Apptainer.hapori_components) in main dir and use scripts 2024-01-30-* in experiments

TODOs:
* it is not clear how domain_properties.csv was generated for the IPC; I adapted it to reflect the merged t0 and fsc domains
* features:
    - since we merged these domains, existing features (e.g., by delfi and eps) need to be updated accordingly
    - EPS features: it is not clear how features_opt.csv and features_sat.csv in learners/explainable_planner_selection/ were created; possibly from the data (json) created with experiments/2023-03-07-A-fawcett-features.py; the data is still checked in under experiments/data/2023-03-07-A-fawcett-features-eval
    - for reproducibility, we should include scripts for generating features, possibly directly in the learner subdirs
* update this file
