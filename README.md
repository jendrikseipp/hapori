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
