# Run a single planner

    ./plan.py images/ipc2018-opt-complementary2.img benchmarks/driverlog-strips/{domain-,}0-p01.pddl sas_plan
    ./plan.py images/ipc2018-lapkt-bfws.img --config poly-bfws benchmarks/driverlog-strips/{domain-,}0-p04.pddl sas_plan

# Run a sequential portfolio

    ./run-portfolio.py --portfolio portfolio_driver/portfolios/hapori-stonesoup-opt.py --overall-time-limit 30m benchmarks/gripper-strips/{domain-,}0-p01.pddl

or equivalently via an alias

    ./run-portfolio.py --alias hapori-stonesoup-opt --overall-time-limit 30m benchmarks/gripper-strips/{domain-,}0-p01.pddl

`run-portfolio.py` detects from the portfolio filename whether the portfolio should stop after finding the first plan ("\*-agl.py" and "\*-opt.py") or continue with subsequent planners ("\*-sat.py").

# Build and test all Hapori Apptainer images

    ./build-all-hapori-images.sh
