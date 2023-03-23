# Dependencies
* PuLP python library, which can be installed through `pip install pulp`
* PuLP supports different solvers. You can use CBC by default, or CPLEX if you have it installed. The solver can be changed in line 66 of `mip.py`

# How to get a portfolio
* `main.py` is the script that execute all the pipeline. To run a new configuration, you just need to modify `main.py` to (1) include the path to the folder where the training data is stored; and (2) the track you want to build the portfolio for.
* `main.py` will print the portfolio, and you will need to copy it and put it in the file called `solution.txt`. An example of the part of the output you need to copy is shown in the example `solution.txt` file.
* Finally, execute `build_portfolio_from_solution.py`, which will output the portfolio in a version similar to the one required by the portfolio driver. You just need to manually separate the planner from its configuration.

# Reproducibility
* Most of the MIP tasks cannot be solved within reasonable time and memory bounds. Therefore, you will get different portfolios depending on the suboptimality ratio (`gapRel` parameter) you specify in line 66 of `mip.py`.
