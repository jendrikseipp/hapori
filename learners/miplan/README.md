# Dependencies
* PuLP python library, which can be installed through `pip install pulp`
* PuLP supports different solvers. You can use CBC by default, or CPLEX if you have it installed. The solver can be changed in line 56 of `mip.py`

# How to get a portfolio
* `main.py` is the script that execute all the pipeline. To run a new configuration, you just need to run `main.py` with your desired parameters.

# Reproducibility
* Most of the MIP tasks cannot be solved within reasonable time and memory bounds. Therefore, you will get different portfolios depending on the `suboptimality-gap` parameter.