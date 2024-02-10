"""
Portfolio generator: IncreasingTimelimitPortfolio

Time for computing portfolio: 0.01s
Score: 1570.00
Average score quota: 0.81
Standard deviation of score quota: 0.20
Training set: learn-opt-portfolios-eval

Settings:
   Maximum plantime: 1800
   Absolute score: True
   Step size: 30
"""

PLANNERS = [
    # opt+ipc2018-opt-delfi+h2-simpless-oss-celmcut
    (30, ['ipc2018-opt-delfi', 'h2-simpless-oss-celmcut']),
    # opt+ipc2014-opt-symba1+default
    (399, ['ipc2014-opt-symba1', 'default']),
    # opt+ipc2018-decstar+opt-config06
    (90, ['ipc2018-decstar', 'opt-config06']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-cpdbshc900
    (329, ['ipc2018-opt-delfi', 'h2-simpless-oss-cpdbshc900']),
    # opt+ipc2018-opt-metis+metis2
    (356, ['ipc2018-opt-metis', 'metis2']),
    # opt+ipc2018-decstar+opt-config01
    (209, ['ipc2018-decstar', 'opt-config01']),
    # opt+ipc2018-opt-scorpion+default
    (387, ['ipc2018-opt-scorpion', 'default']),
]
