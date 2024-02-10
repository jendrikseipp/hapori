"""
Portfolio generator: RanitSearchPortfolio

Time for computing portfolio: 15.51s
Score: 1658.00
Average score quota: 0.85
Standard deviation of score quota: 0.18
Training set: learn-opt-portfolios-eval

Settings:
   Maximum plantime: 1800
   Absolute score: True
   Random seed: 6
"""

PLANNERS = [
    # opt+ipc2014-opt-symba1+default
    (405, ['ipc2014-opt-symba1', 'default']),
    # opt+ipc2018-decstar+opt-config00
    (123, ['ipc2018-decstar', 'opt-config00']),
    # opt+ipc2018-decstar+opt-config06
    (132, ['ipc2018-decstar', 'opt-config06']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-cpdbshc900
    (51, ['ipc2018-opt-delfi', 'h2-simpless-oss-cpdbshc900']),
    # opt+ipc2018-opt-delfi+simpless-oss-masb50kmiasmdfp
    (26, ['ipc2018-opt-delfi', 'simpless-oss-masb50kmiasmdfp']),
    # opt+ipc2018-opt-metis+metis2
    (252, ['ipc2018-opt-metis', 'metis2']),
    # opt+ipc2018-opt-scorpion+default
    (811, ['ipc2018-opt-scorpion', 'default']),
]
