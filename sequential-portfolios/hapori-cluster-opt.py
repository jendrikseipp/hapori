"""
Portfolio generator: ClusterPortfolio

Time for computing portfolio: 0.54s
Score: 1495.00
Average score quota: 0.77
Standard deviation of score quota: 0.24
Training set: learn-opt-portfolios-eval

Settings:
   Maximum plantime: 1800
   Absolute score: True
   Clusters: 5
"""

PLANNERS = [
    # opt+ipc2014-opt-symba1+default
    (360, ['ipc2014-opt-symba1', 'default']),
    # opt+ipc2018-decstar+opt-config01
    (360, ['ipc2018-decstar', 'opt-config01']),
    # opt+ipc2018-decstar+opt-config06
    (360, ['ipc2018-decstar', 'opt-config06']),
    # opt+ipc2018-opt-delfi+h2-simpless-dks-900masb50ksccdfp
    (360, ['ipc2018-opt-delfi', 'h2-simpless-dks-900masb50ksccdfp']),
    # opt+ipc2018-opt-scorpion+default
    (360, ['ipc2018-opt-scorpion', 'default']),
]
