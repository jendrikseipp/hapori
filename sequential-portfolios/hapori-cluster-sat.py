"""
Portfolio generator: ClusterPortfolio

Time for computing portfolio: 3.12s
Score: 1613.68
Average score quota: 0.74
Standard deviation of score quota: 0.24
Training set: learn-sat-portfolios-eval

Settings:
   Maximum plantime: 1800
   Absolute score: True
   Clusters: 10
"""

PLANNERS = [
    # sat+ipc2018-decstar+sat-config01
    (180, ['ipc2018-decstar', 'sat-config01']),
    # sat+ipc2018-fd-2018+config01
    (180, ['ipc2018-fd-2018', 'config01']),
    # sat+ipc2018-fd-2018+config19
    (180, ['ipc2018-fd-2018', 'config19']),
    # sat+ipc2018-fd-2018+config22
    (180, ['ipc2018-fd-2018', 'config22']),
    # sat+ipc2018-fd-2018+config33
    (180, ['ipc2018-fd-2018', 'config33']),
    # sat+ipc2018-fd-2018+config34
    (180, ['ipc2018-fd-2018', 'config34']),
    # sat+ipc2018-fd-2018+config37
    (180, ['ipc2018-fd-2018', 'config37']),
    # sat+ipc2018-fd-2018+config39
    (180, ['ipc2018-fd-2018', 'config39']),
    # sat+ipc2018-freelunch-madagascar+default
    (180, ['ipc2018-freelunch-madagascar', 'default']),
    # sat+ipc2018-lapkt-bfws+poly-bfws
    (180, ['ipc2018-lapkt-bfws', 'poly-bfws']),
]
