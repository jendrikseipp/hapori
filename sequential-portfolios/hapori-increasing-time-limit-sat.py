"""
Portfolio generator: IncreasingTimelimitPortfolio

Time for computing portfolio: 0.02s
Score: 1651.96
Average score quota: 0.75
Standard deviation of score quota: 0.22
Training set: learn-sat-portfolios-eval

Settings:
   Maximum plantime: 1800
   Absolute score: True
   Step size: 10
"""

PLANNERS = [
    # sat+ipc2014-jasper+default
    (150, ['ipc2014-jasper', 'default']),
    # sat+ipc2018-lapkt-bfws+poly-bfws
    (160, ['ipc2018-lapkt-bfws', 'poly-bfws']),
    # sat+ipc2018-fd-2018+config09
    (30, ['ipc2018-fd-2018', 'config09']),
    # sat+ipc2018-freelunch-madagascar+default
    (55, ['ipc2018-freelunch-madagascar', 'default']),
    # sat+ipc2018-fd-2018+config52
    (70, ['ipc2018-fd-2018', 'config52']),
    # sat+ipc2018-olcff+default
    (179, ['ipc2018-olcff', 'default']),
    # sat+ipc2018-fd-2018+config34
    (103, ['ipc2018-fd-2018', 'config34']),
    # sat+ipc2018-fd-2018+config47
    (119, ['ipc2018-fd-2018', 'config47']),
    # sat+ipc2018-lapkt-dfs-plus+default
    (129, ['ipc2018-lapkt-dfs-plus', 'default']),
    # sat+ipc2018-fd-2018+config15
    (113, ['ipc2018-fd-2018', 'config15']),
    # sat+ipc2018-decstar+sat-config01
    (169, ['ipc2018-decstar', 'sat-config01']),
    # sat+ipc2018-fd-2018+config22
    (181, ['ipc2018-fd-2018', 'config22']),
    # sat+ipc2018-fd-2018+config14
    (195, ['ipc2018-fd-2018', 'config14']),
    # sat+ipc2018-fd-2018+config35
    (147, ['ipc2018-fd-2018', 'config35']),
]
