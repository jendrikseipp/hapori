"""
Portfolio generator: DomainwisePortfolio

Time for computing portfolio: 5.38s
Score: 1524.33
Average score quota: 0.70
Standard deviation of score quota: 0.24
Training set: learn-sat-portfolios-eval

Settings:
   Maximum plantime: 1800
   Absolute score: True
"""

PLANNERS = [
    # sat+ipc2018-decstar+sat-config01
    (40, ['ipc2018-decstar', 'sat-config01']),
    # sat+ipc2018-fd-2018+config03
    (646, ['ipc2018-fd-2018', 'config03']),
    # sat+ipc2018-fd-2018+config09
    (12, ['ipc2018-fd-2018', 'config09']),
    # sat+ipc2018-fd-2018+config11
    (588, ['ipc2018-fd-2018', 'config11']),
    # sat+ipc2018-fd-2018+config13
    (3, ['ipc2018-fd-2018', 'config13']),
    # sat+ipc2018-fd-2018+config16
    (44, ['ipc2018-fd-2018', 'config16']),
    # sat+ipc2018-fd-2018+config40
    (4, ['ipc2018-fd-2018', 'config40']),
    # sat+ipc2018-fd-2018+config42
    (66, ['ipc2018-fd-2018', 'config42']),
    # sat+ipc2018-fd-2018+config54
    (98, ['ipc2018-fd-2018', 'config54']),
    # sat+ipc2018-fd-2018+config59
    (30, ['ipc2018-fd-2018', 'config59']),
    # sat+ipc2018-freelunch-madagascar+default
    (1, ['ipc2018-freelunch-madagascar', 'default']),
    # sat+ipc2018-lapkt-bfws+poly-bfws
    (91, ['ipc2018-lapkt-bfws', 'poly-bfws']),
    # sat+ipc2018-lapkt-dfs-plus+default
    (45, ['ipc2018-lapkt-dfs-plus', 'default']),
    # sat+ipc2018-olcff+default
    (140, ['ipc2018-olcff', 'default']),
]
