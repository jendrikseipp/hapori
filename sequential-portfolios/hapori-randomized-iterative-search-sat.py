"""
Portfolio generator: RanitSearchPortfolio

Time for computing portfolio: 144.15s
Score: 1723.16
Average score quota: 0.79
Standard deviation of score quota: 0.21
Training set: learn-sat-portfolios-eval

Settings:
   Maximum plantime: 1800
   Absolute score: True
   Random seed: 7
"""

PLANNERS = [
    # sat+ipc2014-jasper+default
    (176, ['ipc2014-jasper', 'default']),
    # sat+ipc2018-decstar+sat-config01
    (187, ['ipc2018-decstar', 'sat-config01']),
    # sat+ipc2018-decstar+sat-config02
    (1, ['ipc2018-decstar', 'sat-config02']),
    # sat+ipc2018-fd-2018+config00
    (1, ['ipc2018-fd-2018', 'config00']),
    # sat+ipc2018-fd-2018+config01
    (2, ['ipc2018-fd-2018', 'config01']),
    # sat+ipc2018-fd-2018+config03
    (12, ['ipc2018-fd-2018', 'config03']),
    # sat+ipc2018-fd-2018+config04
    (12, ['ipc2018-fd-2018', 'config06']),
    # sat+ipc2018-fd-2018+config08
    (1, ['ipc2018-fd-2018', 'config08']),
    # sat+ipc2018-fd-2018+config10
    (3, ['ipc2018-fd-2018', 'config10']),
    # sat+ipc2018-fd-2018+config12
    (1, ['ipc2018-fd-2018', 'config12']),
    # sat+ipc2018-fd-2018+config13
    (1, ['ipc2018-fd-2018', 'config13']),
    # sat+ipc2018-fd-2018+config14
    (100, ['ipc2018-fd-2018', 'config14']),
    # sat+ipc2018-fd-2018+config18
    (64, ['ipc2018-fd-2018', 'config18']),
    # sat+ipc2018-fd-2018+config23
    (6, ['ipc2018-fd-2018', 'config23']),
    # sat+ipc2018-fd-2018+config24
    (2, ['ipc2018-fd-2018', 'config24']),
    # sat+ipc2018-fd-2018+config28
    (1, ['ipc2018-fd-2018', 'config28']),
    # sat+ipc2018-fd-2018+config29
    (32, ['ipc2018-fd-2018', 'config29']),
    # sat+ipc2018-fd-2018+config32
    (11, ['ipc2018-fd-2018', 'config32']),
    # sat+ipc2018-fd-2018+config33
    (7, ['ipc2018-fd-2018', 'config33']),
    # sat+ipc2018-fd-2018+config35
    (1, ['ipc2018-fd-2018', 'config35']),
    # sat+ipc2018-fd-2018+config36
    (70, ['ipc2018-fd-2018', 'config36']),
    # sat+ipc2018-fd-2018+config37
    (13, ['ipc2018-fd-2018', 'config37']),
    # sat+ipc2018-fd-2018+config40
    (28, ['ipc2018-fd-2018', 'config40']),
    # sat+ipc2018-fd-2018+config41
    (2, ['ipc2018-fd-2018', 'config41']),
    # sat+ipc2018-fd-2018+config42
    (14, ['ipc2018-fd-2018', 'config42']),
    # sat+ipc2018-fd-2018+config44
    (3, ['ipc2018-fd-2018', 'config44']),
    # sat+ipc2018-fd-2018+config45
    (21, ['ipc2018-fd-2018', 'config45']),
    # sat+ipc2018-fd-2018+config51
    (2, ['ipc2018-fd-2018', 'config51']),
    # sat+ipc2018-fd-2018+config52
    (74, ['ipc2018-fd-2018', 'config52']),
    # sat+ipc2018-fd-2018+config55
    (7, ['ipc2018-fd-2018', 'config55']),
    # sat+ipc2018-freelunch-madagascar+default
    (50, ['ipc2018-freelunch-madagascar', 'default']),
    # sat+ipc2018-lapkt-bfws+poly-bfws
    (369, ['ipc2018-lapkt-bfws', 'poly-bfws']),
    # sat+ipc2018-lapkt-dfs-plus+default
    (65, ['ipc2018-lapkt-dfs-plus', 'default']),
    # sat+ipc2018-olcff+default
    (421, ['ipc2018-olcff', 'default']),
]
