"""
Coverage: 1902
Runtime: 1800s
Score: 1738.0540429034063
"""

TRACK = "sat"
PLANNERS = [
    # sat+ipc2014-jasper+default
    (1, ['ipc2014-jasper', 'default']),
    # sat+ipc2018-fd-2018+config24
    (2, ['ipc2018-fd-2018', 'config24']),
    # sat+ipc2018-lapkt-bfws+poly-bfws
    (3, ['ipc2018-lapkt-bfws', 'poly-bfws']),
    # sat+ipc2014-jasper+default
    (6, ['ipc2014-jasper', 'default']),
    # sat+ipc2018-fd-2018+config23
    (1, ['ipc2018-fd-2018', 'config23']),
    # sat+ipc2018-lapkt-bfws+poly-bfws
    (9, ['ipc2018-lapkt-bfws', 'poly-bfws']),
    # sat+ipc2018-freelunch-madagascar+default
    (2, ['ipc2018-freelunch-madagascar', 'default']),
    # sat+ipc2018-fd-2018+config40
    (5, ['ipc2018-fd-2018', 'config40']),
    # sat+ipc2014-jasper+default
    (16, ['ipc2014-jasper', 'default']),
    # sat+ipc2018-fd-2018+config02
    (7, ['ipc2018-fd-2018', 'config02']),
    # sat+ipc2018-lapkt-bfws+poly-bfws
    (21, ['ipc2018-lapkt-bfws', 'poly-bfws']),
    # sat+ipc2018-decstar+sat-config01
    (4, ['ipc2018-decstar', 'sat-config01']),
    # sat+ipc2014-jasper+default
    (46, ['ipc2014-jasper', 'default']),
    # sat+ipc2018-fd-2018+config30
    (2, ['ipc2018-fd-2018', 'config30']),
    # sat+ipc2018-fd-2018+config29
    (1, ['ipc2018-fd-2018', 'config29']),
    # sat+ipc2018-fd-2018+config14
    (8, ['ipc2018-fd-2018', 'config14']),
    # sat+ipc2018-fd-2018+config05
    (6, ['ipc2018-fd-2018', 'config05']),
    # sat+ipc2018-lapkt-bfws+poly-bfws
    (78, ['ipc2018-lapkt-bfws', 'poly-bfws']),
    # sat+ipc2018-decstar+sat-config01
    (10, ['ipc2018-decstar', 'sat-config01']),
    # sat+ipc2018-fd-2018+config15
    (25, ['ipc2018-fd-2018', 'config15']),
    # sat+ipc2018-freelunch-madagascar+default
    (6, ['ipc2018-freelunch-madagascar', 'default']),
    # sat+ipc2018-olcff+default
    (80, ['ipc2018-olcff', 'default']),
    # sat+ipc2018-fd-2018+config04
    (2, ['ipc2018-fd-2018', 'config04']),
    # sat+ipc2018-fd-2018+config52
    (20, ['ipc2018-fd-2018', 'config52']),
    # sat+ipc2018-fd-2018+config12
    (1, ['ipc2018-fd-2018', 'config12']),
    # sat+ipc2018-fd-2018+config55
    (67, ['ipc2018-fd-2018', 'config55']),
    # sat+ipc2018-fd-2018+config44
    (3, ['ipc2018-fd-2018', 'config44']),
    # sat+ipc2018-fd-2018+config01
    (4, ['ipc2018-fd-2018', 'config01']),
    # sat+ipc2014-jasper+default
    (184, ['ipc2014-jasper', 'default']),
    # sat+ipc2018-fd-2018+config13
    (2, ['ipc2018-fd-2018', 'config13']),
    # sat+ipc2018-fd-2018+config49
    (3, ['ipc2018-fd-2018', 'config49']),
    # sat+ipc2018-lapkt-dfs-plus+default
    (4, ['ipc2018-lapkt-dfs-plus', 'default']),
    # sat+ipc2018-fd-2018+config10
    (3, ['ipc2018-fd-2018', 'config10']),
    # sat+ipc2018-fd-2018+config22
    (87, ['ipc2018-fd-2018', 'config22']),
    # sat+ipc2018-freelunch-madagascar+default
    (24, ['ipc2018-freelunch-madagascar', 'default']),
    # sat+ipc2018-lapkt-dfs-plus+default
    (129, ['ipc2018-lapkt-dfs-plus', 'default']),
    # sat+ipc2018-fd-2018+config29
    (12, ['ipc2018-fd-2018', 'config29']),
    # sat+ipc2018-decstar+sat-config01
    (94, ['ipc2018-decstar', 'sat-config01']),
    # sat+ipc2018-fd-2018+config04
    (7, ['ipc2018-fd-2018', 'config04']),
    # sat+ipc2018-fd-2018+config59
    (64, ['ipc2018-fd-2018', 'config59']),
    # sat+ipc2018-fd-2018+config52
    (74, ['ipc2018-fd-2018', 'config52']),
    # sat+ipc2018-fd-2018+config09
    (553, ['ipc2018-fd-2018', 'config09']),
    # sat+ipc2018-fd-2018+config37
    (12, ['ipc2018-fd-2018', 'config37']),
    # sat+ipc2018-fd-2018+config45
    (22, ['ipc2018-fd-2018', 'config45']),
    # sat+ipc2018-fd-2018+config33
    (3, ['ipc2018-fd-2018', 'config33']),
    # sat+ipc2018-fd-2018+config44
    (8, ['ipc2018-fd-2018', 'config44']),
    # sat+ipc2018-fd-2018+config42
    (15, ['ipc2018-fd-2018', 'config42']),
    # sat+ipc2018-fd-2018+config18
    (35, ['ipc2018-fd-2018', 'config18']),
    # sat+ipc2018-fd-2018+config19
    (6, ['ipc2018-fd-2018', 'config19']),
    # sat+ipc2018-fd-2018+config32
    (11, ['ipc2018-fd-2018', 'config32']),
    # sat+ipc2018-fd-2018+config01
    (9, ['ipc2018-fd-2018', 'config01']),
    # sat+ipc2018-fd-2018+config28
    (1, ['ipc2018-fd-2018', 'config28']),
    # sat+ipc2018-fd-2018+config41
    (2, ['ipc2018-fd-2018', 'config41']),
]
