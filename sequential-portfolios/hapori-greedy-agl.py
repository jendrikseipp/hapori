"""
Coverage: 2074
Runtime: 1771s
Score: 1262.8037790312978
"""

TRACK = "agl"
PLANNERS = [
    # agl+ipc2014-agl-mpc+default
    (1, ['ipc2014-agl-mpc', 'default']),
    # agl+ipc2014-agl-probe+default
    (1, ['ipc2014-agl-probe', 'default']),
    # agl+ipc2014-jasper+default
    (4, ['ipc2014-jasper', 'default']),
    # agl+ipc2018-lapkt-bfws+dual-bfws-agl
    (4, ['ipc2018-lapkt-bfws', 'dual-bfws-agl']),
    # agl+ipc2014-agl-mpc+default
    (3, ['ipc2014-agl-mpc', 'default']),
    # agl+ipc2018-lapkt-bfws+poly-bfws
    (10, ['ipc2018-lapkt-bfws', 'poly-bfws']),
    # agl+ipc2018-mercury2014+agl
    (11, ['ipc2018-mercury2014', 'agl']),
    # agl+ipc2018-decstar+agl-config01
    (4, ['ipc2018-decstar', 'agl-config01']),
    # agl+ipc2014-jasper+default
    (21, ['ipc2014-jasper', 'default']),
    # agl+ipc2018-lapkt-bfws+bfws-pref-agl
    (37, ['ipc2018-lapkt-bfws', 'bfws-pref-agl']),
    # agl+ipc2018-saarplan+agl-config01
    (79, ['ipc2018-saarplan', 'agl-config01']),
    # agl+ipc2014-agl-mpc+default
    (13, ['ipc2014-agl-mpc', 'default']),
    # agl+ipc2018-fd-2018+config40
    (1, ['ipc2018-fd-2018', 'config40']),
    # agl+ipc2018-freelunch-madagascar+default
    (2, ['ipc2018-freelunch-madagascar', 'default']),
    # agl+ipc2018-fd-2018+config15
    (7, ['ipc2018-fd-2018', 'config15']),
    # agl+ipc2014-jasper+default
    (120, ['ipc2014-jasper', 'default']),
    # agl+ipc2018-decstar+agl-config01
    (20, ['ipc2018-decstar', 'agl-config01']),
    # agl+ipc2018-cerberus+agl
    (5, ['ipc2018-cerberus', 'agl']),
    # agl+ipc2018-merwin+agl
    (2, ['ipc2018-merwin', 'agl']),
    # agl+ipc2018-lapkt-bfws+bfws-pref-agl
    (120, ['ipc2018-lapkt-bfws', 'bfws-pref-agl']),
    # agl+ipc2018-freelunch-madagascar+default
    (7, ['ipc2018-freelunch-madagascar', 'default']),
    # agl+ipc2018-fd-2018+config34
    (112, ['ipc2018-fd-2018', 'config34']),
    # agl+ipc2018-cerberus+agl
    (151, ['ipc2018-cerberus', 'agl']),
    # agl+ipc2018-fd-2018+config28
    (4, ['ipc2018-fd-2018', 'config28']),
    # agl+ipc2018-fd-2018+config05
    (5, ['ipc2018-fd-2018', 'config05']),
    # agl+ipc2018-decstar+agl-config01
    (164, ['ipc2018-decstar', 'agl-config01']),
    # agl+ipc2014-agl-mpc+default
    (79, ['ipc2014-agl-mpc', 'default']),
    # agl+ipc2018-saarplan+agl-config01
    (278, ['ipc2018-saarplan', 'agl-config01']),
    # agl+ipc2018-fd-2018+config15
    (31, ['ipc2018-fd-2018', 'config15']),
    # agl+ipc2018-fd-2018+config37
    (16, ['ipc2018-fd-2018', 'config37']),
    # agl+ipc2018-fd-2018+config42
    (17, ['ipc2018-fd-2018', 'config42']),
    # agl+ipc2018-fd-2018+config36
    (10, ['ipc2018-fd-2018', 'config36']),
    # agl+ipc2018-fd-2018+config32
    (21, ['ipc2018-fd-2018', 'config32']),
    # agl+ipc2018-decstar+agl-config00
    (11, ['ipc2018-decstar', 'agl-config00']),
    # agl+ipc2018-freelunch-madagascar+default
    (51, ['ipc2018-freelunch-madagascar', 'default']),
    # agl+ipc2018-fd-2018+config11
    (164, ['ipc2018-fd-2018', 'config11']),
    # agl+ipc2018-lapkt-bfws+dual-bfws-agl
    (28, ['ipc2018-lapkt-bfws', 'dual-bfws-agl']),
    # agl+ipc2018-fd-2018+config35
    (63, ['ipc2018-fd-2018', 'config35']),
    # agl+ipc2018-fd-2018+config13
    (17, ['ipc2018-fd-2018', 'config13']),
    # agl+ipc2018-fd-2018+config40
    (25, ['ipc2018-fd-2018', 'config40']),
    # agl+ipc2018-fd-2018+config09
    (26, ['ipc2018-fd-2018', 'config09']),
    # agl+ipc2018-fd-2018+config22
    (26, ['ipc2018-fd-2018', 'config22']),
]
