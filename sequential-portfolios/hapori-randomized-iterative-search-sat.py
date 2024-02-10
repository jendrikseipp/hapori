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
    # sat+ipc2018-cerberus+sat
    (0, ['ipc2018-cerberus', 'sat']),
    # sat+ipc2018-cerberus+sat-gl
    (0, ['ipc2018-cerberus', 'sat-gl']),
    # sat+ipc2018-decstar+sat-config00
    (0, ['ipc2018-decstar', 'sat-config00']),
    # sat+ipc2018-decstar+sat-config01
    (187, ['ipc2018-decstar', 'sat-config01']),
    # sat+ipc2018-decstar+sat-config02
    (1, ['ipc2018-decstar', 'sat-config02']),
    # sat+ipc2018-decstar+sat-config03
    (0, ['ipc2018-decstar', 'sat-config03']),
    # sat+ipc2018-fd-2018+config00
    (1, ['ipc2018-fd-2018', 'config00']),
    # sat+ipc2018-fd-2018+config01
    (2, ['ipc2018-fd-2018', 'config01']),
    # sat+ipc2018-fd-2018+config02
    (0, ['ipc2018-fd-2018', 'config02']),
    # sat+ipc2018-fd-2018+config03
    (12, ['ipc2018-fd-2018', 'config03']),
    # sat+ipc2018-fd-2018+config04
    (0, ['ipc2018-fd-2018', 'config04']),
    # sat+ipc2018-fd-2018+config05
    (0, ['ipc2018-fd-2018', 'config05']),
    # sat+ipc2018-fd-2018+config06
    (12, ['ipc2018-fd-2018', 'config06']),
    # sat+ipc2018-fd-2018+config07
    (0, ['ipc2018-fd-2018', 'config07']),
    # sat+ipc2018-fd-2018+config08
    (1, ['ipc2018-fd-2018', 'config08']),
    # sat+ipc2018-fd-2018+config09
    (0, ['ipc2018-fd-2018', 'config09']),
    # sat+ipc2018-fd-2018+config10
    (3, ['ipc2018-fd-2018', 'config10']),
    # sat+ipc2018-fd-2018+config11
    (0, ['ipc2018-fd-2018', 'config11']),
    # sat+ipc2018-fd-2018+config12
    (1, ['ipc2018-fd-2018', 'config12']),
    # sat+ipc2018-fd-2018+config13
    (1, ['ipc2018-fd-2018', 'config13']),
    # sat+ipc2018-fd-2018+config14
    (100, ['ipc2018-fd-2018', 'config14']),
    # sat+ipc2018-fd-2018+config15
    (0, ['ipc2018-fd-2018', 'config15']),
    # sat+ipc2018-fd-2018+config16
    (0, ['ipc2018-fd-2018', 'config16']),
    # sat+ipc2018-fd-2018+config17
    (0, ['ipc2018-fd-2018', 'config17']),
    # sat+ipc2018-fd-2018+config18
    (64, ['ipc2018-fd-2018', 'config18']),
    # sat+ipc2018-fd-2018+config19
    (0, ['ipc2018-fd-2018', 'config19']),
    # sat+ipc2018-fd-2018+config20
    (0, ['ipc2018-fd-2018', 'config20']),
    # sat+ipc2018-fd-2018+config21
    (0, ['ipc2018-fd-2018', 'config21']),
    # sat+ipc2018-fd-2018+config22
    (0, ['ipc2018-fd-2018', 'config22']),
    # sat+ipc2018-fd-2018+config23
    (6, ['ipc2018-fd-2018', 'config23']),
    # sat+ipc2018-fd-2018+config24
    (2, ['ipc2018-fd-2018', 'config24']),
    # sat+ipc2018-fd-2018+config25
    (0, ['ipc2018-fd-2018', 'config25']),
    # sat+ipc2018-fd-2018+config26
    (0, ['ipc2018-fd-2018', 'config26']),
    # sat+ipc2018-fd-2018+config27
    (0, ['ipc2018-fd-2018', 'config27']),
    # sat+ipc2018-fd-2018+config28
    (1, ['ipc2018-fd-2018', 'config28']),
    # sat+ipc2018-fd-2018+config29
    (32, ['ipc2018-fd-2018', 'config29']),
    # sat+ipc2018-fd-2018+config30
    (0, ['ipc2018-fd-2018', 'config30']),
    # sat+ipc2018-fd-2018+config31
    (0, ['ipc2018-fd-2018', 'config31']),
    # sat+ipc2018-fd-2018+config32
    (11, ['ipc2018-fd-2018', 'config32']),
    # sat+ipc2018-fd-2018+config33
    (7, ['ipc2018-fd-2018', 'config33']),
    # sat+ipc2018-fd-2018+config34
    (0, ['ipc2018-fd-2018', 'config34']),
    # sat+ipc2018-fd-2018+config35
    (1, ['ipc2018-fd-2018', 'config35']),
    # sat+ipc2018-fd-2018+config36
    (70, ['ipc2018-fd-2018', 'config36']),
    # sat+ipc2018-fd-2018+config37
    (13, ['ipc2018-fd-2018', 'config37']),
    # sat+ipc2018-fd-2018+config38
    (0, ['ipc2018-fd-2018', 'config38']),
    # sat+ipc2018-fd-2018+config39
    (0, ['ipc2018-fd-2018', 'config39']),
    # sat+ipc2018-fd-2018+config40
    (28, ['ipc2018-fd-2018', 'config40']),
    # sat+ipc2018-fd-2018+config41
    (2, ['ipc2018-fd-2018', 'config41']),
    # sat+ipc2018-fd-2018+config42
    (14, ['ipc2018-fd-2018', 'config42']),
    # sat+ipc2018-fd-2018+config43
    (0, ['ipc2018-fd-2018', 'config43']),
    # sat+ipc2018-fd-2018+config44
    (3, ['ipc2018-fd-2018', 'config44']),
    # sat+ipc2018-fd-2018+config45
    (21, ['ipc2018-fd-2018', 'config45']),
    # sat+ipc2018-fd-2018+config46
    (0, ['ipc2018-fd-2018', 'config46']),
    # sat+ipc2018-fd-2018+config47
    (0, ['ipc2018-fd-2018', 'config47']),
    # sat+ipc2018-fd-2018+config48
    (0, ['ipc2018-fd-2018', 'config48']),
    # sat+ipc2018-fd-2018+config49
    (0, ['ipc2018-fd-2018', 'config49']),
    # sat+ipc2018-fd-2018+config50
    (0, ['ipc2018-fd-2018', 'config50']),
    # sat+ipc2018-fd-2018+config51
    (2, ['ipc2018-fd-2018', 'config51']),
    # sat+ipc2018-fd-2018+config52
    (74, ['ipc2018-fd-2018', 'config52']),
    # sat+ipc2018-fd-2018+config53
    (0, ['ipc2018-fd-2018', 'config53']),
    # sat+ipc2018-fd-2018+config54
    (0, ['ipc2018-fd-2018', 'config54']),
    # sat+ipc2018-fd-2018+config55
    (7, ['ipc2018-fd-2018', 'config55']),
    # sat+ipc2018-fd-2018+config56
    (0, ['ipc2018-fd-2018', 'config56']),
    # sat+ipc2018-fd-2018+config57
    (0, ['ipc2018-fd-2018', 'config57']),
    # sat+ipc2018-fd-2018+config58
    (0, ['ipc2018-fd-2018', 'config58']),
    # sat+ipc2018-fd-2018+config59
    (0, ['ipc2018-fd-2018', 'config59']),
    # sat+ipc2018-fd-2018+config60
    (0, ['ipc2018-fd-2018', 'config60']),
    # sat+ipc2018-fd-2018+config61
    (0, ['ipc2018-fd-2018', 'config61']),
    # sat+ipc2018-freelunch-madagascar+default
    (50, ['ipc2018-freelunch-madagascar', 'default']),
    # sat+ipc2018-lapkt-bfws+bfws-pref-sat
    (0, ['ipc2018-lapkt-bfws', 'bfws-pref-sat']),
    # sat+ipc2018-lapkt-bfws+dual-bfws-sat
    (0, ['ipc2018-lapkt-bfws', 'dual-bfws-sat']),
    # sat+ipc2018-lapkt-bfws+poly-bfws
    (369, ['ipc2018-lapkt-bfws', 'poly-bfws']),
    # sat+ipc2018-lapkt-dfs-plus+default
    (65, ['ipc2018-lapkt-dfs-plus', 'default']),
    # sat+ipc2018-mercury2014+sat
    (0, ['ipc2018-mercury2014', 'sat']),
    # sat+ipc2018-merwin+sat
    (0, ['ipc2018-merwin', 'sat']),
    # sat+ipc2018-olcff+default
    (421, ['ipc2018-olcff', 'default']),
    # sat+ipc2018-saarplan+sat-config02
    (0, ['ipc2018-saarplan', 'sat-config02']),
    # sat+ipc2018-symple1+symple100000SAT
    (0, ['ipc2018-symple1', 'symple100000SAT']),
    # sat+ipc2018-symple2+symple100000SAT
    (0, ['ipc2018-symple2', 'symple100000SAT']),
]
