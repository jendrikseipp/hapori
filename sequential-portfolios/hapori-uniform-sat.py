"""
Portfolio generator: UniformPortfolio

Time for computing portfolio: 0.04s
Score: 1289.76
Average score quota: 0.60
Standard deviation of score quota: 0.34
Training set: learn-sat-portfolios-eval

Settings:
   Maximum plantime: 1800
   Absolute score: True
   Subset size: 80
"""

PLANNERS = [
    # sat+ipc2014-jasper+default
    (22, ['ipc2014-jasper', 'default']),
    # sat+ipc2018-cerberus+sat
    (22, ['ipc2018-cerberus', 'sat']),
    # sat+ipc2018-cerberus+sat-gl
    (22, ['ipc2018-cerberus', 'sat-gl']),
    # sat+ipc2018-decstar+sat-config00
    (22, ['ipc2018-decstar', 'sat-config00']),
    # sat+ipc2018-decstar+sat-config01
    (22, ['ipc2018-decstar', 'sat-config01']),
    # sat+ipc2018-decstar+sat-config02
    (22, ['ipc2018-decstar', 'sat-config02']),
    # sat+ipc2018-decstar+sat-config03
    (22, ['ipc2018-decstar', 'sat-config03']),
    # sat+ipc2018-fd-2018+config00
    (22, ['ipc2018-fd-2018', 'config00']),
    # sat+ipc2018-fd-2018+config01
    (22, ['ipc2018-fd-2018', 'config01']),
    # sat+ipc2018-fd-2018+config02
    (22, ['ipc2018-fd-2018', 'config02']),
    # sat+ipc2018-fd-2018+config03
    (22, ['ipc2018-fd-2018', 'config03']),
    # sat+ipc2018-fd-2018+config04
    (22, ['ipc2018-fd-2018', 'config04']),
    # sat+ipc2018-fd-2018+config05
    (22, ['ipc2018-fd-2018', 'config05']),
    # sat+ipc2018-fd-2018+config06
    (22, ['ipc2018-fd-2018', 'config06']),
    # sat+ipc2018-fd-2018+config07
    (22, ['ipc2018-fd-2018', 'config07']),
    # sat+ipc2018-fd-2018+config08
    (22, ['ipc2018-fd-2018', 'config08']),
    # sat+ipc2018-fd-2018+config09
    (22, ['ipc2018-fd-2018', 'config09']),
    # sat+ipc2018-fd-2018+config10
    (22, ['ipc2018-fd-2018', 'config10']),
    # sat+ipc2018-fd-2018+config11
    (22, ['ipc2018-fd-2018', 'config11']),
    # sat+ipc2018-fd-2018+config12
    (22, ['ipc2018-fd-2018', 'config12']),
    # sat+ipc2018-fd-2018+config13
    (22, ['ipc2018-fd-2018', 'config13']),
    # sat+ipc2018-fd-2018+config14
    (22, ['ipc2018-fd-2018', 'config14']),
    # sat+ipc2018-fd-2018+config15
    (22, ['ipc2018-fd-2018', 'config15']),
    # sat+ipc2018-fd-2018+config16
    (22, ['ipc2018-fd-2018', 'config16']),
    # sat+ipc2018-fd-2018+config17
    (22, ['ipc2018-fd-2018', 'config17']),
    # sat+ipc2018-fd-2018+config18
    (22, ['ipc2018-fd-2018', 'config18']),
    # sat+ipc2018-fd-2018+config19
    (22, ['ipc2018-fd-2018', 'config19']),
    # sat+ipc2018-fd-2018+config20
    (22, ['ipc2018-fd-2018', 'config20']),
    # sat+ipc2018-fd-2018+config21
    (22, ['ipc2018-fd-2018', 'config21']),
    # sat+ipc2018-fd-2018+config22
    (22, ['ipc2018-fd-2018', 'config22']),
    # sat+ipc2018-fd-2018+config23
    (22, ['ipc2018-fd-2018', 'config23']),
    # sat+ipc2018-fd-2018+config24
    (22, ['ipc2018-fd-2018', 'config24']),
    # sat+ipc2018-fd-2018+config25
    (22, ['ipc2018-fd-2018', 'config25']),
    # sat+ipc2018-fd-2018+config26
    (22, ['ipc2018-fd-2018', 'config26']),
    # sat+ipc2018-fd-2018+config27
    (22, ['ipc2018-fd-2018', 'config27']),
    # sat+ipc2018-fd-2018+config28
    (22, ['ipc2018-fd-2018', 'config28']),
    # sat+ipc2018-fd-2018+config29
    (22, ['ipc2018-fd-2018', 'config29']),
    # sat+ipc2018-fd-2018+config30
    (22, ['ipc2018-fd-2018', 'config30']),
    # sat+ipc2018-fd-2018+config31
    (22, ['ipc2018-fd-2018', 'config31']),
    # sat+ipc2018-fd-2018+config32
    (22, ['ipc2018-fd-2018', 'config32']),
    # sat+ipc2018-fd-2018+config33
    (22, ['ipc2018-fd-2018', 'config33']),
    # sat+ipc2018-fd-2018+config34
    (22, ['ipc2018-fd-2018', 'config34']),
    # sat+ipc2018-fd-2018+config35
    (22, ['ipc2018-fd-2018', 'config35']),
    # sat+ipc2018-fd-2018+config36
    (22, ['ipc2018-fd-2018', 'config36']),
    # sat+ipc2018-fd-2018+config37
    (22, ['ipc2018-fd-2018', 'config37']),
    # sat+ipc2018-fd-2018+config38
    (22, ['ipc2018-fd-2018', 'config38']),
    # sat+ipc2018-fd-2018+config39
    (22, ['ipc2018-fd-2018', 'config39']),
    # sat+ipc2018-fd-2018+config40
    (22, ['ipc2018-fd-2018', 'config40']),
    # sat+ipc2018-fd-2018+config41
    (22, ['ipc2018-fd-2018', 'config41']),
    # sat+ipc2018-fd-2018+config42
    (22, ['ipc2018-fd-2018', 'config42']),
    # sat+ipc2018-fd-2018+config43
    (22, ['ipc2018-fd-2018', 'config43']),
    # sat+ipc2018-fd-2018+config44
    (22, ['ipc2018-fd-2018', 'config44']),
    # sat+ipc2018-fd-2018+config45
    (22, ['ipc2018-fd-2018', 'config45']),
    # sat+ipc2018-fd-2018+config46
    (22, ['ipc2018-fd-2018', 'config46']),
    # sat+ipc2018-fd-2018+config47
    (22, ['ipc2018-fd-2018', 'config47']),
    # sat+ipc2018-fd-2018+config48
    (22, ['ipc2018-fd-2018', 'config48']),
    # sat+ipc2018-fd-2018+config49
    (22, ['ipc2018-fd-2018', 'config49']),
    # sat+ipc2018-fd-2018+config50
    (22, ['ipc2018-fd-2018', 'config50']),
    # sat+ipc2018-fd-2018+config51
    (22, ['ipc2018-fd-2018', 'config51']),
    # sat+ipc2018-fd-2018+config52
    (22, ['ipc2018-fd-2018', 'config52']),
    # sat+ipc2018-fd-2018+config53
    (22, ['ipc2018-fd-2018', 'config53']),
    # sat+ipc2018-fd-2018+config54
    (22, ['ipc2018-fd-2018', 'config54']),
    # sat+ipc2018-fd-2018+config55
    (22, ['ipc2018-fd-2018', 'config55']),
    # sat+ipc2018-fd-2018+config56
    (22, ['ipc2018-fd-2018', 'config56']),
    # sat+ipc2018-fd-2018+config57
    (22, ['ipc2018-fd-2018', 'config57']),
    # sat+ipc2018-fd-2018+config58
    (22, ['ipc2018-fd-2018', 'config58']),
    # sat+ipc2018-fd-2018+config59
    (22, ['ipc2018-fd-2018', 'config59']),
    # sat+ipc2018-fd-2018+config60
    (22, ['ipc2018-fd-2018', 'config60']),
    # sat+ipc2018-fd-2018+config61
    (22, ['ipc2018-fd-2018', 'config61']),
    # sat+ipc2018-freelunch-madagascar+default
    (22, ['ipc2018-freelunch-madagascar', 'default']),
    # sat+ipc2018-lapkt-bfws+bfws-pref-sat
    (22, ['ipc2018-lapkt-bfws', 'bfws-pref-sat']),
    # sat+ipc2018-lapkt-bfws+dual-bfws-sat
    (22, ['ipc2018-lapkt-bfws', 'dual-bfws-sat']),
    # sat+ipc2018-lapkt-bfws+poly-bfws
    (22, ['ipc2018-lapkt-bfws', 'poly-bfws']),
    # sat+ipc2018-lapkt-dfs-plus+default
    (22, ['ipc2018-lapkt-dfs-plus', 'default']),
    # sat+ipc2018-mercury2014+sat
    (22, ['ipc2018-mercury2014', 'sat']),
    # sat+ipc2018-merwin+sat
    (22, ['ipc2018-merwin', 'sat']),
    # sat+ipc2018-olcff+default
    (22, ['ipc2018-olcff', 'default']),
    # sat+ipc2018-saarplan+sat-config02
    (22, ['ipc2018-saarplan', 'sat-config02']),
    # sat+ipc2018-symple1+symple100000SAT
    (22, ['ipc2018-symple1', 'symple100000SAT']),
    # sat+ipc2018-symple2+symple100000SAT
    (22, ['ipc2018-symple2', 'symple100000SAT']),
]
