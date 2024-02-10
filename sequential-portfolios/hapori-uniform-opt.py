"""
Portfolio generator: UniformPortfolio

Time for computing portfolio: 0.01s
Score: 1201.00
Average score quota: 0.63
Standard deviation of score quota: 0.27
Training set: learn-opt-portfolios-eval

Settings:
   Maximum plantime: 1800
   Absolute score: True
   Subset size: 30
"""

PLANNERS = [
    # opt+ipc2014-opt-symba1+default
    (60, ['ipc2014-opt-symba1', 'default']),
    # opt+ipc2018-decstar+opt-config00
    (60, ['ipc2018-decstar', 'opt-config00']),
    # opt+ipc2018-decstar+opt-config01
    (60, ['ipc2018-decstar', 'opt-config01']),
    # opt+ipc2018-decstar+opt-config02
    (60, ['ipc2018-decstar', 'opt-config02']),
    # opt+ipc2018-decstar+opt-config03
    (60, ['ipc2018-decstar', 'opt-config03']),
    # opt+ipc2018-decstar+opt-config04
    (60, ['ipc2018-decstar', 'opt-config04']),
    # opt+ipc2018-decstar+opt-config05
    (60, ['ipc2018-decstar', 'opt-config05']),
    # opt+ipc2018-decstar+opt-config06
    (60, ['ipc2018-decstar', 'opt-config06']),
    # opt+ipc2018-opt-complementary2+default
    (60, ['ipc2018-opt-complementary2', 'default']),
    # opt+ipc2018-opt-delfi+h2-simpless-dks-900masb50ksbmiasm
    (60, ['ipc2018-opt-delfi', 'h2-simpless-dks-900masb50ksbmiasm']),
    # opt+ipc2018-opt-delfi+h2-simpless-dks-900masb50ksccdfp
    (60, ['ipc2018-opt-delfi', 'h2-simpless-dks-900masb50ksccdfp']),
    # opt+ipc2018-opt-delfi+h2-simpless-dks-900masginfsccdfp
    (60, ['ipc2018-opt-delfi', 'h2-simpless-dks-900masginfsccdfp']),
    # opt+ipc2018-opt-delfi+h2-simpless-dks-blind
    (60, ['ipc2018-opt-delfi', 'h2-simpless-dks-blind']),
    # opt+ipc2018-opt-delfi+h2-simpless-dks-celmcut
    (60, ['ipc2018-opt-delfi', 'h2-simpless-dks-celmcut']),
    # opt+ipc2018-opt-delfi+h2-simpless-dks-cpdbshc900
    (60, ['ipc2018-opt-delfi', 'h2-simpless-dks-cpdbshc900']),
    # opt+ipc2018-opt-delfi+h2-simpless-dks-zopdbsgenetic
    (60, ['ipc2018-opt-delfi', 'h2-simpless-dks-zopdbsgenetic']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-900masb50ksbmiasm
    (60, ['ipc2018-opt-delfi', 'h2-simpless-oss-900masb50ksbmiasm']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-900masb50ksccdfp
    (60, ['ipc2018-opt-delfi', 'h2-simpless-oss-900masb50ksccdfp']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-blind
    (60, ['ipc2018-opt-delfi', 'h2-simpless-oss-blind']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-celmcut
    (60, ['ipc2018-opt-delfi', 'h2-simpless-oss-celmcut']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-cpdbshc900
    (60, ['ipc2018-opt-delfi', 'h2-simpless-oss-cpdbshc900']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-masginfsccdfp
    (60, ['ipc2018-opt-delfi', 'h2-simpless-oss-masginfsccdfp']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-zopdbsgenetic
    (60, ['ipc2018-opt-delfi', 'h2-simpless-oss-zopdbsgenetic']),
    # opt+ipc2018-opt-delfi+simpless-dks-masb50kmiasmdfp
    (60, ['ipc2018-opt-delfi', 'simpless-dks-masb50kmiasmdfp']),
    # opt+ipc2018-opt-delfi+simpless-oss-masb50kmiasmdfp
    (60, ['ipc2018-opt-delfi', 'simpless-oss-masb50kmiasmdfp']),
    # opt+ipc2018-opt-metis+metis2
    (60, ['ipc2018-opt-metis', 'metis2']),
    # opt+ipc2018-opt-planning-pdbs+default
    (60, ['ipc2018-opt-planning-pdbs', 'default']),
    # opt+ipc2018-opt-scorpion+default
    (60, ['ipc2018-opt-scorpion', 'default']),
    # opt+ipc2018-symple1+symple100000OPT
    (60, ['ipc2018-symple1', 'symple100000OPT']),
    # opt+ipc2018-symple2+symple100000OPT
    (60, ['ipc2018-symple2', 'symple100000OPT']),
]
