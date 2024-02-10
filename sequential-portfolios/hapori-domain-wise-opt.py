"""
Portfolio generator: DomainwisePortfolio

Time for computing portfolio: 1.06s
Score: 1403.00
Average score quota: 0.72
Standard deviation of score quota: 0.23
Training set: learn-opt-portfolios-eval

Settings:
   Maximum plantime: 1800
   Absolute score: True
"""

PLANNERS = [
    # opt+ipc2014-opt-symba1+default
    (475, ['ipc2014-opt-symba1', 'default']),
    # opt+ipc2018-decstar+opt-config00
    (13, ['ipc2018-decstar', 'opt-config00']),
    # opt+ipc2018-decstar+opt-config03
    (12, ['ipc2018-decstar', 'opt-config03']),
    # opt+ipc2018-decstar+opt-config05
    (272, ['ipc2018-decstar', 'opt-config05']),
    # opt+ipc2018-decstar+opt-config06
    (5, ['ipc2018-decstar', 'opt-config06']),
    # opt+ipc2018-opt-delfi+h2-simpless-dks-900masb50ksbmiasm
    (247, ['ipc2018-opt-delfi', 'h2-simpless-dks-900masb50ksbmiasm']),
    # opt+ipc2018-opt-delfi+h2-simpless-dks-cpdbshc900
    (184, ['ipc2018-opt-delfi', 'h2-simpless-dks-cpdbshc900']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-900masb50ksbmiasm
    (37, ['ipc2018-opt-delfi', 'h2-simpless-oss-900masb50ksbmiasm']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-blind
    (99, ['ipc2018-opt-delfi', 'h2-simpless-oss-blind']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-cpdbshc900
    (89, ['ipc2018-opt-delfi', 'h2-simpless-oss-cpdbshc900']),
    # opt+ipc2018-opt-delfi+h2-simpless-oss-masginfsccdfp
    (18, ['ipc2018-opt-delfi', 'h2-simpless-oss-masginfsccdfp']),
    # opt+ipc2018-opt-metis+metis2
    (139, ['ipc2018-opt-metis', 'metis2']),
    # opt+ipc2018-opt-scorpion+default
    (212, ['ipc2018-opt-scorpion', 'default']),
]
