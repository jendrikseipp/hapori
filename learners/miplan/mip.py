from pulp import *

"""def get_time(df, s, t): # This is equivalent to the r(p,t,s) parameter
    domain = t.split('||')[0]
    problem = t.split('||')[1]
    runtime_mip = df[(df['algorithm'] == s) & (df['domain'] == domain) & (df['problem'] == problem)]['runtime_mip'].values[0]
    return runtime_mip

def get_quality(df, s, t): # This is equivalent to the q(p,t,s) parameter
    domain = t.split('||')[0]
    problem = t.split('||')[1]
    quality_mip = \
    df[(df['algorithm'] == s) & (df['domain'] == domain) & (df['problem'] == problem)]['quality_mip'].values[0]
    return quality_mip"""

def mip(data, solvers, tasks, track):
    print('***BUILDING THE MODEL***')
    print(f'{len(solvers)} solvers')
    print(f'{len(tasks)} tasks')

    prob = LpProblem('MIPlan', LpMaximize)

    # Decision variables
    print('Building variables...')
    solved_by = LpVariable.dicts('sb', (solvers,tasks), cat=LpBinary)
    quality = LpVariable.dicts('quality', tasks, 0, 1, cat=LpContinuous)
    if track == 'agl':
        time = LpVariable.dicts('time', solvers, 0, 300, cat=LpInteger)
    else:
        time = LpVariable.dicts('time', solvers, 0, 1800, cat=LpInteger)
    print('Variables built')

    # Weights
    w1 = 1 # Quality of the portfolio
    w2 = 0 # Execution time of the portfolio

    # Objective function
    prob += w1 * lpSum([quality[t] for t in tasks]) + w2 * (1 - lpSum(time[s] for s in solvers))

    # Constraints
    # Do not exceed available time
    if track == 'agl':
        prob += lpSum([time[s] for s in solvers]) <= 300
    else:
        prob += lpSum([time[s] for s in solvers]) <= 1800
    # The alloted time to each solver is equal or higher than the required time
    for s in solvers:
        for t in tasks:
            prob += time[s] >= solved_by[s][t] * data[s][t]['runtime_mip']

    # Constraint to enforce the value of the quality variable
    for t in tasks:
        prob += lpSum([solved_by[s][t] * data[s][t]['quality_mip'] for s in solvers]) == quality[t]

    # Enforce the selection of a single planner to solve a task
    for t in tasks:
        prob += lpSum([solved_by[s][t] for s in solvers]) <= 1

    # Constraint to add in the second optimization run depending on the quality score obtained in the first run
    #prob += lpSum([quality[t] for t in tasks]) >= 2079

    print('***MODEL BUILT, READY TO SOLVE***')
    # Depending on the available solver
    #solver = PULP_CBC_CMD(msg=True, timeLimit=54000)
    solver = CPLEX_PY(msg=True,timeLimit=18000,gapRel=0.04)
    prob.solve(solver)
    solving_status = 1 if LpStatus[prob.status] == 'Optimal' else 0

    # Output the solution
    print('***SOLUTION***')
    if solving_status == 1:
        for v in prob.variables():
            if 'time' in v.name:
                print(f'{v.name} = {v.varValue}')
    print(f'Solution quality {prob.objective.value()}')