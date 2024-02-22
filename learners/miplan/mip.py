from pulp import *

def mip(data, solvers, tasks, track, suboptimality_gap, mip_time, outfile, minimize_time, min_quality):
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
    if not minimize_time:
        w1 = 1 # Quality of the portfolio
        w2 = 0 # Execution time of the portfolio
    else:
        w1 = 0
        w2 = 1

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

    if minimize_time:
    # Constraint to add in the second optimization run depending on the quality score obtained in the first run
        prob += lpSum([quality[t] for t in tasks]) >= min_quality

    print('***MODEL BUILT, READY TO SOLVE***')
    # Depending on the available solver
    solver = PULP_CBC_CMD(msg=True, timeLimit=mip_time, gapRel=suboptimality_gap)
    #solver = CPLEX_PY(msg=True,timeLimit=mip_time,gapRel=suboptimality_gap)
    prob.solve(solver)
    solving_status = 1 if LpStatus[prob.status] == 'Optimal' else 0

    # Output the solution
    solution_file = open(outfile, 'w+')
    solution_file.write(f'PLANNERS = [\n')
    print('***SOLUTION***')
    if solving_status == 1:
        for v in prob.variables():
            if 'time' in v.name:
                if v.varValue > 0:
                    planner = v.name.split('PLUS')[1].replace('DASH','-')
                    config = v.name.split('PLUS')[-1].replace('DASH','-')
                    solution_file.write(f'({v.varValue}, ["{planner}", "{config}"]),\n')
                    print(f'{v.name} = {v.varValue}')
    solution_file.write(']')
    solution_file.close()
    print(f'Solution quality {prob.objective.value()}')
    return prob.objective.value()