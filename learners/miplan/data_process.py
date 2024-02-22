import pandas as pd
import os
import math
import lzma
import json

relevant_columns = [
    'algorithm',
    'cost',
    'coverage',
    'domain',
    'wall_time',
    'problem',
    'time_limit'
]

def obtain_relevant_data_2(properties, track):
    df_list = []
    for key, value in properties.items():
        try:
            this_result = {
                'algorithm': value['algorithm'],
                'cost': value['cost'],
                'coverage': value['coverage'],
                'domain': value['domain'],
                'wall_time': value['wall_time'],
                'problem': value['problem'],
                'time_limit': value['time_limit']
            }
            df_list.append(this_result)
        except:
            print(f'The planner {key} dont have the right columns')
    df = pd.DataFrame(df_list)
    df['quality_mip'] = 'None'
    df['runtime_mip'] = 'None'
    return df



def obtain_relevant_data(data_directory, track):
    df_list = []
    for file in os.listdir(data_directory):
        raw_df = pd.read_json(data_directory + '/' + file).transpose()
        try:
            planner_df = raw_df[relevant_columns]
            df_list.append(planner_df)
        except:
            print(f'The planner {file} dont have the right columns')
    df = pd.concat(df_list)
    df['quality_mip'] = 'None'
    df['runtime_mip'] = 'None'
    # Uncomment line below when running on all data
    #df['algorithm'] = [','.join(map(str, l)) for l in df['algorithm']]
    if track == 'opt':
        df = df[(df["algorithm"].str.contains('opt') | (df["algorithm"].str.contains('OPT')))]
    return df

# Preprocess the data as needed by the MIP
def get_mip_data(df, track):
    dictionary = {}
    problem_domain_combinations = list(df.groupby(['domain', 'problem'], group_keys=True).indices)
    for combination in problem_domain_combinations:
        combination_str = '-'.join(combination)
        task = '||'.join(combination).replace('.pddl','').replace('.','').replace('_','')
        solutions = df[(df['domain'] == combination[0]) & (df['problem'] == combination[1])]
        min_solutions = solutions[solutions['coverage'] == 1]
        if len(min_solutions.index) == 0: # In this case no planner could solve it, so no need to include it
            print(f'{combination} could not be solved by any planner')
            continue
        else:
            lowest_solution_cost = min(min_solutions['cost'])
            for index, row in solutions.iterrows():
                #solver = index.replace('-' + combination_str, '')
                solver = row['algorithm']
                solver = solver.replace('+', 'PLUS').replace('-', 'DASH').replace('_','UNDER')
                if solver not in dictionary.keys():
                    dictionary[solver] = {}
                if task not in dictionary[solver].keys():
                    dictionary[solver][task] = {}
                if row['coverage'] == 1:
                    dictionary[solver][task]['runtime_mip'] = row['wall_time']
                    if track == 'agl':
                        if row['wall_time'] < 1:
                            dictionary[solver][task]['quality_mip'] = 1
                        else:
                            dictionary[solver][task]['quality_mip'] = 1 - (math.log(row['wall_time']) / math.log(300))
                    else:
                        if row['cost'] == 0 and lowest_solution_cost == 0:
                            dictionary[solver][task]['quality_mip'] = 1
                        elif row['cost'] == 0 and lowest_solution_cost != 0:
                            raise ValueError('cost is 0 but lowest solution cost is higher!')
                        else:
                            dictionary[solver][task]['quality_mip'] = lowest_solution_cost / row['cost']
                else:
                    dictionary[solver][task]['quality_mip'] = 0
                    if track == 'agl':
                        dictionary[solver][task]['runtime_mip'] = 301
                    else:
                        dictionary[solver][task]['runtime_mip'] = 1801
    return dictionary

def analyze_data(df):
    print('Coverage analysis')
    planners = df.algorithm.unique()
    for planner in planners:
        this_planner = df[df['algorithm'] == planner]
        this_planner_solved = this_planner[this_planner['coverage'] == 1]
        try:
            print(f"{planner}: {df.loc[df['algorithm'] == planner, 'coverage'].sum()}")
            print(f"{planner} time : {this_planner_solved['wall_time'].mean()} ({this_planner_solved['wall_time'].std()})")
        except:
            pass

def read_properties(path):
    open_func = lzma.open if path.endswith(".xz") else open
    with open_func(path) as f:
        return json.load(f)

def get_data(data_directory, track):
    properties = read_properties(data_directory)
    df = obtain_relevant_data_2(properties, track)
    #analyze_data(df)
    print('*Relevant data obtained*')
    dictionary = get_mip_data(df, track)
    solvers = list(dictionary.keys())
    tasks = list(dictionary[solvers[0]].keys())
    return dictionary, solvers, tasks