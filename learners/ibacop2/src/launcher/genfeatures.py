import os 
import sys 
import time 
import math
import argparse
import pickle
import pandas as pd

from sklearn.ensemble import RandomForestClassifier

from ftglobals import *


def extract_features(rootpath, domain, problem, ft_file):
    # ##Features
    timeTranslate = 0
    timePreprocess = 0
    timeFFLearner = 0
    timeHeuristic = 0
    timeLandmark = 0
    timeMercury = 0
   
    # try:
    #     if(data):
    command = ("ulimit -v 4194304;ulimit -t 50; python2.7 " + rootpath + 
               "/features/translate/translate.py " + domain + " " + problem)
    print "Run command: " + str(command)

    os.system(command)

    timeTranslate = time.time()
    command =  ("ulimit -v 4194304;ulimit -t 50; " + rootpath + 
                "/features/preprocess/preprocess < " + startpath + "/output.sas")
    print "Run command: " + str(command)
    os.system(command)

    timePreprocess = time.time()
    command = "ulimit -v 4194304;ulimit -t 50; " + rootpath + "/features/ff-learner/roller3.0 -o " + domain + " -f " + problem + " -S 28"
    print "Run command: " + str(command)
    os.system(command)

    timeFFLearner = time.time()
    command = "ulimit -v 4194304;ulimit -t 100; " + rootpath + "/features/heuristics/training.sh "  + domain + " " + problem
    print "Run command: " + str(command)
    os.system(command)

    timeHeuristic = time.time()
    command = "ulimit -v 4194304;ulimit -t 100; "+  rootpath +"/search/downward-4 --landmarks \"lm=lm_merged([lm_hm(m=1),lm_rhw(),lm_zg()])\" < " + startpath + "/output"
    os.system(command)
    timeLandmark = time.time()
    ## print "Run command: " + str(command)            

    #TDR: no encuentro esto tampoco 
    command = "ulimit -v 4194304;ulimit -t 100; "+  rootpath + "/search-mercury/downward ipc seq-agl-mercury <" + startpath + "/output"
    timeMercury = time.time()
    print "Run command: " + str(command)
    os.system(command)
    
    actual_rootpath = rootpath + "/models"
    command = "python2.7 "+ actual_rootpath + "/output_features.py " + ft_file
    print "Run command: " + str(command)
    os.system(command)



def compute_planners_solveprob(model_file, ft_file, avail_time, n_planners=5):

    with open(model_file, 'rb') as fi:
        model_data = pickle.load(fi)

    if os.path.exists(ft_file):
        data = pd.read_csv(ft_file, na_values='?')
    else:
        return {}

    data['cpt_fact_balance'] = data.h_ff_ratio.notnull().astype(int)
    data['cpt_heuristics'] = data.Goal_count.notnull().astype(int)
    data['cpt_landmarks'] = data.n_landmarks.notnull().astype(int)
    data['cpt_redblack'] = data.blackVariables.notnull().astype(int)

    exclude = [c for c in excled_cols if c in data.columns]

    raw_features = data.drop(exclude, axis=1)
    planners = model_data['planners']
    print('Available planners:', planners)
    
    ft_w_planners = []
    for iplanner in planners:
        ip_data = raw_features.copy()
        ip_data['planner'] = iplanner
        ft_w_planners.append(ip_data)
    raw_features_planner = pd.concat(ft_w_planners)
    raw_features_planner.fillna(model_data['fillna_avg'], inplace=True)
    
    features = pd.get_dummies(raw_features_planner)
    features = features[model_data['columns']]

    model = model_data['model']
    print("classes:", model.classes_)
    solve_proba = model.predict_proba(features)[:,1]
    plan_solprob = {p: prob for p, prob in zip(planners, solve_proba)}

    selected_planners = pd.Series(plan_solprob).sort_values(ascending=False)
    print(selected_planners)
    
    base_planner_time = int(avail_time/n_planners)
    planner_list = selected_planners.index[:n_planners]
    planner_schedule = {pl: base_planner_time for pl in planner_list}
    return planner_schedule


def get_parser():
    desc = """
This program uses PDDL domain and problem files to generate a set of features
for the empirical performance modelling of planners
""" 
    parser = argparse.ArgumentParser(
                    prog='IBaCoP Feature Generator and Predictor',
                    description=desc,                    
                    )
    parser.add_argument(
        '-d', '--domain', 
        help="PDDL domain file",
        required=True
    )
    parser.add_argument(
        '-p', '--problem', 
        help="PDDL problem file",
        required=True)      
    parser.add_argument(
        '-f', '--features', 
        help="feature CSV output file", 
        default='all_features.csv'
    )
    parser.add_argument(
        '-m', '--model', 
        help = ("sklearn model data pickle file. When included, "
                "it provides a list of planner probabilities for "
                "solving current task")
    )
    parser.add_argument(
        '-o', '--outproba', 
        help = 'output file with planner probabilities',
        default = 'planner_schedule.txt'
    )
    parser.add_argument(
        '-t', '--fulltime', 
        help='overall time limit for the portfolio',
        default=1800,
        type=int
    )

    parser.add_argument(
        '-n', '--nplanners', 
        help='number of planners to include in schedule',
        default=5,
        type=int
    )
    
    return parser 

# main
# -----------------------------------------------------------------------------
if __name__ == '__main__':
    # planners = []
    # timeouts = []
    # default_planners = ["jasper", "mercury", "bfs-f","siw", "fdss-2" , "probe", "yahsp2-mt"]
    # default_timeouts = [257, 257, 257, 257, 257, 257, 257]
    # ##memory   = 9395240960 # 3,95 GB
    # memory   = 8482560410
    # original_data = True
    # timelimit = 1800
    # mem = virtual_memory()
    # best_cost = -1
    # counter = 1
    # if (mem.total < memory):
    #     memory = int(mem.total*0.9)
    #     timelimit = 280
    #     sys.stderr.write('WARNING IT IS A TEST CONFIGURATION')
    #     print "\n***** The memory limit is: ", str(memory)
    #     print "***************************\n"
    #     print "WARNING: IT IS A TEST CONFIGURATION ***************\n"
    #     print "***************************\n"
    # # Check params
    parser = get_parser()
    args = parser.parse_args()

    original_domain = os.path.abspath(args.domain)
    original_problem = os.path.abspath(args.problem)
    
    ft_file = args.features

    # # Getting root path
    pathname = os.path.dirname(sys.argv[0])
    currentpath = os.path.abspath(pathname)
    startpath = os.getcwd()
    rootpath = os.path.abspath(os.path.join(currentpath,".."))
    pddlpath = os.path.abspath(os.path.dirname(sys.argv[1]))
    rootpathOutput = startpath
    
    # # Getting modified paths
    modified_domain  = pddlpath + "/domain_ready.txt"
    modified_problem = pddlpath + "/problem_ready.txt"
    plans_folder = rootpathOutput 
    print "plan folder", plans_folder
    print "root" , rootpath
    print "src curr", currentpath
    print "Curr", startpath
    
    begin_time = time.time()
    # # Parse original domain and original problem (conditional effects)
    # begin = time.time()
    command = "python2.7 " + rootpath + "/parser/parse.py " + original_domain + " " + original_problem
    print "Run command: " + str(command)
    os.system(command)
    # end = time.time()

    init_time_features = time.time()
    if((os.path.isfile(modified_domain)) and (os.path.isfile(modified_problem))):
        print "\nModified domain/problem detected\n"
        original_data = False
        extract_features(
            rootpath, modified_domain, modified_problem, args.features
        )
    else:
        print "\nPortfolio runs original domain/problem\n"
        extract_features(
            rootpath, original_domain, original_problem, args.features
        )

    accumulated_time = math.ceil(time.time() - begin_time)
    # print "Parser took " + str(accumulated_time) + " seconds\n"

    if args.model is not None and args.outproba is not None:
        model_file = os.path.abspath(args.model)
        avail_time = args.fulltime  - accumulated_time
        planner_schedule = compute_planners_solveprob(
            model_file, ft_file, avail_time, args.nplanners
        )
        print("Plan schedule")
        print(planner_schedule)
        with open('ibacop_planners.out','w') as f:
            f.write("SCHEDULE = " + str(planner_schedule) + "\n")
        

