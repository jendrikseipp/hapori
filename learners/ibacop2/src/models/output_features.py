# import pandas as pd
import os
import sys 
from joinFile import join, readFile


Ntranlate=26
Npreprocess=49
Nheuristic=8
Nlandmark=11
NredBlack=7
NBalance=15

CSV_HEADER_FIELDS = [
    "domain",
    "task_name",
    "requirements",
    "types",
    "objects",
    "predicates",
    "functions",
    "init",
    "goal",
    "actions",
    "axioms",
    "use_min_cost_metric",
    "generated_rules",
    "relevant_atoms",
    "auxiliary_atoms",
    "final_queue_length",
    "total_queue_pushes",
    "implied_effects_removed",
    "effect_conditions_simplified",
    "implied_preconditions_added",
    "translator_variables",
    "translator_derived_variables",
    "translator_facts",
    "translator_mutex_groups",
    "translator_total_mutex_groups_size",
    "translator_operators",
    "translator_task_size",
    "numberVariablesCG",
    "highLevelVariablesCG",
    "totalEdgesCG",
    "totalWeightCG",
    "veRatio",
    "weRatio",
    "wvRatio",
    "hvRatio",
    "inputEdgeCGMax",
    "inputEdgeCGAvg",
    "inputEdgeCGStd",
    "outputEdgeCGMax",
    "outputEdgeCGAvg",
    "outputEdgeCGStd",
    "inputWeightCGMax",
    "inputWeightCGAvg",
    "inputWeightCGStd",
    "outputWeightCGMax",
    "outputWeightCGAvg",
    "outputWeightCGStd",
    "inputEdgeHVMax",
    "inputEdgeHVAvg",
    "inputEdgeHVStd",
    "outputEdgeHVMax",
    "outputEdgeHVAvg",
    "outputEdgeHVStd",
    "inputWeightHVMax",
    "inputWeightHVAvg",
    "inputWeightHVStd",
    "outputWeightHVMax",
    "outputWeightHVAvg",
    "outputWeightHVStd",
    "numberVariablesDTG",
    "totalEdgesDTG",
    "totalWeigthDTG",
    "edVaRatioDTG",
    "weEdRatioDTG",
    "weVaRatioDTG",
    "inputEdgeDTGMax",
    "inputEdgeDTGAvg",
    "inputEdgeDTGStd",
    "outputEdgeDTGMax",
    "outputEdgeDTGAvg",
    "outputEdgeDTGStd",
    "inputWeightDTGMax",
    "inputWeightDTGAvg",
    "inputWeightDTGStd",
    "outputWeightDTGMax",
    "outputWeightDTGAvg",
    "outputWeightDTGStd",
    "no",
    "file",
    "num_relevant_facts",
    "num_actions",
    "h_max",
    "h_ff",
    "h_ff_ratio",
    "rp_fact_balance_min",
    "rp_fact_balance_avg",
    "rp_fact_balance_var",
    "rp_goal_balance_min",
    "rp_goal_balance_avg",
    "rp_goal_balance_var",
    
    "balance_Ratio",
    "Unbalance_Ratio",
    "Balance_Distorsion",

    "Additive ",
    "Blind",
    "Causal_graph",
    "Context-enhanced_additive",
    "FF",
    "Goal_count",
    "Landmark_count",
    "Landmark-cut",
    "Max",
   
    "n_landmarks",
    "numberEdges",
    "edVaratioLand",
    "numberFatherNodes",
    "numberChildrenNodes",
    "nodosbetween",
    
    "avginput",
    "maxinput",
    "stdinput",
    
    "avgoutput",
    "maxoutput",
    "stdoutput",
    
    "blackVariables",
    "blackRootVariables",
    
    "redBlackVariables",
    "allpairsvaluesconnected",
    "allvaluesconnectedgoal",
    
    "blackstronglpyarents",
    "Maximaleffectsblack",
    "Hred-black",
]


# def read_sub_ft_file(fpath, n_features):
#     if os.path.exists(fpath):
#         data = pd.read_csv(fpath, header=False)
#     else:
#         data = 

def write_features_file(name, data):
    with open(name,'w') as fd:

        header = ','.join(CSV_HEADER_FIELDS)
        fd.write(header + '\n')
        fd.write(data + '\n')

if __name__ == '__main__':

    translate = []
    preprocess =[]
    fflearner = []
    heuristics =[]
    landmarks = []
    redblack = []
    union_final = ""
    ft_file = "all_features.csv"
    route = os.getcwd()
    if (len(sys.argv) == 2):
        ft_file = sys.argv[1]
    else:
        print "ERROR:::: Need one argument to create the features file" 
        sys.exit(-1)

    try:
	    translate = readFile(route+"/translateFile", translate) ## translateFile
    except:
	    print "No file in translate"
	    translate = ["?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?"]
    try:
	    preprocess = readFile(route+"/features.arff", preprocess) ## features.arff
    except:
	    print "No file in preprocess"
	    preprocess = ["?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?\n"]
    try:
	    fflearner = readFile(route+"/initfeature-info.txt", fflearner) 
    except:
	    print "No file in fflearner"
	    fflearner = ["?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?\n"]
    try:
        ##the route is wrong
	    landmarks = readFile(route+"/landmark.arff", landmarks)
    except:
	    print "No file in landmarks"
	    landmarks = ["?,?,?,?,?,?,?,?,?,?,?,?\n"]
    try:
        ##the route is wrong
	    redblack = readFile(route+"/red-black", redblack)
    except:
	    print "No file in red-black"
	    redblack = ["?,?,?,?,?,?,?,?\n"]
    try:
	    heuristics = readFile(route+"/tmp_results", heuristics)
    except:
	    print "No file in heuristics"
	    heuristics = ["?,?,?,?,?,?,?,?,?\n"]
	    
	    
    try:
	    union_final = join(translate, preprocess, fflearner, heuristics, landmarks,redblack, union_final)
    except:
	    print "General error"
	    union_final = "?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?"

    union_final = union_final.replace("-nan", "?")
    union_final = union_final.replace("nan", "?")
    union_final = union_final.replace("-inf", "?")
    union_final = union_final.replace("inf", "?")
    union_final = union_final.replace("-2147483647", "-1000")
    union_final = union_final.replace("2147483647", "1000")

    write_features_file(ft_file, union_final)    
    # writeFile(route+"/global_features.arff", union_final, head)
