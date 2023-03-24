# import pandas as pd
import os
import sys 
from joinFile import join, readFile
from head import CSV_HEADER_FIELDS

Ntranlate=26
Npreprocess=49
Nheuristic=8
Nlandmark=11
NredBlack=7
NBalance=15

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
