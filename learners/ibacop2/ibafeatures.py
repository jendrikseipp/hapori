import sys
from time import sleep 
import os
import argparse
import pandas as pd


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--path', required=True) 
    parser.add_argument('-o', '--output', required=True)
    parser.add_argument('-s', '--subset')
    parser.add_argument('-d', '--domain', default='domain.pddl')
    parser.add_argument('-m', '--matched', action='store_true')
    args = parser.parse_args()
    
    clean_cmd = './cleanOutput'
    clean_cpddl = './cleanCompiledPDDL ' + args.path 
    ft_file = "all_features.csv"

    os.system(clean_cmd)
    list_fts = []
    if args.domain is not None:
        dompath = os.path.join(args.path, args.domain)

    strpath = args.path if args.path[-1] != '/' else args.path[:-1]
    domain_name = os.path.basename(strpath)
    print("Domain name:", domain_name)
    problems = sorted(os.listdir(args.path))
    print(args.subset)
    if args.subset:
        if isinstance(args.subset, int):
            problems = problems[:args.subset]
        elif isinstance(args.subset, str) and os.path.exists(args.subset):
            tasks = pd.read_csv(args.subset)
            dom_tasks = tasks[tasks.domain == domain_name]
            problems = dom_tasks.problem
            print(problems)

    # print(problems)
    
    for f in problems:
        # check if current path is a file
        if f.startswith('domain-'):
            continue 

        if args.matched:
            dompath = fpath = os.path.join(args.path, 'domain-' + f)
        fpath = os.path.join(args.path, f)

        if os.path.isfile(fpath) and args.domain != f:
            os.system(clean_cmd)
            os.system(clean_cpddl)

            print("Features from " + fpath)
            cmd = "python2.7 src/launcher/genfeatures.py -d " + dompath + " -p " + fpath 
            print(cmd)
            os.system(cmd)
            sleep(1)
            if os.path.exists(ft_file):
                features = pd.read_csv(ft_file, na_values='?')
                features['problem_name'] = f
                list_fts.append(features)
            else:
                print("No Features generated:", fpath)
    ft_df = pd.concat(list_fts)
    ft_df.to_csv(args.output, index=False)
