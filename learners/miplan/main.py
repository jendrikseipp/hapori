from data_process import get_data
from mip import mip
import time
import argparse

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("properties_file")
    parser.add_argument("--portfolio-time", default=1800, type=int)
    parser.add_argument("--track", choices=["agl", "opt", "sat"], default="sat")
    parser.add_argument("--outfile", default='hapori-miplan.py')
    parser.add_argument("--suboptimality-gap", default=0.02, type=float)
    parser.add_argument("--mip-time", default=36000, type=float)
    parser.add_argument("--minimize-time", default=False, type=bool)
    return parser.parse_args()

def main():
    args = parse_args()
    init_time = time.time()
    data_dictionary, solvers, tasks = get_data(args.properties_file, args.track)
    end_time = time.time()
    print(f'Data preprocess done in {end_time - init_time}')
    init_time = time.time()
    objective_function_value = mip(data_dictionary, solvers, tasks, args.track, args.suboptimality_gap, args.mip_time, args.outfile, args.minimize_time, 0)
    if args.minimize_time:
        objective_function_value = mip(data_dictionary, solvers, tasks, args.track, args.suboptimality_gap, args.mip_time, args.outfile, args.minimize_time, objective_function_value)
    end_time = time.time()
    print(f'MIP done in {end_time - init_time}')

if __name__ == '__main__':
    main()
