import os
import subprocess

schedule_file = "ibacop_planners.out"

DEFAULT_PLANNERS = [
    (600, ['ipc2014-agl-jasper', 'default']),
    (600, ['ipc2014-agl-probe', 'default']),
    (600, ['ipc2018-lapkt-bfws', 'dual-bfws-agl']),
]

cwd = os.getcwd()

print("dom", DOMAIN)
print("prob", PROBLEM)
print("time", AVAIL_TIME)
print("CWD", cwd)

if os.path.exists(schedule_file):
    os.remove(schedule_file)

PLANNERS = DEFAULT_PLANNERS
model_file = "/planner/learners/ibacop2/src/models/iba_model_data_rf.pkl"
n_planners = "5"

cpt_features_program = "/planner/learners/ibacop2/ibacop-features.sh"

cmd_args = [
    "-d", DOMAIN,
    "-p", PROBLEM,
    "-f", 'ibacop_task_features.csv',
    "-t", str(AVAIL_TIME),
    "-m", model_file,
    "-n", n_planners,
]
subprocess.run([cpt_features_program] + cmd_args)

p_attributes = {}
if os.path.exists(schedule_file):
    with open(schedule_file, "rb") as sf:
        content = sf.read()
        try:
            exec(content, p_attributes)
        except Exception as e:
            print(e)

    if "SCHEDULE" in p_attributes:
        schedule = p_attributes['SCHEDULE']
        PLANNERS = [(t, algo.split(':')) for algo, t in schedule.items()]
        print("IBAcOP Ready to plan")
        print(PLANNERS)
