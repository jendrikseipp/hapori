import os
import subprocess

schedule_file = "ibacop_planners.out"

DEFAULT_PLANNERS = [
    (1795, ['ipc2018-saarplan', 'sat-config02']),
]

cwd = os.getcwd()

print("dom", DOMAIN)
print("prob", PROBLEM)
print("time", AVAIL_TIME)
print("CWD", cwd)

if os.path.exists(schedule_file):
    os.remove(schedule_file)

PLANNERS = DEFAULT_PLANNERS

base_path = "/planner/learners/ibacop2/"
model_file = os.path.join(base_path, "src/models/iba_model_rf_img_journal_sat.pkl")
n_planners = "5"

cpt_features_program = os.path.join(base_path,"src/launcher/genfeatures.py")
cmd_args = [
    "-d", DOMAIN,
    "-p", PROBLEM,
    "-f", 'ibacop_task_features.csv',
    "-t", str(AVAIL_TIME),
    "-m", model_file,
    "-n", n_planners,
]
subprocess.run(['python2.7', cpt_features_program] + cmd_args)

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
        PLANNERS = []
        for algo, t in schedule.items():
            algo_parts = algo.split('+')
            iplanner = (t, [algo_parts[1], algo_parts[2]])
            PLANNERS.append(iplanner)
        
        print("IBAcOP Ready to plan")
        print(PLANNERS)
