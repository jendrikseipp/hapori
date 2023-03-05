#!/usr/bin/python2.7
# -*- coding: utf-8 -*-
__author__      = "Isabel Cenamor"
__copyright__   = "Copyright 2018, Portfolio Project Dynamic"
__email__ = "icenamor@inf.uc3m.es"

# imports
# -----------------------------------------------------------------------------
import os               # path and process management
import resource         # process resources
import shutil           # copy files and directories
import signal           # process management
import sys              # argv, exit
import time             # time mgmt

import systools         # IPC process management
import timetools        # IPC timing management
import math             # Ceil the time
from psutil import virtual_memory
# -----------------------------------------------------------------------------

# globals
# -----------------------------------------------------------------------------

CHECK_INTERVAL = 5           # how often we query the process group status
KILL_DELAY = 5               # how long we wait between SIGTERM and SIGKILL


# -----------------------------------------------------------------------------

# constants
# -----------------------------------------------------------------------------

optimal_planning = True
cleaned_plan_file = "cleaned_result.result"
no_cleaned_plan_file = "no_cleaned_result.result"


# -----------------------------------------------------------------------------

# funcs
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# set_limit
#
# sets 'amount' as the maximum allowed capacity of the resource 'kind'
# -----------------------------------------------------------------------------
def set_limit(kind, amount):
    """
    sets 'amount' as the maximum allowed capacity of the resource 'kind'
    """

    try:
        resource.setrlimit(kind, (amount, amount))
    except OSError, e:
        print ("c %s in 'set_limit'" % e)


# -----------------------------------------------------------------------------
# kill_pgrp
#
# sends the signal sig to the process group pgrp
# -----------------------------------------------------------------------------
def kill_pgrp(pgrp, sig):
    """
    sends the signal sig to the process group pgrp
    """

    try:
        os.killpg(pgrp, sig)
    except OSError:
        pass


# -----------------------------------------------------------------------------
# run
#
# Time is measured in seconds and memory in bytes
#
# -----------------------------------------------------------------------------
def run (script, domain, problem, plan_sol, timeout, memory):
    global counter
    global best_cost

    # create a timer
    runtimer = timetools.Timer ()

    # Now, a child is created which will host the planner execution while this
    # process simply monitors the resource comsumption. If any is exceeded the
    # whole process group is killed
    with runtimer:

        child_pid = os.fork()
        if not child_pid:                                            # child's code
            os.setpgrp()
            set_limit(resource.RLIMIT_CPU, timeout)
            set_limit(resource.RLIMIT_AS, memory)
            set_limit(resource.RLIMIT_CORE, 0)
            os.execl(script, script, domain, problem, plan_sol)

        real_time = 0
        while True:
            time.sleep(CHECK_INTERVAL)
            real_time += CHECK_INTERVAL

            # Check if the current planner has generated some plans
            if(not original_data):
                data = os.system("ls -l " + plans_folder + "/" + no_cleaned_plan_file + "* > /dev/null 2>&1")
                if(data == 0):
                    command = "python2.7 " + rootpath + "/parser/clean_plans.py " + plans_folder + " " + no_cleaned_plan_file + " " +  cleaned_plan_file + " " + original_domain + " " + original_problem + " " + str(best_cost)
                    print "Run command: " + str(command)
                    os.system(command)

            data = os.system("ls -l " + plans_folder + "/" + cleaned_plan_file + "* > /dev/null 2>&1")
            if(data == 0):
                # Move plan files to the specific folder/name provided by the IPC software
                data = os.popen("ls -l " + plans_folder + "/" + cleaned_plan_file + "*")
                for line in data.readlines():
                    if((len(line) > 0) and (line[len(line)-1] == '\n')):
                        line = line[:-1].strip()		# Quitamos el \n

                    elements = line.split()

                    if(len(elements) > 0):
                        name = elements[len(elements)-1].strip()
                        print "Name: " + str(name)

                        # Validate the current plan file to get the plan cost
                        command = rootpath + "/parser/VAL-4.2.08/validate -v " + original_domain + "  " + original_problem + " " + name
                        val_data = os.popen(command)
                        successful_plan = False
                        current_cost = -1

                        for val_line in val_data.readlines():
                            val_line = val_line.strip()
                            if((len(val_line) > 0) and (val_line[len(val_line)-1] == '\n')):
                                val_line = val_line[:-1]		# Quitamos el \n

                            if(val_line.find("Successful plans:") >= 0):
                                successful_plan = True

                            elif(val_line.find("Value:") >= 0):
                                cost_elements = val_line.split()
                                if(len(cost_elements) == 2):
                                    current_cost = int(cost_elements[1].strip())
                                else:
                                    print "ERROR! Wrong cost line: " + str(val_line)

                        if((not successful_plan) or (current_cost == -1)):
                            print("ERROR: Plan " + str(name) + " is not valid or the plan cost is equal to -1, therefore we remove it")
                            os.system("rm -f " + name)

                        elif((counter == 1) or (current_cost < best_cost)):
                            best_cost = current_cost
                            print "New best plan cost found: " + str(best_cost)
                            command = "mv " + name + " " + original_plan_file
                            print "Run command: " + str(command)
                            os.system(command)
                            counter += 1

                        else:
                            print("ERROR: El plan " + str(name) + " is worse (" + str(current_cost) + ") than the previous plan generated (" + str(best_cost) + "), therefore we remove it")
                            os.system("rm -f " + name)


            group = systools.ProcessGroup(child_pid)

            # Generate the children information before the waitpid call to avoid a
            # race condition. This way, we know that the child_pid is a descendant.
            if os.waitpid(child_pid, os.WNOHANG) != (0, 0):
                break

            # get the total time and memory usage
            process_time = real_time
            total_time = group.total_time()

            # if multicore ain't enabled, the usual rules apply
            try_term = (total_time >= timeout or
                        real_time >= 1.5 * timeout)
            try_kill = (total_time >= timeout + KILL_DELAY or
                        real_time >= 1.5 * timeout + KILL_DELAY)

            term_attempted = False
            if try_term and not term_attempted:
                print ("c aborting children with SIGTERM...")
                print ("c children found: %s" % group.pids())
                kill_pgrp(child_pid, signal.SIGTERM)
                term_attempted = True
            elif term_attempted and try_kill:
                print ("c aborting children with SIGKILL...")
                print ("c children found: %s" % group.pids())
                kill_pgrp(child_pid, signal.SIGKILL)

        # Even if we got here, there may be orphaned children or something we may
        # have missed due to a race condition. Check for that and kill.
        group = systools.ProcessGroup(child_pid)
        if group:
            # If we have reason to suspect someone still lives, first try to kill
            # them nicely and wait a bit.
            print ("c aborting orphaned children with SIGTERM...")
            print ("c children found: %s" % group.pids())
            kill_pgrp(child_pid, signal.SIGTERM)
            time.sleep(1)

        # Either way, kill properly for good measure. Note that it's not clear if
        # checking the ProcessGroup for emptiness is reliable, because reading the
        # process table may not be atomic, so for this last blow, we don't do an
        # emptiness test.
        kill_pgrp(child_pid, signal.SIGKILL)

    return real_time


# -----------------------------------------------------------------------------
# run_portfolio
#
# Run each planner with its allotted time 
#
# -----------------------------------------------------------------------------
def run_portfolio (planners, timeouts, memory):

    accumulated_time = 0

    print "\nOriginal_Domain: " + str(original_domain)
    print "Original_Problem: " + str(original_problem)
    print "Modified_Domain: " + str(modified_domain)
    print "Modified_Problem: " + str(modified_problem)
    print "Original_Plan_file: " + str(original_plan_file)
    print "Plans folder: " + str(plans_folder) + "\n"


    for i in xrange(0, len(planners)):
        # Configuring planner path
        planner = rootpath + "/" + planners[i] + "/plan"
        timeout = timeouts[i]

        print "\n\n****************************************************"
        print "*** Planner_path: " + planner + " TimeOut: " + str(timeout) + " ***"
        print "****************************************************\n\n"

        if(original_data):
            result = plans_folder + "/" + cleaned_plan_file
            executed_time = run (planner, original_domain, original_problem, result, timeout, memory)
            print "Planner " + planner + " run " + str(executed_time) + " seconds\n"
            accumulated_time += executed_time

        else:
            result = plans_folder + "/" + no_cleaned_plan_file
            executed_time = run (planner, modified_domain, modified_problem, result, timeout, memory)
            print "Planner " + planner + " run " + str(executed_time) + " seconds\n"
            accumulated_time += executed_time


        # If we are in optimal planning and the optimal solution was found, we finish the execution
        if((counter > 1) and (optimal_planning)):
            i = len(planners)

    return accumulated_time

def run_portfolio_end (planners, timeouts, memory):

    accumulated_time = 0

    print "\nOriginal_Domain: " + str(original_domain)
    print "Original_Problem: " + str(original_problem)
    print "Modified_Domain: " + str(modified_domain)
    print "Modified_Problem: " + str(modified_problem)
    print "Original_Plan_file: " + str(original_plan_file)
    print "Plans folder: " + str(plans_folder) + "\n"
    for i in xrange(0, len(planners)):
        # Configuring planner path
        planner = rootpath + "/" + planners[i] + "/plan"
        timeout = timeouts[i]

        print "\n\n****************************************************"
        print "*** Planner_path: " + planner + " TimeOut: " + str(timeout) + " ***"
        print "****************************************************\n\n"
        result = plans_folder + "/" + cleaned_plan_file
        executed_time = run (planner, original_domain, original_problem, result, timeout, memory)
        print "Planner " + planner + " run " + str(executed_time) + " seconds\n"
        accumulated_time += executed_time
        # If we are in optimal planning and the optimal solution was found, we finish the execution
        if((counter > 1) and (optimal_planning)):
            i = len(planners)

    return accumulated_time
    
def readFile(data, name):
	fd = open(name,'r')
	data2 = []
	data2 = fd.readlines()
	fd.close()
	lista_nueva = []
	for i in data2:
	        lista_nueva.append(i[:-1])
	for i in lista_nueva:
	    if i not in data:
	        data.append(i)
	return data
	
	

# main
# -----------------------------------------------------------------------------
if __name__ == '__main__':
    planners = []
    timeouts = []
    default_planners = ["jasper", "mercury", "bfs-f","siw", "fdss-2" , "probe", "yahsp2-mt"]
    ##"arvand", "fd-autotune-1", "lama-2008",  "lama-2011","fd-autotune-2", "fdss-2"]
    default_timeouts = [15, 30, 45, 45, 45, 45, 45]
    ##memory   = 4241280205 # 3,95 GB
    memory   = 7965671424  
    mem = virtual_memory()
    original_data = True
    timelimit = 300
    best_cost = -1
    counter = 1
    ##cores = multiprocessing.cpu_count()
    print "MEMORY TOTAL: ", mem.total
    if (mem.total < memory):
        memory = int(mem.total*0.9)
        timelimit = 280
        default_planners = ["jasper", "mercury", "bfs-f","siw", "fdss-2" , "probe", "yahsp2-mt", "arvand"]
        current_time  = (timelimit * 1) / len(default_planners)
        timeouts = [current_time, current_time, current_time, current_time, current_time, current_time, current_time, current_time]
        test = 1
        print "\n***** The memory limit is: ", str(memory)
        print "\n***** The time limit is: ", str(timelimit)
        print "\n***** The cores is: ", str(1)
        print "***************************\n"
        print "WARNING: THIS IS A TEST CONFIGURATION\n"
        print "***************************\n"
    print "***************************\n"
    
    # Check params
    if len(sys.argv) == 4:

        if os.path.isfile(sys.argv[1]):
            original_domain = os.path.abspath(sys.argv[1])

            if os.path.isfile(sys.argv[2]):
                original_problem = os.path.abspath(sys.argv[2])
                original_plan_file = sys.argv[3]

            else:
                print >> sys.stderr, "The problem file does not exist: %s" % sys.argv[2]
                sys.exit(-1)

        else:
            print >> sys.stderr, "The domain file does not exist: %s" % sys.argv[1]
            sys.exit(-1)
    else:
        raise SystemExit("Usage: %s <domain_file> <problem_file> <result_file>" % sys.argv[0])
        sys.exit(-1)

    # Getting root path
    pathname = os.path.dirname(sys.argv[0])
    currentpath = os.path.abspath(pathname)
    rootpath = os.path.abspath(os.path.join(currentpath,".."))
    rootpathOutput = os.path.abspath(os.path.dirname(sys.argv[1]))

    # Getting modified paths
    modified_domain  = rootpathOutput + "/domain_ready.txt"
    modified_problem = rootpathOutput + "/problem_ready.txt"
    plans_folder = rootpathOutput 
    print plans_folder
    print rootpath
    # Parse original domain and original problem (conditional effects)
    begin = time.time()
    command = "python2.7 " + rootpath + "/parser/parse.py " + original_domain + " " + original_problem
    print "Run command: " + str(command)
    os.system(command)
    end = time.time()
    if((os.path.isfile(modified_domain)) and (os.path.isfile(modified_problem))):
        print "\nModified domain/problem detected\n"
        original_data = False

    else:
        print "\nPortfolio runs original domain/problem\n"
    accumulated_time = end - begin
    accumulated_time = math.ceil(accumulated_time)
    print "Parser took " + str(accumulated_time) + " seconds\n"
    init_time_features = time.time()
    ##Features
    timeTranslate = 0
    timePreprocess = 0
    timeFFLearner = 0
    timeHeuristic = 0
    timeLandmark = 0
    timeMercury = 0
    try:
        if(original_data):
            print "Extract Features with original problem and domain"
            command = "ulimit -v 4194304;ulimit -t 50; python2.7 " + rootpath + "/features/translate/translate.py " + original_domain + " " + original_problem
            print "Run command: " + str(command)
            os.system(command)
            timeTranslate = time.time()
            command =  "ulimit -v 4194304;ulimit -t 50; " + rootpath + "/features/preprocess/preprocess < " + rootpathOutput + "/output.sas"
            print "Run command: " + str(command)
            os.system(command)
            timePreprocess = time.time()
            command = "ulimit -v 4194304;ulimit -t 50; " + rootpath + "/features/ff-learner/roller3.0 -o " + original_domain + " -f " + original_problem + " -S 28"
            print "Run command: " + str(command)
            os.system(command)
            timeFFLearner = time.time()
            command = "ulimit -v 4194304;ulimit -t 100; " + rootpath + "/features/heuristics/training.sh "  + original_domain + " " + original_problem
            print "Run command: " + str(command)
            os.system(command)
            timeHeuristic = time.time()
            command = "ulimit -v 4194304;ulimit -t 100; "+  rootpath +"/search/downward --landmarks \"lm=lm_merged([lm_hm(m=1),lm_rhw(),lm_zg()])\" < " + rootpathOutput + "/output"
            os.system(command)
            timeLandmark = time.time()
            print "Run command: " + str(command)            
            command = "ulimit -v 4194304;ulimit -t 100; "+  rootpath + "/search-mercury/downward ipc seq-agl-mercury <" + rootpathOutput + "/output"
            timeMercury = time.time()

            print "Run command: " + str(command)
            os.system(command)
            actual_rootpath = rootpath + "/models"
            command = "python2.7 "+ actual_rootpath + "/joinFile.py " + rootpathOutput
            print "Run command: " + str(command)
            os.system(command)
            

            
        else:
            print "Extract Features with modified problem and domain"
            command = "ulimit -v 4194304;ulimit -t 50; python2.7 " + rootpath + "/features/translate/translate.py " + modified_domain + " " + modified_problem
            print "Run command: " + str(command)
            os.system(command)
            timeTranslate = time.time()
            command =  "ulimit -v 4194304;ulimit -t 50; " + rootpath + "/features/preprocess/preprocess  cosas <  " + rootpathOutput + "/output.sas"
            print "Run command: " + str(command)
            os.system(command)
            timePreprocess = time.time()
            command = "ulimit -v 4194304;ulimit -t 50; " + rootpath + "/features/ff-learner/roller3.0 -o " + modified_domain + " -f " + modified_problem + " -S 28"
            print "Run command: " + str(command)
            os.system(command)
            timeFFLearner = time.time()
            command = "ulimit -v 4194304;ulimit -t 100; " + rootpath + "/features/heuristics/training.sh "  + modified_domain + " " + modified_problem
            print "Run command: " + str(command)
            os.system(command)
            
            timeHeuristic = time.time()
            ##Now Landmarks
            command = "ulimit -v 4194304;ulimit -t 100; "+  rootpath +"/search/downward --landmarks \"lm=lm_merged([lm_hm(m=1),lm_rhw(),lm_zg()])\" < " + rootpathOutput + "/output"
            
            print "Run command: " + str(command)
            os.system(command)
            
            timeLandmark = time.time()
            command = "ulimit -v 4194304;ulimit -t 100; "+  rootpath + "/search-mercury/downward ipc seq-agl-mercury < " + rootpathOutput + "/output"
            print "Run command: " + str(command)
            os.system(command)
            timeMercury = time.time()

            actual_rootpath = rootpath + "/models"
            command = "python2.7 "+ actual_rootpath + "/joinFile.py " + rootpathOutput
            print "Run command: " + str(command)
            os.system(command)
        command = "java -cp "+ rootpath +"/models/weka.jar -Xms256m -Xmx1024m weka.filters.unsupervised.attribute.Remove -R 1-3,18,20,65,78-79,119-120 -i "+ rootpathOutput + "/global_features.arff -o "+ rootpathOutput +"/global_features_simply.arff"
        print "Run command: " + str(command)
        os.system(command)
    except:
        print "**** Features Fail**** "       
        ##Features
    features_time = math.ceil(time.time()-init_time_features)
    accumulated_time += features_time
    print "Features " + str(features_time) + " seconds\n"
    print "Features and parser took " + str(accumulated_time) + " seconds\n"
    ##PORTFOLIO
    try:
    	model1 = time.time()
    	command = "java -Xms256m -Xmx1024m -cp "+ rootpath +"/models/weka.jar weka.classifiers.meta.RotationForest -l "+ rootpath +"/models/meta.RotationForest.model -T "+ rootpathOutput +"/global_features_simply.arff -p 113 > "+  rootpathOutput +"/outputModel"
    	print "Run command: " + str(command)
    	os.system(command)
    	print "   \n"
    	## python models/parseWekaOutputFile.py models/outputModel models/listPlanner
    	command = "python2.7 "+ rootpath +"/models/parseWekaOutputFile.py "+   rootpathOutput +"/outputModel " + rootpathOutput +"/listPlanner"
    	print "Run command: " + str(command)
    	os.system(command)
       
       
    	##End Model
    	model = math.ceil(time.time() - model1)
    	print "Model " + str(model) + " seconds\n"
    	accumulated_time += model
   	print "Main portfolio runs " + str(accumulated_time) + " seconds\n"
      
        ##Find the planners
        planners_time_start = time.time()
        route = rootpathOutput + "/listPlanner"
        planners = readFile(planners, route)
        time_total = int(math.ceil(timelimit) / len(planners))
        for i in xrange(len(planners)):
            timeouts.append(time_total)
        print "**** PRINT PLANNERS SELECTED **** "
        for p,t in zip(planners, timeouts):
            print p, t
            
        print "**** END PLANNERS SELECTED **** "
    except:
        planners = default_planners
        timeouts = default_timeouts
        print "**** PRINT PLANNERS DEFAULT**** "
        for p,t in zip(planners, timeouts):
            print p, t
        print "**** END PLANNERS DEFAULT**** "
	print "ERROR"
    planners_time_end = time.time() - planners_time_start
    print "Select the planners " + str(planners_time_end) + " seconds\n"
    accumulated_time += math.ceil(planners_time_end)
    print "Time took " + str(accumulated_time) + " seconds\n"
    accumulated_time += run_portfolio (planners, timeouts, memory)
    print "Main portfolio runs " + str(accumulated_time) + " seconds\n"
    # some planner failed, therefore there is remaining time. Run default planner
    
    if((accumulated_time < timelimit) and ((counter == 1) or ((counter > 1) and (not optimal_planning)))):
        planners2 =  ["siw", "mercury", "jasper", "bfs-f"]
        finalPlanners = planners2 ##list(set(planners2) - set(planners))
        timeFinal = []
        if(len(finalPlanners) == 0):
        	finalPlanners = ["siw"]
        	time_total = int(math.ceil(timelimit - accumulated_time))
        	timeFinal = [time_total]
        else:
        	for i in range(len(finalPlanners)):
        		time_total = int(math.ceil(timelimit - accumulated_time))
        		timeFinal.append(time_total)
        for p,t in zip(finalPlanners, timeFinal):
            print p, t
        accumulated_time += run_portfolio_end (finalPlanners, timeFinal, memory)
        print "Main portfolio plus default planner run " + str(accumulated_time) + " seconds (in total)\n"

        # It is very rare.. It is possible that all planners failed: memory or there is a problem with the original problem/domain. We run blind planner with original_data
        if((accumulated_time < timelimit) and ((counter == 1) or ((counter > 1) and (not optimal_planning)))):
            planners = ["lama-2011",  "arvand","lamar"]
            time_end = int((timelimit - accumulated_time))
            timeouts = [time_end, time_end, time_end]
            original_data = True
            accumulated_time += run_portfolio (planners, timeouts, memory)
    print "Main portfolio runs " + str(accumulated_time) + " seconds\n"                            
    print "-------------------- END --------------------"
    """print "---------------------TIME FEATURES------------------------------------\n"
    print "timeTranslate: ", math.ceil(timeTranslate-init_time_features), round(timeTranslate-init_time_features,4)
    print "timePreprocess: ", math.ceil(timePreprocess-timeTranslate), round(timePreprocess-timeTranslate,4)
    print "timeFFLearner: ", math.ceil(timeFFLearner-timePreprocess), round(timeFFLearner-timePreprocess,4)
    print "timeHeuristic: ", math.ceil(timeHeuristic-timeFFLearner), round(timeHeuristic-timeFFLearner,4)
    print "timeLandmark: ", math.ceil(timeLandmark-timeHeuristic), round(timeLandmark-timeHeuristic,4)
    print "timeMercury: ", math.ceil(timeMercury-timeLandmark), round(timeMercury-timeLandmark,4)
    print "Rest time: ", math.ceil(time.time()-timeMercury), round(time.time()-timeMercury,4)
    print "---------------------TIME FEATURES------------------------------------\n"
    """
 
