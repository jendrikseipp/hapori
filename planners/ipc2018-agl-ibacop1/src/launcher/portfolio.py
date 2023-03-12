   
    ##PORTFOLIO
    try:
    	print "ISA PRUEBA"
    	command ="ls"
    	os.system(command)
    	print "ISA PRUEBA"
    	model1 = time.time()
    	command = "java -Xms256m -Xmx1024m -cp "+ rootpath +"/models/weka.jar weka.classifiers.meta.RotationForest -l "+ rootpath +"/models/meta.RotationForest.model -T global_features_simply.arff -p 113 > "+  rootpath +"/models/outputModel"
    	print "Run command: " + str(command)
    	os.system(command)
    	print "   \n"
    	## python models/parseWekaOutputFile.py models/outputModel models/listPlanner
    	command = "python2.7 "+ rootpath +"/models/parseWekaOutputFile.py "+ rootpath +"/models/outputModel "+ rootpath +"/models/listPlanner"
    	print "Run command: " + str(command)
    	os.system(command)
       
       
    	##End Model
    	model = math.ceil(time.time() - model1)
    	print "Model " + str(model) + " seconds\n"
    	accumulated_time += model
   	print "Main portfolio runs " + str(accumulated_time) + " seconds\n"
      
        ##Find the planners
        planners_time_start = time.time()
        route = os.path.abspath(sys.argv[1])
        ##route = route[:route.rfind("/")] + "/src/models/listPlanner"
        route = rootpath + "/models/listPlanner"
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
    print "Main portfolio runs " + str(accumulated_time/3600) + " seconds\n"
    # some planner failed, therefore there is remaining time. Run default planner
    
    if((accumulated_time < timelimit) and ((counter == 1) or ((counter > 1) and (not optimal_planning)))):
        planners2 = ["yahsp2-mt", "madagascar", "fd-autotune-1",  "lpg", "arvand", "lama-2008","lama-2011","lamar","probe", "fd-autotune-2","fdss-2"]
        finalPlanners = list(set(planners2) - set(planners))
        timeFinal = []
        if(len(finalPlanners)==0):
        	finalPlanners = ["yahsp2-mt", "madagascar", "fd-autotune-1",  "lpg", "arvand", "lama-2008","lama-2011","lamar","probe", "fd-autotune-2","fdss-2"]
        	timeFinal = [5, 45, 50, 50, 55, 70, 90, 90, 95, 110, 140]
        else:
        	for i in range(len(finalPlanners)):
        		time_total = int(math.ceil(timelimit - accumulated_time)/ len(finalPlanners))
        		timeFinal.append(time_total)
        for p,t in zip(finalPlanners, timeFinal):
            print p, t
        accumulated_time += run_portfolio (finalPlanners, timeFinal, memory)
        print "Main portfolio plus default planner run " + str(accumulated_time) + " seconds (in total)\n"

        # It is very rare.. It is possible that all planners failed: memory or there is a problem with the original problem/domain. We run blind planner with original_data
        if((accumulated_time < timelimit) and ((counter == 1) or ((counter > 1) and (not optimal_planning)))):
            planners = ["blind"]
            timeouts = [(timelimit - accumulated_time)]
            original_data = True
            accumulated_time += run_portfolio (planners, timeouts, memory)
            print "Main portfolio plus default planner plus blind planner run " + str(accumulated_time) + " seconds (in total)\n"
    planners2 = ["yahsp2-mt", "madagascar", "fd-autotune-1",  "lpg", "arvand", "lama-2008","lama-2011","lamar","probe", "fd-autotune-2","fdss-2"]
    timeouts2 =  [5, 45, 50, 50, 55, 70, 90, 90, 95, 110, 140]
    accumulated_time += run_portfolio (planners2, timeouts2, memory)
	
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
