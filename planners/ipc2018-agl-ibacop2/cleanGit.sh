cd src
cd arvand
git rm release-search -f
git rm  Makefile.depend profile-search gmon.out PROFILE core -f 
git rm -f sas_plan -f
cd ..
cd features/ff-learner
git rm -f *.o *.bak *~ *% core *_pure_p9_c0_400.o.warnings
cd ..
cd heuristics/preprocess
git rm -f *~ *.pyc
git rm -f Makefile.depend gmon.out PROFILE core
git rm -f output preprocess
cd ..
cd heuristics/translate
git rm -f *~ *.pyc
cd ..
cd heuristics/search 
git rm -f *~ *.pyc
git rm -f Makefile.depend gmon.out PROFILE core
git rm -f sas_plan
cd ..
cd ..
cd preprocess
git rm -f *~ *.pyc
git rm -f Makefile.depend gmon.out PROFILE core
git rm -f output preprocess
cd ..
cd translate
git rm -f *~ *.pyc
cd ..
cd lama-2008
git rm -f *~ *.pyc
git	rm -f Makefile.depend profile-search gmon.out PROFILE core
git	rm -f sas_plan
git rm release-search search
cd ..
cd lamar/search
git rm -f *~ *.pyc
git	rm -f Makefile.depend profile-search gmon.out PROFILE core
git	rm -f sas_plan
git rm release-search search
cd ..
cd ..
cd launcher
git rm -f *~ *.pyc
cd ..
cd models
git rm -f prueba.arff
git rm -f listPlanner *.pyc 
git rm -f outputModel
cd ..
cd parser
git rm -f *.pyc 
cd adl2strips
git rm -f *.o *.bak *~ *% core *_pure_p9_c0_400.o.warnings
git rm ff
cd ..
cd ..
cd preprocess
git rm -f *~ *.pyc
git rm -f Makefile.depend gmon.out PROFILE core
git rm -f output preprocess
cd ..
cd translate
git rm -f *~ *.pyc
cd ..
cd probe
cd src/parser-ff
cd ../..
git rm -f *.stats
git rm -f *.list
git rm -f *.dot
git rm -f *.png
cd ..
cd search 
git rm -f .obj
git rm -f *~ *.pyc
git rm -f Makefile.depend gmon.out PROFILE core
git rm -f sas_plan
git rm downward-1 downward-2 downward-4
cd ..
cd search-mercury
git rm -f .obj
git rm -f *~ *.pyc
git rm -f Makefile.depend gmon.out PROFILE core
git rm -f sas_plan
git rm downward-1 downward-2 downward-4
cd ..
cd translate
git rm -f *~ *.pyc
cd ..
cd translate-old
git rm -f *~ *.pyc
cd ..
git rm -f elapsed.time
git rm -f exec.stats
git rm -f features.arff
git rm -f featuresGraph
git rm -f featuresSimply.arff
git rm -f global_features.arff
git rm -f global_features_simply.arff
git rm -f grounding.stats
git rm -f heuristic
git rm -f heuristics.sas
git rm -f initfeature-info.txt
git rm -f output
git rm -f output-old
git rm -f output-old.sas
git rm -f output.sas
git rm -f plan_numbers_and_cost
git rm -f salida.*
git rm -f test.groups
git rm -f tmp_results
git rm -f translateFile
git rm -f translateFileSimply
git rm -f all.groups
git rm -f *~
git rm -f red-black
git rm -f landmark-graph
git rm -f landmark.arff
git rm -f *.txt
git rm -f down*
git rm -f src/translate/axiom_rules.pyc
git rm -f src/translate/build_model.pyc
git rm -f src/translate/constraints.pyc
git rm -f src/translate/fact_groups.pyc
git rm -f src/translate/graph.pyc
git rm -f src/translate/greedy_join.pyc
git rm -f src/translate/instantiate.pyc
git rm -f src/translate/invariant_finder.pyc
git rm -f src/translate/invariants.pyc
git rm -f src/translate/normalize.pyc
git rm -f src/translate/pddl_to_prolog.pyc
git rm -f src/translate/sas_tasks.pyc
git rm -f src/translate/simplify.pyc
git rm -f src/translate/split_rules.pyc
git rm -f src/translate/timers.pyc
git rm -f src/translate/tools.pyc
 
 
git add Singularity
git add build
git add cleanOutput
git add src/launcher/solve.py
git add src/parser/clean_action_costs.py
git add src/parser/clean_plans.py
git add src/parser/parse.py
git add src/parser/restore_action_costs.py
git add src/translate/translate.py

echo "Delete all temporal files - clean"


