from . import object_bagger
from . import predicate_bagger
from . import task_modifier
from . import operator_bagger
from . import mapping_printer
from . import operator_grounding
from . import count_mutex_printer
from . import from_to_count
from . import helper_functions
import invariant_finder
import copy
import options
import time

def transform(task, reachable_action_params):
    new_task = copy.deepcopy(task)
    new_task.solution_mapper = mapping_printer.SolutionMappings(new_task)
    invariants = sorted(invariant_finder.find_invariants(task, reachable_action_params))
    
    if options.writeout_reformulation_logic:
        print('-----------------------OBJECT MERGING-----------------------------------') 
    baggable_types_list, type_checker_list = object_bagger.bag_objects(new_task, invariants)
    
    
    if not len(baggable_types_list):
        if options.writeout_reformulation_logic:
            print('There are no objects to bag in this problem. Have a good day!') 
            print('------------------------------------------------------------------------')
        return task

    
    if options.writeout_reformulation_logic:
        print('--------------------------PREDICATE MERGING-----------------------------')
    predicate_bagger.bag_predicates(baggable_types_list, new_task, invariants, type_checker_list)
    
    if options.writeout_reformulation_logic:
        print('------------------------REFORMULATING OPERATORS-------------------------')
    operator_bagger.bag_operators(baggable_types_list, new_task, type_checker_list)
    
    
    
    if options.add_mutexes:
        count_mutex_printer.add_count_mutexes(baggable_types_list)
    
    
    if options.ground_operators:
        if options.writeout_reformulation_logic:
            print('Pregrounding reformulated operators')
        operator_grounding.ground_operators(new_task, baggable_types_list)
        
    
    mapping_printer.print_mappings(baggable_types_list, new_task, task)
    
    
    if options.writeout_reformulation_logic:
        print('------------------------------------------------------------------------')
    
    return new_task
    
    