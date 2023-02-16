import copy
import options
import pddl
from . import single_valued_checker
from . import relation_mapping
from . import helper_functions
from collections import OrderedDict



def bag_predicates(baggable_types_list, task, invariants, type_checker_list):
    
   
    create_macropredicates(baggable_types_list, task, invariants, type_checker_list) # Generate macropredicates
    desired_count_grounded_atoms = modify_goal_state(baggable_types_list, task, invariants, type_checker_list) # Ground macropredicates in goal
    modify_initial_state(baggable_types_list, task, invariants, type_checker_list, desired_count_grounded_atoms) # Ground macropredicates in initial state
    
    if options.writeout_reformulation_logic:
        print_types_and_objects(task)
        


def modify_initial_state(baggable_types_list, task, invariants, type_checker_list, desired_count_grounded_atoms):
    
    if options.writeout_reformulation_logic:
        print("Grounding macropredicates in initial state...")
    
    new_init = []
    init_bags = [[] for x in baggable_types_list]
    
        
        
    # Find all init atoms which baggable objects appear in, and add all other init atoms into the new init
    for init in [x for x in task.init if type(x) is pddl.conditions.Atom and not x.predicate == '=']:
        to_remove = False
        for b in range(0, len(baggable_types_list)):
            baggable_type = baggable_types_list[b]
            for bag in baggable_type.bags:
                if len([x for x in bag.objects if x.name in init.args]):
                    to_remove = True
                    init_bags[b].append(init)
        if not to_remove:
            new_init.append(init)
    new_init.extend([x for x in task.init if type(x) is pddl.f_expression.Assign])    
    
    
    
    # Build list of all macropredicates which need to be added to the initial state and ground them. We may also need to ground the satisfied macropredicate if a GTE system is required
    macropredicates_to_add = []
    for b in range(0, len(baggable_types_list)):
        baggable_type = baggable_types_list[b]
        init_bag = init_bags[b]
        for macropredicate in [baggable_type.macropredicate] + baggable_type.goal_macropredicates:
            for bag in baggable_type.bags:
                group_macropredicates = []
                for obj in bag.objects:
                    new_macro = macropredicate.ground_macropredicate(obj, init_bag)
                    matching_macro = [x for x in group_macropredicates if x.args[:-1] == new_macro.args[:-1]]
                    if len(matching_macro):
                        macropredicate.increment(matching_macro[0])
                        
                        # If number of instances has reached the goal requirements, then set the satisfied macropredicate to true
                        desired_count_obj = [x.args[-1] for x in desired_count_grounded_atoms if x.predicate == macropredicate.desired_count_goal_macropredicate_name and x.args[:-1] == matching_macro[0].args[:-1]]
                        if len(desired_count_obj):

                            desired_count_num = [x for x in macropredicate.counts if x.number_object.name == desired_count_obj[0]][0]
                            actual_count_num = [x for x in macropredicate.counts if x.number_object.name == matching_macro[0].args[-1]][0]
                            #print("There are", actual_count_num.number, "instances of", matching_macro[0], "and we need", desired_count_num.number)
                            if(desired_count_num.number == actual_count_num.number):
                                group_macropredicates.append(pddl.conditions.Atom(macropredicate.satisfied_goal_macropredicate_name, copy.deepcopy(matching_macro[0].args[:-1])))
                    else: 
                        group_macropredicates.append(new_macro)
                        
                        # If number of instances has reached the goal requirements, then set the satisfied macropredicate to true
                        desired_count_obj = [x.args[-1] for x in desired_count_grounded_atoms if x.predicate == macropredicate.desired_count_goal_macropredicate_name and x.args[:-1] == new_macro.args[:-1]]
                        if len(desired_count_obj):

                            desired_count_num = [x for x in macropredicate.counts if x.number_object.name == desired_count_obj[0]][0]
                            actual_count_num = [x for x in macropredicate.counts if x.number_object.name == new_macro.args[-1]][0]
                            #print("There are", actual_count_num.number, "instances of", new_macro, "and we need", desired_count_num.number)
                            if(desired_count_num.number == actual_count_num.number):
                                group_macropredicates.append(pddl.conditions.Atom(macropredicate.satisfied_goal_macropredicate_name, copy.deepcopy(new_macro.args[:-1])))
                        
                macropredicates_to_add = macropredicates_to_add + group_macropredicates
        
        
        
    
    # Ground the first argument (bag id) of each initial state macropredicate
    for baggable_type in baggable_types_list:
        for obj in baggable_type.all_unbagged_objects:
            helper_functions.ground_atoms(macropredicates_to_add + desired_count_grounded_atoms, obj.name, baggable_type.get_bag_name_of_object(obj))
          
                      
    if options.writeout_reformulation_logic:
        print_grounded_macropredicates(macropredicates_to_add, 'initial')
        
    task.init = new_init + macropredicates_to_add
    for desired_count_grounded_atom in desired_count_grounded_atoms:
        task.init.append(desired_count_grounded_atom)
    
    
    # Finally add the zero counts to the initial state
    add_zero_counts(task, baggable_types_list)	


     
def add_zero_counts(task, baggable_types_list):
    
    
    new_init = []
    for baggable_type in baggable_types_list:
        for macropredicate in [baggable_type.macropredicate] + baggable_type.goal_macropredicates:
            for bag_obj in baggable_type.all_bagged_objects:
                init_atoms = list(OrderedDict.fromkeys(macropredicate.ground_all_at_zero(bag_obj)))
                init_macros_for_this_bag = [x for x in task.init if type(x) is pddl.conditions.Atom and x.predicate == macropredicate.name and x.args[0] == bag_obj.name]
                init_desired_preds_for_this_bag = [x for x in task.init if type(x) is pddl.conditions.Atom and len(x.args) and x.args[0] == bag_obj.name and x.predicate == macropredicate.desired_count_goal_macropredicate_name]
                zero_count = [x for x in macropredicate.counts if x.number == 0][0].number_object.name
                
                print("\n\nAdding zeroes for", macropredicate.name, bag_obj)
                print("Already in init:", init_macros_for_this_bag)
                
                for init in init_atoms:
                    
                    # We don't want to include the zero-count if it is not a zero count in the initial state
                    already_in = len([x for x in init_macros_for_this_bag if tuple(x.args[:-1]) == init.args[:-1]])
                    if not already_in:
                        new_init.append(init)
                    else:
                        print("Will not add", init, "because it is already there")
                        
                        
                        
                    # Add zero count to desired predicate if there is not already a non zero
                    matching_desired_pred = [x for x in init_desired_preds_for_this_bag if tuple(x.args[:-1]) == init.args[:-1]]
                    if not len(matching_desired_pred) and macropredicate.desired_count_goal_macropredicate_name is not None:
                        new_init.append(pddl.conditions.Atom(macropredicate.desired_count_goal_macropredicate_name, list(init.args[:-1]) + [zero_count]))
                    
    
    # Add to initial state
    task.init = task.init + new_init
    

def modify_goal_state(baggable_types_list, task, invariants, type_checker_list):
    if options.writeout_reformulation_logic:
        print("Grounding macropredicates in goal state...")
    
    new_goal = []
    goal_bags = [[] for x in baggable_types_list]
    desired_count_grounded_atoms = []
    
    
    
    # Find all goal atoms which baggable objects appear in, and add all other goal atoms into the new goal
    for goal in [x for x in task.goal.parts if type(x) is pddl.conditions.Atom]:
        to_remove = False
        for b in range(0, len(baggable_types_list)):
            baggable_type = baggable_types_list[b]
            for bag in baggable_type.bags:
                if len([x for x in bag.objects if x.name in goal.args]):
                    to_remove = True
                    goal_bags[b].append(goal)
        if not to_remove:
            new_goal.append(goal)
    new_goal.extend([x for x in task.goal.parts if type(x) is pddl.f_expression.Assign])
        
    
    
    # Build list of all macropredicates which need to be added to the goal and ground them. These depend on whether or not a GTE system is required for each type
    ungrounded_macropredicates_to_add = []
    ungrounded_desired_macropredicates = []
    grounded_macropredicates_to_add = []
    for b in range(0, len(baggable_types_list)):
        baggable_type = baggable_types_list[b]
        ungrounded_macropredicates_to_add.append(baggable_type.macropredicate)
        for bag in baggable_type.bags:
            
            grounded_macropredicates_to_add_temp = []
            for obj in bag.objects:
            
                #print("obj", obj)

                sub_macropredicate, satisfied_predicate, desired_predicate = baggable_type.macropredicate.create_goal_macropredicates(goal_bags[b], obj)
                if not sub_macropredicate:
                    continue
                    
                #print('good', sub_macropredicate.name, satisfied_predicate.name, desired_predicate.name)
                
                if satisfied_predicate is False: # GTE system not required
                    add_goal_macropredicate(task, obj, goal_bags[b], baggable_type, ungrounded_macropredicates_to_add, sub_macropredicate, grounded_macropredicates_to_add_temp, bag)
                else:							 # GTE system required
                    add_satisfied_macropredicate(task, obj, goal_bags[b], baggable_type, ungrounded_macropredicates_to_add, sub_macropredicate, satisfied_predicate, desired_predicate, ungrounded_desired_macropredicates, grounded_macropredicates_to_add_temp, desired_count_grounded_atoms, bag)
            
            grounded_macropredicates_to_add = grounded_macropredicates_to_add + grounded_macropredicates_to_add_temp 

    
    
    # Ground the first argument (the bag id) in each of these goal macropredicates
    for baggable_type in baggable_types_list:
        for obj in baggable_type.all_unbagged_objects:
            helper_functions.ground_atoms(grounded_macropredicates_to_add, obj.name, baggable_type.get_bag_name_of_object(obj))
                
                
    
    if options.writeout_reformulation_logic:
        print_grounded_macropredicates(grounded_macropredicates_to_add, 'goal')
    
    # Add the new goal macropredicates to the goal and return the list of 'desired-count' atoms which will later be added to the initial state (only if a GTE system is needed)
    task.goal.parts = new_goal + grounded_macropredicates_to_add
    return desired_count_grounded_atoms



# Add partial macropredicates if no GTE system is required for this baggable type
def add_goal_macropredicate(task, obj, goal_bag, baggable_type, ungrounded_macropredicates_to_add, sub_macropredicate, grounded_macropredicates, bag):


    # See if new macropredicate already exists
    existing_macro = [x for x in ungrounded_macropredicates_to_add if x.args == sub_macropredicate.args]
    if len(existing_macro):
        sub_macropredicate = existing_macro[0]
    else: 
        print("bag", bag, "requires goal macropredicate", sub_macropredicate.name)
        ungrounded_macropredicates_to_add.append(sub_macropredicate)
        pred_to_add = pddl.predicates.Predicate(sub_macropredicate.name, sub_macropredicate.args)
        task.predicates.append(pred_to_add)
        baggable_type.goal_macropredicates.append(sub_macropredicate)
        bag.goal_macropredicate = sub_macropredicate
        if options.writeout_reformulation_logic:
            print("Goal macropredicate added for", sub_macropredicate.typ, "is", pred_to_add)
        
    
    new_grounded_pred = sub_macropredicate.ground_macropredicate(obj, goal_bag)
    matching_grounded_macro = [x for x in grounded_macropredicates if x.args[:-1] == new_grounded_pred.args[:-1]]
    if len(matching_grounded_macro):
        print("Incrementing goal macropredicate", new_grounded_pred)
        sub_macropredicate.increment(matching_grounded_macro[0])
    else: 
        print("Does not need incrementing", new_grounded_pred)
        grounded_macropredicates.append(new_grounded_pred)
                    


# Add satisfied macropredicates if GTE system is required for this baggable type
def add_satisfied_macropredicate(task, obj, goal_bag, baggable_type, ungrounded_macropredicates_to_add, sub_macropredicate, satisfied_predicate, desired_predicate, ungrounded_desired_macropredicates, grounded_macropredicates, desired_count_grounded_atoms, bag):
    
    # See if new macropredicate already exists
    existing_macro = [x for x in ungrounded_macropredicates_to_add if x.args == sub_macropredicate.args]
    if len(existing_macro):
        sub_macropredicate = existing_macro[0]
        if existing_macro[0].desired_count_goal_macropredicate_name is None:
            existing_macro[0].desired_count_goal_macropredicate_name = desired_predicate.name
            existing_macro[0].satisfied_goal_macropredicate_name = satisfied_predicate.name
            
            
    else: 
        print("bag", bag, "requires goal macropredicate", sub_macropredicate.name)
        ungrounded_macropredicates_to_add.append(sub_macropredicate)
        pred_to_add = pddl.predicates.Predicate(sub_macropredicate.name, sub_macropredicate.args)
        task.predicates.append(pred_to_add) 
        baggable_type.goal_macropredicates.append(sub_macropredicate)
        bag.goal_macropredicate = sub_macropredicate
        sub_macropredicate.desired_count_goal_macropredicate_name = desired_predicate.name
        sub_macropredicate.satisfied_goal_macropredicate_name = satisfied_predicate.name
        
        if options.writeout_reformulation_logic:
            print("Macropredicate added for", sub_macropredicate.typ, "is", pred_to_add)
    
    
    # See if satisfied / desired already exist
    existing_desired = [x for x in ungrounded_desired_macropredicates if x.arguments == desired_predicate.arguments]
    if not len(existing_desired):       
       task.predicates.append(satisfied_predicate)
       task.predicates.append(desired_predicate)   
       ungrounded_desired_macropredicates.append(desired_predicate)       
                
    
    new_grounded_pred = sub_macropredicate.ground_macropredicate(obj, goal_bag, init = False)
    
    # Ground them in goal
    matching_grounded_macro = [x for x in grounded_macropredicates if x.args == new_grounded_pred[0].args and x.predicate == new_grounded_pred[0].predicate]
    if len(matching_grounded_macro):
        sub_macropredicate.increment([x for x in desired_count_grounded_atoms if x.predicate ==  sub_macropredicate.desired_count_goal_macropredicate_name and x.args[:-1] == new_grounded_pred[1].args[:-1]][0])
    else: 
        grounded_macropredicates.append(new_grounded_pred[0])
        
        # Add grounded desired count predicate to initial state
        desired_count_grounded_atoms.append(new_grounded_pred[1])
        


def create_macropredicates(baggable_types_list, task, invariants, type_checker_list):
    
    if options.writeout_reformulation_logic:
        print("Generating macropredicates...")
    
    new_predicates = []
    
    # Get rid of all '=' predicates (we don't need them in Baggy) and get rid of all predicates which have an argument of a baggable type
    types_to_bag = [x.object_type for x in baggable_types_list]
    new_predicates = [x for x in task.predicates if not x.name == '=' and not len([y for y in types_to_bag if y.name in [z.type_name for z in x.arguments]])]
    
    # For each baggable type
    for baggable_type in baggable_types_list:
        invariants_of_type_b = baggable_type.type_checker.invariants_of_this_type
        macropredicate = relation_mapping.Macropredicate(task, baggable_type.object_type, baggable_type.type_checker.supertypes, type_checker_list)
        macropredicate.baggable_type = baggable_type
        
        # Invariants
        add_none_objects_to_invariants(task, invariants_of_type_b)
        for invariant_group in invariants_of_type_b:
            print("Invariant group of type", baggable_type.object_type.name, invariant_group)
            macropredicate.add_invariant_argument(invariant_group)
        
        # Final argument is the counter
        less_than_predicate = macropredicate.add_count()
        new_predicates.append(less_than_predicate)
        
        
        # See if macropredicate needs GTE system
        macropredicate.see_if_type_needs_GTE_system()
        
        
        # Create macropredicate
        macropred = pddl.predicates.Predicate(macropredicate.name, macropredicate.args)
        new_predicates.append(macropred)
        baggable_type.macropredicate = macropredicate
        for bag in baggable_type.bags:
            bag.goal_macropredicate = macropredicate
        
        if options.writeout_reformulation_logic:
            print("Macropredicate added for", baggable_type.object_type.name, "is", macropred, end = "\n")
    
    task.predicates = list(OrderedDict.fromkeys(new_predicates))




        
        
# Ensure that if the number of instances for an invariant group can decrease, then there is a 'none' object. 
def add_none_objects_to_invariants(task, invariant_groups):
    for invar in invariant_groups:
        for part in invar.parts:
            pred = [x for x in task.predicates if x.name == part.predicate][0]
            
            # For each action where this predicate is removed, is it the case that one or more of the predicates in this invariant are added?
            for action in task.actions:
                add_effects = [eff.literal for eff in action.effects if not eff.literal.negated]
                del_effects = [eff.literal for eff in action.effects if eff.literal.negated and eff.literal.predicate == pred.name]
                some_part_is_added = False
                
                for removed in del_effects:
                
                    for part_sub in invar.parts:

                        pred_sub = [x for x in task.predicates if x.name == part_sub.predicate][0]
                        if pred_sub.name in [x.predicate for x in add_effects if x.predicate == pred_sub.name and removed.args[part.order[0]] == x.args[part_sub.order[0]]]:
                            some_part_is_added = True
                            break
                
                    if not some_part_is_added:
                        if options.writeout_reformulation_logic:
                            print("Adding none-object for", pred, "because there exists an action that removes this invariant without adding anything back.")
                        invar.nein = True
                        break
                



    


def print_types_and_objects(task):
    for typ in task.types:
        print("Type", typ.name, "has the following objects: ", end = "")
        print(', '.join([x.name for x in task.objects if x.type_name == typ.name]))


def print_grounded_macropredicates(preds, terminus):
    for pred in preds:
        print("Grounded macropredicate added to", terminus, "state:", pred)
    print()
