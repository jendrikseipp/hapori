import copy
import options
import pddl
import re
import sys
from collections import OrderedDict
from . import single_valued_checker
from . import relation_mapping
from . import operator_splitter
from . import mapping_printer
from . import helper_functions
from . import bagged_operator


def bag_operators(baggable_types_list, task, type_checker_list):
    
    
    # Convert operators into bagged format
    split_operators_into_subtypes(task, baggable_types_list, type_checker_list)
    variables_to_ground_mapping_list = transform_operators(task, baggable_types_list, type_checker_list)
    task.solution_mapper.variables_to_ground_mapping_list = variables_to_ground_mapping_list
    
    # Split operators into cases of equality between parameters that could be grounded to the same object
    equality_mapper = split_operators_into_equality_classes(task, baggable_types_list)
    task.solution_mapper.equality_mapper = equality_mapper
    
    # Add GTE system to operators
    add_GTE_system(task, variables_to_ground_mapping_list, baggable_types_list)
    
    
    

def transform_operators(task, baggable_types_list, type_checker_list):
    
    
    variables_to_ground_mapping_list_all = []
    baggable_ops = []
    for action in task.actions:
        
        #print("ACTION:", action.name)
        action_precond = tuple([action.precondition]) if not len(action.precondition.parts) else action.precondition.parts
        
        baggable_op = bagged_operator.BaggableOperator(action, task)
        
        action_names = [x.name for x in task.actions]
        goal_macropredicate_infomation = []
        for baggable_type in baggable_types_list:
        
            objects_of_type_in_action = [x for x in action.parameters if x.type_name == baggable_type.object_type.name]
            
            # Do we need to keep the old version of this action? eg. grasp action in barman needs one for shot and one for shaker
            for obj in objects_of_type_in_action:
                
                
                # Add preconditions/effects only for the complete macropredicate (not the partial ones yet)
                baggable_param = bagged_operator.BaggableParameter(obj, baggable_type, baggable_op, action, task)
                param_needs_macropredicate = baggable_param.initialise_macropredicate_for_parameter()
                if not param_needs_macropredicate:
                    continue
                baggable_param.add_macropredicates_for_parameter()
                baggable_op.baggable_parameters.append(baggable_param)
              
 
        
        # Create goal macropredicates
        bagged_ops_to_add, variables_to_ground_mapping_list_new = add_goal_macropredicates(baggable_op, task, action_names)
        
        # If this action needs to be reformulated then add to the list of operators to add
        baggable_ops.extend(bagged_ops_to_add)
        variables_to_ground_mapping_list_all.extend(variables_to_ground_mapping_list_new)
    
    
    # Remove the original space operators and replace them with the reformulated ones
    for reform_action in baggable_ops:
        task.actions = [x for x in task.actions if x.name != reform_action.original_action_name]
        task.actions.append(reform_action.convert_to_pddl_action())
    
    return variables_to_ground_mapping_list_all
        
  



    
def add_goal_macropredicates(baggable_op, task, action_names):
    
    #print('\n\nAction being modified', baggable_op.name)
    
    all_new_actions = [baggable_op]
    variables_to_ground_mapping_list_all = []
    
    
    # For each baggable parameter in the action
    for baggable_param in baggable_op.baggable_parameters:
        #print("\n\nThis param", baggable_param.parameter)
    
        # For each newly created grounding of the action
        new_actions = []
        for action in all_new_actions:
            #print("\tAdding goal macropredicates to action", action.name)
            
            # Create the goal macropredicates
            action_baggable_param = [x for x in action.baggable_parameters if x.parameter == baggable_param.parameter][0]
            new_actions_for_split, variables_to_ground_mapping_list = add_goal_macropredicates_for_parameter(action, action_baggable_param, task, action_names)
            #print("\tNew actions generated:", [x.name for x in new_actions_for_split])
            new_actions.extend(new_actions_for_split)
            variables_to_ground_mapping_list_all.extend(variables_to_ground_mapping_list)
            
        all_new_actions = new_actions
        #print("Current actions:", [x.name for x in all_new_actions])
        
     
    return all_new_actions, variables_to_ground_mapping_list_all
   
 
def add_goal_macropredicates_for_parameter(baggable_op, baggable_param, task, action_names):


    #print("Action:", baggable_op.name)

    all_new_actions = []
    variables_to_ground_mapping_list_all = []
    variables_to_ground_mapping_list_bag_groundings = []

    # If there is more than 1 type of goal macropredicate then we ground each goal-macropredicate-containing operator to the bag 
    ground_ops_to_bag = len(baggable_param.baggable_type.goal_macropredicates) > 1
    needs_splitting = False # Do we definitely require this operator to be split into different groundings. We don't need to if all splits are equivalent. Won't know until later
    
    
    # For each goal-macropredicate of this type
    original_actions_to_keep = []
    new_actions_for_this_goal_macropredicate = []
    for goal_macropredicate in [baggable_param.baggable_type.macropredicate] + baggable_param.baggable_type.goal_macropredicates:
        
        
        
        # If there is a GTE system then the goal is described using desired-count predicates in the initial state
        # If not then the goal is described in the goal state
        relevant_goal_macropredicates = [x for x in task.goal.parts if type(x) is pddl.conditions.Atom and x.predicate == goal_macropredicate.name] if (not goal_macropredicate.needs_GTE) else [x for x in task.init if type(x) is pddl.conditions.Atom and x.predicate == goal_macropredicate.desired_count_goal_macropredicate_name and not x.args[-1] == [x for x in goal_macropredicate.counts if x.number == 0][0].number_object.name]
        #print("This macropredicate:", goal_macropredicate.name, relevant_goal_macropredicates)
        
        
        # Find variables in this macropredicate which need to be grounded in order to generate goal macropredicates
        variables_to_ground_mapping_list = baggable_param.get_variables_to_ground_for_goal_macropredicates(relevant_goal_macropredicates, goal_macropredicate)
        
        
        # Generate the goal-macropredicate preconditions and effects by grounding these variables
        goal_preconditions, count_mapping, params_to_add = baggable_param.add_precondition_for_goal_macopredicate(goal_macropredicate, variables_to_ground_mapping_list)
        if not len(goal_preconditions):
        
            # If no goal-macropredicate preconditions/effects are required then we may still need to ground the operator to each bag with this macropredicate in the goal to make the operator applicable
            if ground_ops_to_bag:
                #print("Non goal macropredicate, grounding bags", [x.bagname for x in baggable_param.baggable_type.bags if x.goal_macropredicate is not None and x.goal_macropredicate.name == goal_macropredicate.name], "for", baggable_op.name)
                all_new_actions.extend(baggable_op.ground_to_bags(baggable_param.parameter, [x.bagname for x in baggable_param.baggable_type.bags if x.goal_macropredicate is not None and x.goal_macropredicate.name == goal_macropredicate.name], variables_to_ground_mapping_list_bag_groundings, action_names))
            continue
            
            
        needs_splitting = True  
        goal_effects = baggable_param.add_effects_for_goal_macopredicate(goal_macropredicate, count_mapping, variables_to_ground_mapping_list)
        param_to_ground = variables_to_ground_mapping_list[0].variable
        variables_to_ground_mapping_list_all = variables_to_ground_mapping_list_all + variables_to_ground_mapping_list
        
        
        print("Yes", baggable_op.name, "does need goal macropredicates", goal_preconditions, "Effects", goal_effects)
        
        
        # For each parameter in this action we are to ground in the goal-macropredicates
        new_actions_for_this_parameter = []
        ungrounded_op = copy.deepcopy(baggable_op)
        maps_with_this_param = [x for x in variables_to_ground_mapping_list if x.variable.name == param_to_ground.name]
        
        #print("Parameter of interest is", param_to_ground.name)
        
        
        
        # Add the preconditions and effects now if the target is not a variable
        if str(param_to_ground.name)[0] != "?":
            #print("Not variable")
            new_actions_for_this_parameter = [] if ground_ops_to_bag else [baggable_op]
            if params_to_add[0].name in [x.name for x in baggable_op.parameters]:
                break
            baggable_op.parameters = baggable_op.parameters + params_to_add
            parts = list(baggable_op.precondition.parts)
            parts = parts + copy.deepcopy(goal_preconditions)
            baggable_op.precondition.parts = tuple(parts)
            baggable_op.effects = baggable_op.effects + [pddl.effects.Effect([], pddl.conditions.Truth(), copy.deepcopy(x)) for x in goal_effects if not x is None]
            
            if ground_ops_to_bag:
                #print("Constant objects, grounding bags", [x.bagname for x in baggable_param.baggable_type.bags if x.goal_macropredicate is not None and x.goal_macropredicate.name == goal_macropredicate.name], "for", baggable_op.name)
                new_actions_for_this_parameter.extend(baggable_op.ground_to_bags(baggable_param.parameter, [x.bagname for x in baggable_param.baggable_type.bags if x.goal_macropredicate is not None and x.goal_macropredicate.name == goal_macropredicate.name], variables_to_ground_mapping_list_bag_groundings, action_names))
        
        
        
        # Otherwise modify the original action by grounding the attribute parameter
        #print("Current action split:", baggable_op.name)
    
        # If one action is grounded for every object of that type, remove ungrounded action
        else:
            keep_ungrounded_action = True
            neq_preconds = []
            if len(variables_to_ground_mapping_list) == maps_with_this_param[0].num_objects or baggable_op.name in [x.name for x in original_actions_to_keep]:
                keep_ungrounded_action = False
    
    
            # For each object in the goal state this parameter can be grounded to
            for variable_to_ground_map in maps_with_this_param:
        
                #print("Variable to ground:", variable_to_ground_map.ground_to)
            
                # If there is a neq precondition between this variable and this grounding, then skip
                neq_precond = [x for x in baggable_op.precondition.parts if type(x) is pddl.conditions.NegatedAtom and x.predicate == "=" and variable_to_ground_map.ground_to in x.args and param_to_ground.name in x.args]
                if len(neq_precond):
                    #print("neq", neq_precond)
                    continue
    
                # Create grounded action
                new_action = copy.deepcopy(baggable_op)
            
                #print("Grounded to", new_action.name, new_action.parameters , ';', variable_to_ground_map.ground_to, "\n\n\n")
            
            
                # Add preconditions, effects and parameters
                new_action.parameters = new_action.parameters + params_to_add
                if not len(new_action.precondition.parts) and len(goal_preconditions):
                    new_action.precondition.parts = [new_action.precondition] + copy.deepcopy(goal_preconditions)
                else:
                    new_action.precondition = pddl.conditions.Conjunction(list(new_action.precondition.parts) + copy.deepcopy(goal_preconditions))
                new_action.effects = new_action.effects + [pddl.effects.Effect([], pddl.conditions.Truth(), copy.deepcopy(x)) for x in goal_effects if not x is None]
                new_actions_for_this_parameter.append(new_action)
            
                # Ground the action
                new_action.ground_atoms_for_goal(variable_to_ground_map, action_names)
            
               # print("GROUNDED", new_action.name)
            
                # Add neq preconditions to ungrounded action (if there is one)
                if keep_ungrounded_action:
                    neq_preconds.append(pddl.NegatedAtom('=', [variable_to_ground_map.variable.name, variable_to_ground_map.ground_to]))
               
            
            # Create ungrounded action if appropriate   
            if keep_ungrounded_action:   
                if not len(baggable_op.precondition.parts) and len(neq_preconds):
                    ungrounded_op.precondition.parts = [ungrounded_op.precondition] + neq_preconds
                else:
                    ungrounded_op.precondition = pddl.conditions.Conjunction(list(ungrounded_op.precondition.parts) + neq_preconds)
                new_actions_for_this_parameter.append(ungrounded_op)
        
        
        # Ground bag if necessary
        new_ops = [] if ground_ops_to_bag else new_actions_for_this_parameter
        for split_action in [x for x in new_actions_for_this_parameter]:
            
            # Ground the operator to each bag
            if ground_ops_to_bag:
               # print("Variable objects, grounding bags", [x.bagname for x in baggable_param.baggable_type.bags if x.goal_macropredicate is not None and x.goal_macropredicate.name == goal_macropredicate.name], "for", split_action.name)
                new_ops.extend(split_action.ground_to_bags(baggable_param.parameter, [x.bagname for x in baggable_param.baggable_type.bags if x.goal_macropredicate is not None and x.goal_macropredicate.name == goal_macropredicate.name], variables_to_ground_mapping_list_bag_groundings, action_names))
        new_actions_for_this_parameter = new_ops
        
        # Update list of new actions
        new_actions_for_this_goal_macropredicate.extend(new_actions_for_this_parameter)
    
    if needs_splitting:
        #print("Adding new actions", [x.name for x in new_actions_for_this_goal_macropredicate])
        all_new_actions.extend(new_actions_for_this_goal_macropredicate)     
    
    all_new_actions = all_new_actions if len(all_new_actions) else [baggable_op]
    
    
    
    return all_new_actions, variables_to_ground_mapping_list_all + variables_to_ground_mapping_list_bag_groundings
    

    
    
# ASSUMPTION: !=(constant, constant) happens sometimes  
# ASSUMPTION: goal may be satisfied if one of the goal attributes is satisfied but not the rest. Need to ground the ?dc in some operators (or something like that)
# Maybe ground remaining desired counts to zero in initial state
def add_GTE_system(task, variables_to_ground_mapping_list, baggable_types_list):
    
    
    if options.writeout_reformulation_logic:
        print('Adding GTE system to types:', ', '.join([x.macropredicate.typ.name for x in baggable_types_list if x.macropredicate.needs_GTE]))
    
    new_actions = []
    for action in task.actions:
        
        #print("ACTION:", action.name)
        
        split_actions = [action]
        action_has_been_split = False


		# For each baggable type
        for baggable_type in baggable_types_list:
            #print("i=", i)

            objects_of_type_in_action = [x for x in action.parameters if x.type_name == baggable_type.object_type.name] + get_grounded_bags_from_operator(action, task, baggable_type.object_type.name)
            
            # Find which macropredicate style (if any) is used and generate its GTE actions
            GTE_actions = None
                
            # For each style of macropredicate of this type (full or partial)
            for macropredicate_style in [baggable_type.macropredicate] + baggable_type.goal_macropredicates:
                #print("macropredicate_style", macropredicate_style.name)
                
                # Find each precondition macropredicate (of this type) in this action that could be grounded with a desired count macropredicate (whose count > 0)
                preconds_indices_to_split_over = get_GTE_action_preconditions_to_factor_over(task, action, macropredicate_style)
                for index in preconds_indices_to_split_over:
                    new_splits = []
                    for split_action in split_actions:
                        #print("Current split action", split_action, "from pool of", [x.name for x in split_actions])
                         
                        GTE_actions = macropredicate_style.add_gte_macropredicates_to_action(split_action, index, task, [x.name for x in new_actions])
                        if not GTE_actions is None:
                            #print("GTE actions added:", [x.name for x in GTE_actions])
                            new_splits = new_splits + GTE_actions
                            action_has_been_split = True
                        #else:
                            #print("NOT NEEDED")
            
            
                #if not GTE_actions is None:
                   # print("GTE actions added:", [x.name for x in GTE_actions])
                   # new_splits = new_splits + GTE_actions
                    #action_has_been_split = True
        
            
                    if len(new_splits):
                        #print("New splits are",  new_splits)   
                        split_actions = new_splits
            
        
        if action_has_been_split:
            #print("action has been split", split_actions)
            new_actions = new_actions + split_actions
        else:
            #print("action has NOT been split", split_actions)
            new_actions.append(action)
    
    task.actions = new_actions        
                    

                    
def get_GTE_action_preconditions_to_factor_over(task, action, macropredicate):    
    

    # We need to split GTE operators over this factor
    action_macropred_preconds = [x for x in action.precondition.parts if x.predicate is macropredicate.name]
    nonzero_desired_inits = [x for x in task.init if type(x) is pddl.conditions.Atom and x.predicate is macropredicate.desired_count_goal_macropredicate_name and not x.args[-1] == [x.number_object.name for x in macropredicate.counts if x.number == 0][0]]
    preconds_indices_to_split_over = []
    for j in range(0, len(action_macropred_preconds)):
    
        #print("Precond = ", action_macropred_preconds[j])
        for des in nonzero_desired_inits:
        
            #print("Desired = ", des)
        
            can_ground_to = True
            
            # The desired predicate can be grounded to the action's macropredicate iff all precond arguments belong to the respective des arg supertype or objects are equal
            for arg_index in range(1, len(action_macropred_preconds[j].args)-1):
            

                j_arg = action_macropred_preconds[j].args[arg_index]
                des_arg = des.args[arg_index]
                #print("J arg = ", j_arg, " Des arg = ", des_arg)
                j_type = [x.type_name for x in action.parameters if x.name == j_arg]
                
                if not len(j_type): # If the precond is already grounded then see if des arg equals it
                    
                    if des_arg != j_arg:
                        can_ground_to = False
                        #print("1 THIS ACTION DOES NOT NEED ANY GTE FOR", action_macropred_preconds[j], " WRT ", des)
                        break
                
                else: # If precond arg is not grounded then see if its arg type belongs to des args supertype
                
                    
                    des_type = [x.type_name for x in task.objects if x.name == des_arg][0]
                    des_supertype = helper_functions.get_supertypes([x for x in task.types if x.name == des_type][0], task)
                    #print("Des args supertype: ", des_supertype, " j arg type ", j_type[0])
                    if not j_type[0] in des_supertype:
                        can_ground_to = False
                        #print("2 THIS ACTION DOES NOT NEED ANY GTE FOR", action_macropred_preconds[j], " WRT ", des)
                        break
            
            
            if can_ground_to:
                #print("XXX Precondition ", action_macropred_preconds[j], " need to be split over. Adding index ", j)
                preconds_indices_to_split_over.append(j)
                break
                
    #print("For action ", action.name, " returning ", preconds_indices_to_split_over, "\n")
    return preconds_indices_to_split_over
                    
                    
                                                                                
                    
def get_grounded_bags_from_operator(action, task, bag_type):

    bags_to_return = []
    bag_objects = [x for x in task.objects if x.type_name == bag_type]
    for prec in action.precondition.parts:
        for bag_object in bag_objects:
            if bag_object.name in prec.args:
                bags_to_return.append(bag_object)
    
    return list(OrderedDict.fromkeys(bags_to_return))
                            
                                                
                                                                    
                                                                                                            
def split_operators_into_equality_classes(task, baggable_types_list):

    equality_mapper = []
    for action in [x for x in task.actions if x.precondition.parts is not None]:
    
        action_splitter = operator_splitter.splitter(action, task)
        
        # 1) Build equivalence pairs of parameters
        equivalence_pairs = action_splitter.build_parameter_equivalence_pairs(baggable_types_list)
                                                           
                
        # 2) Split over each equivalence class
        split_actions = [action]
        task.actions = [x for x in task.actions if not x is action]
        action_names = [x.name for x in task.actions]
        for pair in equivalence_pairs:
        
            #print("\nPair", pair)
        
            
            new_splits = []
            for split_action in split_actions:
            
            
                # Confirm that the splits are sitll needed for this pair (one of the previous pairs may have affected it)
                need_split = pair.validate_pair(split_action)
                if not need_split:
                    new_splits.append(split_action)
                    continue
                
                # We have one operator where the two macropreds have the same attributes (we increment/decrement the count by 2).
                #print("Creating eq operator")
                new_splits.append(pair.create_eq_operator(copy.deepcopy(split_action), action_names))
            
                # We may also have one operator where the two macropreds have different attributes (increment/decrement by 1)
               # print("Creating neq operator")
                new_splits.extend(pair.create_neq_operator(copy.deepcopy(split_action), action_names))
                
            
            split_actions = new_splits
        
        
        # Tidy the operators
        for split_action in split_actions:
            
            if len(split_action.precondition.parts):
            
            
                # Remove duplicate preconditions
                split_action.precondition.parts = list(OrderedDict.fromkeys(split_action.precondition.parts))
                
                
                # Remove neq (=) preconds which apply to bags
                for bag_variable in list(OrderedDict.fromkeys([x.args[0] for x in split_action.precondition.parts if x.predicate in [y.macropredicate.name for y in baggable_types_list]])):
                    neq_precond = [x for x in split_action.precondition.parts if type(x) is pddl.conditions.NegatedAtom and x.predicate is "=" and bag_variable in x.args and bag_variable in x.args]
                    split_action.precondition.parts = [x for x in split_action.precondition.parts if not x in neq_precond]
            
            
                # Replace all new not equals (=%) preconds with the normal not equals (=) 
                new_neq = [x for x in split_action.precondition.parts if type(x) is pddl.conditions.NegatedAtom and x.predicate == "=%"]
                for atom in new_neq:
                    atom.predicate = "="
                
                
                # Remove all new equals preconds and replace all instances of the second argument with the first one
                new_equals = [x for x in split_action.precondition.parts if type(x) is pddl.conditions.Atom and x.predicate == "=%"]
                split_action.precondition.parts = [x for x in split_action.precondition.parts if not x in new_equals]
                for equal in new_equals:
                    operator_splitter.splitter.replace_argument_with(equal.args[1], equal.args[0], split_action.precondition.parts)
                    operator_splitter.splitter.replace_argument_with(equal.args[1], equal.args[0], [x.literal for x in split_action.effects if type(x) is not pddl.f_expression.Assign])
                    
            
            
            
                # Remove redundant baggable parameter names from the parameter list
                for macropred in [x.macropredicate for x in baggable_types_list]:
                    for param_name in [x.name for x in split_action.parameters if x.type_name == macropred.typ.name]:
                    
                                        
                        # If parameter not used then remove it. If it has been merged with another parameter (through equality) then we must record this for solution transformation
                        if not param_name in [y.args[0] for y in split_action.precondition.parts if y.predicate == macropred.name] and not param_name in [y.literal.args[0] for y in split_action.effects if type(y) is not pddl.f_expression.Assign and y.literal.predicate == macropred.name]:
                            split_action.parameters = [x for x in split_action.parameters if not x.name == param_name]
                            equality_args_with_param = [x.args for x in split_action.precondition.parts if type(x) is pddl.conditions.Atom and x.predicate == "=%" and param_name in x.args]
                            if len(equality_args_with_param):
                                equal_to = [z for z in equality_args_with_param[0] if z != param_name]
                                if len(equal_to):
                                    equality_mapper.append(mapping_printer.BagEqualityMapper(split_action.name, equal_to[0], param_name))
            
                
                
                    
               
            task.actions.append(split_action)
            
    return equality_mapper
        
        

          
            
# If an operator parameter is a supertype of a baggable type, then split the operator so that there is one operator for each child subtype of the original parameter type         
def split_operators_into_subtypes(task, baggable_types_list, type_checker_list):
    
    # Split each operator based off parameter types
    new_task_actions = copy.deepcopy(task.actions)
    baggable_types = [x.object_type for x in baggable_types_list]
    for action in task.actions:
    
        actions_to_check = [action]
        actions_to_add = [action]
        param_index = 0
        for action_to_check in actions_to_check:
            new_task_actions = [x for x in new_task_actions if not x.name == action_to_check.name]
            
            for p in range(param_index, len(action_to_check.parameters)):
                
                param = action_to_check.parameters[p]
                new_actions = split_tree(task, baggable_types, type_checker_list, param.type_name, action_to_check, p, [], action_to_check.name, new_task_actions)
                
                # If there is still only 1 action, move onto next parameter
                if len(new_actions) < 2:
                    continue

                # Otherwise we repeat this process for all the other parameters
                actions_to_add = [x for x in actions_to_add if x.name != action_to_check.name] + new_actions
                for a in new_actions:
                    actions_to_check.append(a)
                
                param_index = p
                break
            
        
        new_task_actions = new_task_actions + actions_to_add    
    task.actions = new_task_actions
            
                    

# Keep splitting until this type is not in some baggable objects supertype list
def split_tree(task, baggable_types, type_checker_list, current_type, action, p, accumulative_names, original_name, list_of_actions):
    
    
    children = [x.name for x in task.types if x.basetype_name == current_type]
    if len(children) and len([x for x in baggable_types if current_type in type_checker_list.get([y for y in task.types if y.name == x.name][0]).supertypes]):
        to_return = []
        for child in children:
            child_action = copy.deepcopy(action)
            child_action.name = helper_functions.create_unique_name(original_name + '#' + child, [x.name for x in list_of_actions if not x is None] + accumulative_names)
            child_action.parameters[p].type_name = child
            accumulative_names = accumulative_names + [child_action.name]
            if options.writeout_reformulation_logic:
                print('Adding new action', re.sub("[#]", "-", child_action.name), 'for', original_name) 
            to_return = to_return + split_tree(task, baggable_types, type_checker_list, child, child_action, p, accumulative_names, original_name, list_of_actions)
        
        return to_return
    
    return [action]
        

    