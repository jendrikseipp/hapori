import sys
import os
sys.path.append(os.path.dirname(__file__) + '/PyBliss-0.50beta/')
sys.path.append(os.path.dirname(__file__) + '/PyBliss-0.50beta/lib/python')
import PyBliss
import networkx as nx

import pddl

def report(perm, text=None):
    SymFinder.autoGens.append(perm)
    #print text, perm

class SymFinder:
    # TODO(speckd): We need to fix this hack :^) => no real need to be static
    autoGens = []

    def __init__(self):
        self.cur_action = None
        self.cur_graph = None
        self.color_to_entity = {}
        self.entity_to_color = {}
        self.color_counter = 0

    def get_color(self, entity_name):
        if entity_name not in self.entity_to_color:
            self.entity_to_color[entity_name] = self.color_counter
            self.color_to_entity[self.color_counter] = entity_name
        self.color_counter += 1
        return self.entity_to_color[entity_name]

    def find_symmetries(self, action):
        self.cur_action = action
        #action.dump()
        self.init_graph()
        self.encode_parameters(action.parameters)
        self.encode_preconditions(action.precondition)
        self.encode_effect(action.effects)
        self.encode_cost(action.cost)
        SymFinder.autoGens = []
        self.cur_graph.find_automorphisms(report, "Aut gen:")
        # TODO: if a generator element consists of multiple not connected components we
        # remove all. We probably can prune more but this should be sound and optimality preserving
        
        relations = self.filter_relevation_params(action, SymFinder.autoGens)
        # Remove empty components s.t.
        relations = [r for r in relations if r != []]
        if len(relations) == 0:
            return []

        relations = self.filter_unconnected_components(relations)
        #print(SymFinder.autoGens)
        return self.transitive_closure(relations)

    def init_graph(self):
        self.cur_graph = PyBliss.Graph()
        self.cur_graph.add_vertex("pre", self.get_color("pre"))
        self.cur_graph.add_vertex("eff", self.get_color("eff"))
        self.cur_graph.add_edge("pre", "eff")

    def encode_parameters(self, parameters):
        #print(parameters)
        for par in parameters:
            color = self.get_color(par.type)
            self.cur_graph.add_vertex(par.name, color)

    def encode_preconditions(self, precondition):
        # Iterate over all pres
        for p in precondition.parts:
            # add predicate
            name = str(p)
            # TODO(speckd): is it possible that predicates have same name but are definied over different types?
            # This could cause coloring problems
            color = self.get_color(p.predicate)
            self.cur_graph.add_vertex(name, color)
            # add true
            name_true = str(p) + "_true"
            color_true = self.get_color(str(p.predicate) + "_true")
            self.cur_graph.add_vertex(name_true, color_true)
            self.cur_graph.add_edge(name, name_true)
            # add false
            name_false = str(p) + "_false"
            color_false = self.get_color(str(p.predicate) + "_false")
            self.cur_graph.add_vertex(name_false, color_false)
            self.cur_graph.add_edge(name, name_false)

            # connect pres with (neg) atom
            if p.negated:
                self.cur_graph.add_edge("pre", name_false)
            else:
                self.cur_graph.add_edge("pre", name_true)

            # Create input arguments
            for i in range(len(p.args)):
                name_arg = str(p) + "_arg_" + str(i)
                if str(p.predicate) == "=":
                    color_arg = self.get_color("arg_0")
                else:
                    color_arg = self.get_color("arg_" + str(i))
                self.cur_graph.add_vertex(name_arg, color_arg)
                self.cur_graph.add_edge(name, name_arg)

                # We need to check if the arg is a constant and give it a unique color
                if p.args[i][0] != "?":
                    self.cur_graph.add_vertex(p.args[i], self.get_color(p.args[i]))
                self.cur_graph.add_edge(name_arg, p.args[i])


    def encode_effect(self, effect):
        for eff in effect:
            e = eff.literal
            name = str(e)
            color = self.get_color(e.predicate)
            self.cur_graph.add_vertex(name, color)

            # add true
            name_true = str(e) + "_true"
            color_true = self.get_color(str(e.predicate) + "_true")
            self.cur_graph.add_vertex(name_true, color_true)
            self.cur_graph.add_edge(name, name_true)
            # add false
            name_false = str(e) + "_false"
            color_false = self.get_color(str(e.predicate) + "_false")
            self.cur_graph.add_vertex(name_false, color_false)
            self.cur_graph.add_edge(name, name_false)

            # connect pres with (neg) atom
            if e.negated:
                self.cur_graph.add_edge("eff", name_false)
            else:
                self.cur_graph.add_edge("eff", name_true)

            # Create input arguments
            for i in range(len(e.args)):
                name_arg = str(e) + "_arg_" + str(i)
                if str(e.predicate) == "=":
                    color_arg = self.get_color("arg_0")
                else:
                    color_arg = self.get_color("arg_" + str(i))
                self.cur_graph.add_vertex(name_arg, color_arg)
                self.cur_graph.add_edge(name, name_arg)
                # We need to check if the arg is a constant and give it a unique color
                if e.args[i][0] != "?":
                    self.cur_graph.add_vertex(e.args[i], self.get_color(e.args[i]))
                self.cur_graph.add_edge(name_arg, e.args[i])

    def encode_cost(self, cost):
        if cost is None:
            return
        if type(cost.expression) is pddl.NumericConstant:
            return
        # is a function
        #print(type(cost.expression))
        #print(cost.expression.symbol)
        #print(cost.expression.args)
        name = str(cost.expression.symbol) + "_cost"
        # TODO(speckd): is it possible that predicates have same name but are definied over different types?
        # This could cause coloring problems
        color = self.get_color(name)
        self.cur_graph.add_vertex(name, color)
        self.cur_graph.add_edge("eff", name)
        for i in range(len(cost.expression.args)):
            name_arg = str(name) + "_arg_" + str(i)
            color_arg = self.get_color("arg_" + str(i))
            self.cur_graph.add_vertex(name_arg, color_arg)
            self.cur_graph.add_edge(name, name_arg)
            if cost.expression.args[i][0] != "?":
                self.cur_graph.add_vertex(cost.expression.args[i], self.get_color(cost.expression.args[i]))
            self.cur_graph.add_edge(name_arg, cost.expression.args[i])

    def filter_relevation_params(self, action, autoGens):
        rels = []
        par_names = [p.name for p in action.parameters]
        for a in autoGens:
            cur_rel = []
            for p in par_names:
                if p != a[p]:
                    cur_rel.append((p, a[p]))
            rels.append(cur_rel)
        #print(rels)
        return rels

    def filter_unconnected_components(self, relations):
        new_relations = []
        for r in relations:
            G = nx.Graph()
            for edge in r:
                G.add_edge(edge[0], edge[1])
            largest_comp = sorted(nx.connected_components(G), key=len, reverse=True)[0]
            filterd_rel = []
            for edge in r:
                if edge[0] in largest_comp:
                    filterd_rel.append(edge)
            new_relations.append(filterd_rel)
        #print(new_relations)
        return new_relations


    def dump_graph(self):
        if self.cur_graph is not None:
            self.cur_graph.write_dot(sys.stdout)

    def transitive_closure(self, relations):
        closure = set([j for i in relations for j in i])
        while True:
            new_relations = set((x, w) for x, y in closure for q, w in closure if q == y)
            closure_until_now = closure | new_relations
            if closure_until_now == closure:
                break
            closure = closure_until_now
        tr_closure = list(closure)
        return sorted([x for x in tr_closure if x[0] < x[1]])

if __name__ == "__main__":
    SymFinder()
    pass


