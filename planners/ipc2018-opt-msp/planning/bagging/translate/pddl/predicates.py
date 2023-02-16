from . import pddl_types

class Predicate(object):
    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments

    def __str__(self):
        return "%s(%s)" % (self.name, ", ".join(map(str, self.arguments)))

    def get_arity(self):
        return len(self.arguments)

    def pddl_str(self):
        return '(%s %s)' % (self.name, ' '.join([arg.pddl_str() for arg in self.arguments]))
