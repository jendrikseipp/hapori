(define (domain slitherlink)
(:requirements
    :strips
    :typing
    :negative-preconditions
    :equality
    :conditional-effects
)
(:types
    node - object
    cell - object
    cell-capacity-level - object
)
(:constants
    cap-0 - cell-capacity-level
    cap-1 - cell-capacity-level
    cap-2 - cell-capacity-level
    cap-3 - cell-capacity-level
    cap-4 - cell-capacity-level
    n-0-0 - node
    n-0-1 - node
    n-0-2 - node
    n-0-3 - node
    n-0-4 - node
    n-0-5 - node
    n-0-6 - node
    n-0-7 - node
    n-0-8 - node
    n-0-9 - node
    n-0-10 - node
    n-0-11 - node
    n-0-12 - node
    n-1-0 - node
    n-1-1 - node
    n-1-2 - node
    n-1-3 - node
    n-1-4 - node
    n-1-5 - node
    n-1-6 - node
    n-1-7 - node
    n-1-8 - node
    n-1-9 - node
    n-1-10 - node
    n-1-11 - node
    n-1-12 - node
    n-2-0 - node
    n-2-1 - node
    n-2-2 - node
    n-2-3 - node
    n-2-4 - node
    n-2-5 - node
    n-2-6 - node
    n-2-7 - node
    n-2-8 - node
    n-2-9 - node
    n-2-10 - node
    n-2-11 - node
    n-2-12 - node
    n-3-0 - node
    n-3-1 - node
    n-3-2 - node
    n-3-3 - node
    n-3-4 - node
    n-3-5 - node
    n-3-6 - node
    n-3-7 - node
    n-3-8 - node
    n-3-9 - node
    n-3-10 - node
    n-3-11 - node
    n-3-12 - node
    n-4-0 - node
    n-4-1 - node
    n-4-2 - node
    n-4-3 - node
    n-4-4 - node
    n-4-5 - node
    n-4-6 - node
    n-4-7 - node
    n-4-8 - node
    n-4-9 - node
    n-4-10 - node
    n-4-11 - node
    n-4-12 - node
    n-5-0 - node
    n-5-1 - node
    n-5-2 - node
    n-5-3 - node
    n-5-4 - node
    n-5-5 - node
    n-5-6 - node
    n-5-7 - node
    n-5-8 - node
    n-5-9 - node
    n-5-10 - node
    n-5-11 - node
    n-5-12 - node
    n-6-0 - node
    n-6-1 - node
    n-6-2 - node
    n-6-3 - node
    n-6-4 - node
    n-6-5 - node
    n-6-6 - node
    n-6-7 - node
    n-6-8 - node
    n-6-9 - node
    n-6-10 - node
    n-6-11 - node
    n-6-12 - node
    n-7-0 - node
    n-7-1 - node
    n-7-2 - node
    n-7-3 - node
    n-7-4 - node
    n-7-5 - node
    n-7-6 - node
    n-7-7 - node
    n-7-8 - node
    n-7-9 - node
    n-7-10 - node
    n-7-11 - node
    n-7-12 - node
    n-8-0 - node
    n-8-1 - node
    n-8-2 - node
    n-8-3 - node
    n-8-4 - node
    n-8-5 - node
    n-8-6 - node
    n-8-7 - node
    n-8-8 - node
    n-8-9 - node
    n-8-10 - node
    n-8-11 - node
    n-8-12 - node
    n-9-0 - node
    n-9-1 - node
    n-9-2 - node
    n-9-3 - node
    n-9-4 - node
    n-9-5 - node
    n-9-6 - node
    n-9-7 - node
    n-9-8 - node
    n-9-9 - node
    n-9-10 - node
    n-9-11 - node
    n-9-12 - node
    cell-0-0 - cell
    cell-0-1 - cell
    cell-0-2 - cell
    cell-0-3 - cell
    cell-0-4 - cell
    cell-0-5 - cell
    cell-0-6 - cell
    cell-0-7 - cell
    cell-0-8 - cell
    cell-0-9 - cell
    cell-0-10 - cell
    cell-0-11 - cell
    cell-1-0 - cell
    cell-1-1 - cell
    cell-1-2 - cell
    cell-1-3 - cell
    cell-1-4 - cell
    cell-1-5 - cell
    cell-1-6 - cell
    cell-1-7 - cell
    cell-1-8 - cell
    cell-1-9 - cell
    cell-1-10 - cell
    cell-1-11 - cell
    cell-2-0 - cell
    cell-2-1 - cell
    cell-2-2 - cell
    cell-2-3 - cell
    cell-2-4 - cell
    cell-2-5 - cell
    cell-2-6 - cell
    cell-2-7 - cell
    cell-2-8 - cell
    cell-2-9 - cell
    cell-2-10 - cell
    cell-2-11 - cell
    cell-3-0 - cell
    cell-3-1 - cell
    cell-3-2 - cell
    cell-3-3 - cell
    cell-3-4 - cell
    cell-3-5 - cell
    cell-3-6 - cell
    cell-3-7 - cell
    cell-3-8 - cell
    cell-3-9 - cell
    cell-3-10 - cell
    cell-3-11 - cell
    cell-4-0 - cell
    cell-4-1 - cell
    cell-4-2 - cell
    cell-4-3 - cell
    cell-4-4 - cell
    cell-4-5 - cell
    cell-4-6 - cell
    cell-4-7 - cell
    cell-4-8 - cell
    cell-4-9 - cell
    cell-4-10 - cell
    cell-4-11 - cell
    cell-5-0 - cell
    cell-5-1 - cell
    cell-5-2 - cell
    cell-5-3 - cell
    cell-5-4 - cell
    cell-5-5 - cell
    cell-5-6 - cell
    cell-5-7 - cell
    cell-5-8 - cell
    cell-5-9 - cell
    cell-5-10 - cell
    cell-5-11 - cell
    cell-6-0 - cell
    cell-6-1 - cell
    cell-6-2 - cell
    cell-6-3 - cell
    cell-6-4 - cell
    cell-6-5 - cell
    cell-6-6 - cell
    cell-6-7 - cell
    cell-6-8 - cell
    cell-6-9 - cell
    cell-6-10 - cell
    cell-6-11 - cell
    cell-7-0 - cell
    cell-7-1 - cell
    cell-7-2 - cell
    cell-7-3 - cell
    cell-7-4 - cell
    cell-7-5 - cell
    cell-7-6 - cell
    cell-7-7 - cell
    cell-7-8 - cell
    cell-7-9 - cell
    cell-7-10 - cell
    cell-7-11 - cell
    cell-8-0 - cell
    cell-8-1 - cell
    cell-8-2 - cell
    cell-8-3 - cell
    cell-8-4 - cell
    cell-8-5 - cell
    cell-8-6 - cell
    cell-8-7 - cell
    cell-8-8 - cell
    cell-8-9 - cell
    cell-8-10 - cell
    cell-8-11 - cell
    cell-outside-0-left - cell
    cell-outside-0-right - cell
    cell-outside-1-left - cell
    cell-outside-1-right - cell
    cell-outside-2-left - cell
    cell-outside-2-right - cell
    cell-outside-3-left - cell
    cell-outside-3-right - cell
    cell-outside-4-left - cell
    cell-outside-4-right - cell
    cell-outside-5-left - cell
    cell-outside-5-right - cell
    cell-outside-6-left - cell
    cell-outside-6-right - cell
    cell-outside-7-left - cell
    cell-outside-7-right - cell
    cell-outside-8-left - cell
    cell-outside-8-right - cell
    cell-outside-0-up - cell
    cell-outside-0-down - cell
    cell-outside-1-up - cell
    cell-outside-1-down - cell
    cell-outside-2-up - cell
    cell-outside-2-down - cell
    cell-outside-3-up - cell
    cell-outside-3-down - cell
    cell-outside-4-up - cell
    cell-outside-4-down - cell
    cell-outside-5-up - cell
    cell-outside-5-down - cell
    cell-outside-6-up - cell
    cell-outside-6-down - cell
    cell-outside-7-up - cell
    cell-outside-7-down - cell
    cell-outside-8-up - cell
    cell-outside-8-down - cell
    cell-outside-9-up - cell
    cell-outside-9-down - cell
    cell-outside-10-up - cell
    cell-outside-10-down - cell
    cell-outside-11-up - cell
    cell-outside-11-down - cell
)
(:predicates
    (cell-capacity-inc ?x0 - cell-capacity-level ?x1 - cell-capacity-level)
    (cell-capacity ?x0 - cell ?x1 - cell-capacity-level)
    (cell-edge ?x0 - cell ?x1 - cell ?x2 - node ?x3 - node)
    (node-degree0 ?x0 - node)
    (node-degree1 ?x0 - node)
    (node-degree2 ?x0 - node)
    (linked ?x0 - node ?x1 - node)
    (disable-link-0-0)
    (NOT-node-degree1 ?x0 - node)
)
(:action link-0-0
    :parameters (?n1 - node ?n2 - node ?c1 - cell ?c1capfrom - cell-capacity-level ?c1capto - cell-capacity-level ?c2 - cell ?c2capfrom - cell-capacity-level ?c2capto - cell-capacity-level)
    :precondition (and (not (linked ?n1 ?n2)) (node-degree0 ?n1) (node-degree0 ?n2) (cell-edge ?c1 ?c2 ?n1 ?n2) (cell-capacity ?c1 ?c1capfrom) (cell-capacity ?c2 ?c2capfrom) (cell-capacity-inc ?c1capto ?c1capfrom) (cell-capacity-inc ?c2capto ?c2capfrom) (not (disable-link-0-0)))
    :effect (and (linked ?n1 ?n2) (not (node-degree0 ?n1)) (not (node-degree0 ?n2)) (not (cell-capacity ?c1 ?c1capfrom)) (cell-capacity ?c1 ?c1capto) (not (cell-capacity ?c2 ?c2capfrom)) (cell-capacity ?c2 ?c2capto) (disable-link-0-0) (node-degree1 ?n1) (not (NOT-node-degree1 ?n1)) (node-degree1 ?n2) (not (NOT-node-degree1 ?n2)))
)

(:action link-0-1
    :parameters (?n1 - node ?n2 - node ?c1 - cell ?c1capfrom - cell-capacity-level ?c1capto - cell-capacity-level ?c2 - cell ?c2capfrom - cell-capacity-level ?c2capto - cell-capacity-level)
    :precondition (and (not (linked ?n1 ?n2)) (node-degree0 ?n1) (node-degree1 ?n2) (cell-edge ?c1 ?c2 ?n1 ?n2) (cell-capacity ?c1 ?c1capfrom) (cell-capacity ?c2 ?c2capfrom) (cell-capacity-inc ?c1capto ?c1capfrom) (cell-capacity-inc ?c2capto ?c2capfrom))
    :effect (and (linked ?n1 ?n2) (not (node-degree0 ?n1)) (node-degree2 ?n2) (not (cell-capacity ?c1 ?c1capfrom)) (cell-capacity ?c1 ?c1capto) (not (cell-capacity ?c2 ?c2capfrom)) (cell-capacity ?c2 ?c2capto) (node-degree1 ?n1) (not (NOT-node-degree1 ?n1)) (not (node-degree1 ?n2)) (NOT-node-degree1 ?n2))
)

(:action link-1-0
    :parameters (?n1 - node ?n2 - node ?c1 - cell ?c1capfrom - cell-capacity-level ?c1capto - cell-capacity-level ?c2 - cell ?c2capfrom - cell-capacity-level ?c2capto - cell-capacity-level)
    :precondition (and (not (linked ?n1 ?n2)) (node-degree1 ?n1) (node-degree0 ?n2) (cell-edge ?c1 ?c2 ?n1 ?n2) (cell-capacity ?c1 ?c1capfrom) (cell-capacity ?c2 ?c2capfrom) (cell-capacity-inc ?c1capto ?c1capfrom) (cell-capacity-inc ?c2capto ?c2capfrom))
    :effect (and (linked ?n1 ?n2) (node-degree2 ?n1) (not (node-degree0 ?n2)) (not (cell-capacity ?c1 ?c1capfrom)) (cell-capacity ?c1 ?c1capto) (not (cell-capacity ?c2 ?c2capfrom)) (cell-capacity ?c2 ?c2capto) (not (node-degree1 ?n1)) (NOT-node-degree1 ?n1) (node-degree1 ?n2) (not (NOT-node-degree1 ?n2)))
)

(:action link-1-1
    :parameters (?n1 - node ?n2 - node ?c1 - cell ?c1capfrom - cell-capacity-level ?c1capto - cell-capacity-level ?c2 - cell ?c2capfrom - cell-capacity-level ?c2capto - cell-capacity-level)
    :precondition (and (not (linked ?n1 ?n2)) (node-degree1 ?n1) (node-degree1 ?n2) (cell-edge ?c1 ?c2 ?n1 ?n2) (cell-capacity ?c1 ?c1capfrom) (cell-capacity ?c2 ?c2capfrom) (cell-capacity-inc ?c1capto ?c1capfrom) (cell-capacity-inc ?c2capto ?c2capfrom))
    :effect (and (linked ?n1 ?n2) (node-degree2 ?n1) (node-degree2 ?n2) (not (cell-capacity ?c1 ?c1capfrom)) (cell-capacity ?c1 ?c1capto) (not (cell-capacity ?c2 ?c2capfrom)) (cell-capacity ?c2 ?c2capto) (not (node-degree1 ?n1)) (NOT-node-degree1 ?n1) (not (node-degree1 ?n2)) (NOT-node-degree1 ?n2))
)

)
