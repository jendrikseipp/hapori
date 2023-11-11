;; Generated with ../generator.py -s 1087 -o p16-1087.pddl -p p16-1087.plan 16
(define
(problem rubiks-cube-shuffle-16)
(:domain rubiks-cube)
(:objects yellow white blue green orange red)
(:init
    (cube1 red yellow green)
    (cube2 yellow orange blue)
    (cube3 blue red yellow)
    (cube4 yellow orange green)
    (cube5 white orange green)
    (cube6 white red green)
    (cube7 blue red white)
    (cube8 orange white blue)
    (edge12 orange blue)
    (edge24 white orange)
    (edge34 blue red)
    (edge13 yellow red)
    (edge15 yellow blue)
    (edge26 green red)
    (edge48 yellow orange)
    (edge37 white green)
    (edge56 white red)
    (edge68 yellow green)
    (edge78 green orange)
    (edge57 blue white)
)
(:goal
    (and
        (cube1 red white blue)
        (cube2 orange white blue)
        (cube3 red yellow blue)
        (cube4 orange yellow blue)
        (cube5 red white green)
        (cube6 orange white green)
        (cube7 red yellow green)
        (cube8 orange yellow green)

        (edge12 white blue)
        (edge24 orange blue)
        (edge34 yellow blue)
        (edge13 red blue)

        (edge15 red white)
        (edge26 orange white)
        (edge48 orange yellow)
        (edge37 red yellow)

        (edge56 white green)
        (edge68 orange green)
        (edge78 yellow green)
        (edge57 red green)

    )
)
)
