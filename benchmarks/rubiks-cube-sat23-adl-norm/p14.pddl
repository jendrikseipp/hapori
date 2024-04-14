(define (problem rubiks-cube-shuffle-14) (:domain rubiks-cube)
(:init
  (cube1 green yellow orange)
  (cube2 green red white)
  (cube3 blue white red)
  (cube4 yellow blue orange)
  (cube5 white orange green)
  (cube6 blue yellow red)
  (cube7 orange blue white)
  (cube8 green yellow red)
  (edge12 yellow orange)
  (edge24 blue white)
  (edge34 orange green)
  (edge13 green yellow)
  (edge15 red green)
  (edge26 red blue)
  (edge48 green white)
  (edge37 blue orange)
  (edge56 white red)
  (edge68 yellow red)
  (edge78 orange white)
  (edge57 blue yellow)
)
(:goal (and (cube1 red white blue) (cube2 orange white blue) (cube3 red yellow blue) (cube4 orange yellow blue) (cube5 red white green) (cube6 orange white green) (cube7 red yellow green) (cube8 orange yellow green) (edge12 white blue) (edge24 orange blue) (edge34 yellow blue) (edge13 red blue) (edge15 red white) (edge26 orange white) (edge48 orange yellow) (edge37 red yellow) (edge56 white green) (edge68 orange green) (edge78 yellow green) (edge57 red green)))
)
