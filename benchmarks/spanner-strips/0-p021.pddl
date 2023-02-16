(define (problem prob)
 (:domain spanner)
 (:objects 
     bob - man
 spanner1 spanner2 spanner3 spanner4 spanner5 spanner6 spanner7 spanner8 spanner9 spanner10 spanner11 spanner12 spanner13 - spanner
     nut1 nut2 nut3 nut4 nut5 nut6 nut7 nut8 nut9 nut10 nut11 nut12 nut13 - nut
     location1 location2 location3 location4 location5 location6 location7 location8 location9 location10 location11 location12 location13 location14 location15 location16 location17 location18 location19 location20 location21 location22 location23 location24 location25 - location
     shed gate - location
    )
 (:init 
    (at bob shed)
    (at spanner1 location4)
    (useable spanner1)
    (at spanner2 location9)
    (useable spanner2)
    (at spanner3 location22)
    (useable spanner3)
    (at spanner4 location7)
    (useable spanner4)
    (at spanner5 location1)
    (useable spanner5)
    (at spanner6 location25)
    (useable spanner6)
    (at spanner7 location2)
    (useable spanner7)
    (at spanner8 location3)
    (useable spanner8)
    (at spanner9 location7)
    (useable spanner9)
    (at spanner10 location21)
    (useable spanner10)
    (at spanner11 location23)
    (useable spanner11)
    (at spanner12 location6)
    (useable spanner12)
    (at spanner13 location8)
    (useable spanner13)
    (loose nut1)
    (at nut1 gate)
    (loose nut2)
    (at nut2 gate)
    (loose nut3)
    (at nut3 gate)
    (loose nut4)
    (at nut4 gate)
    (loose nut5)
    (at nut5 gate)
    (loose nut6)
    (at nut6 gate)
    (loose nut7)
    (at nut7 gate)
    (loose nut8)
    (at nut8 gate)
    (loose nut9)
    (at nut9 gate)
    (loose nut10)
    (at nut10 gate)
    (loose nut11)
    (at nut11 gate)
    (loose nut12)
    (at nut12 gate)
    (loose nut13)
    (at nut13 gate)
    (link shed location1)
    (link location25 gate)
    (link location1 location2)
    (link location2 location3)
    (link location3 location4)
    (link location4 location5)
    (link location5 location6)
    (link location6 location7)
    (link location7 location8)
    (link location8 location9)
    (link location9 location10)
    (link location10 location11)
    (link location11 location12)
    (link location12 location13)
    (link location13 location14)
    (link location14 location15)
    (link location15 location16)
    (link location16 location17)
    (link location17 location18)
    (link location18 location19)
    (link location19 location20)
    (link location20 location21)
    (link location21 location22)
    (link location22 location23)
    (link location23 location24)
    (link location24 location25)
)
 (:goal
  (and
   (tightened nut1)
   (tightened nut2)
   (tightened nut3)
   (tightened nut4)
   (tightened nut5)
   (tightened nut6)
   (tightened nut7)
   (tightened nut8)
   (tightened nut9)
   (tightened nut10)
   (tightened nut11)
   (tightened nut12)
   (tightened nut13)
)))
