(define (problem transport-l8-t1-p9---int100n150-m25---int100c150---s2024---e0)
(:domain transport-strips)

(:objects
l0 l1 l2 l3 l4 l5 l6 l7 - location
t0 - truck
p0 p1 p2 p3 p4 p5 p6 p7 p8 - package
level0 level1 level2 level3 level4 level5 level6 level7 level8 level9 level10 level11 level12 level13 level14 level15 level16 level17 level18 level19 level20 level21 level22 level23 level24 level25 level26 level27 level28 level29 level30 level31 level32 level33 level34 level35 level36 level37 level38 level39 level40 level41 level42 level43 level44 level45 level46 level47 level48 level49 level50 level51 level52 level53 level54 - fuellevel
)

(:init
(sum level0 level1 level1)
(sum level0 level3 level3)
(sum level0 level5 level5)
(sum level0 level9 level9)
(sum level0 level20 level20)
(sum level0 level22 level22)
(sum level0 level23 level23)
(sum level0 level24 level24)
(sum level0 level25 level25)
(sum level1 level1 level2)
(sum level1 level3 level4)
(sum level1 level5 level6)
(sum level1 level9 level10)
(sum level1 level20 level21)
(sum level1 level22 level23)
(sum level1 level23 level24)
(sum level1 level24 level25)
(sum level1 level25 level26)
(sum level2 level1 level3)
(sum level2 level3 level5)
(sum level2 level5 level7)
(sum level2 level9 level11)
(sum level2 level20 level22)
(sum level2 level22 level24)
(sum level2 level23 level25)
(sum level2 level24 level26)
(sum level2 level25 level27)
(sum level3 level1 level4)
(sum level3 level3 level6)
(sum level3 level5 level8)
(sum level3 level9 level12)
(sum level3 level20 level23)
(sum level3 level22 level25)
(sum level3 level23 level26)
(sum level3 level24 level27)
(sum level3 level25 level28)
(sum level4 level1 level5)
(sum level4 level3 level7)
(sum level4 level5 level9)
(sum level4 level9 level13)
(sum level4 level20 level24)
(sum level4 level22 level26)
(sum level4 level23 level27)
(sum level4 level24 level28)
(sum level4 level25 level29)
(sum level5 level1 level6)
(sum level5 level3 level8)
(sum level5 level5 level10)
(sum level5 level9 level14)
(sum level5 level20 level25)
(sum level5 level22 level27)
(sum level5 level23 level28)
(sum level5 level24 level29)
(sum level5 level25 level30)
(sum level6 level1 level7)
(sum level6 level3 level9)
(sum level6 level5 level11)
(sum level6 level9 level15)
(sum level6 level20 level26)
(sum level6 level22 level28)
(sum level6 level23 level29)
(sum level6 level24 level30)
(sum level6 level25 level31)
(sum level7 level1 level8)
(sum level7 level3 level10)
(sum level7 level5 level12)
(sum level7 level9 level16)
(sum level7 level20 level27)
(sum level7 level22 level29)
(sum level7 level23 level30)
(sum level7 level24 level31)
(sum level7 level25 level32)
(sum level8 level1 level9)
(sum level8 level3 level11)
(sum level8 level5 level13)
(sum level8 level9 level17)
(sum level8 level20 level28)
(sum level8 level22 level30)
(sum level8 level23 level31)
(sum level8 level24 level32)
(sum level8 level25 level33)
(sum level9 level1 level10)
(sum level9 level3 level12)
(sum level9 level5 level14)
(sum level9 level9 level18)
(sum level9 level20 level29)
(sum level9 level22 level31)
(sum level9 level23 level32)
(sum level9 level24 level33)
(sum level9 level25 level34)
(sum level10 level1 level11)
(sum level10 level3 level13)
(sum level10 level5 level15)
(sum level10 level9 level19)
(sum level10 level20 level30)
(sum level10 level22 level32)
(sum level10 level23 level33)
(sum level10 level24 level34)
(sum level10 level25 level35)
(sum level11 level1 level12)
(sum level11 level3 level14)
(sum level11 level5 level16)
(sum level11 level9 level20)
(sum level11 level20 level31)
(sum level11 level22 level33)
(sum level11 level23 level34)
(sum level11 level24 level35)
(sum level11 level25 level36)
(sum level12 level1 level13)
(sum level12 level3 level15)
(sum level12 level5 level17)
(sum level12 level9 level21)
(sum level12 level20 level32)
(sum level12 level22 level34)
(sum level12 level23 level35)
(sum level12 level24 level36)
(sum level12 level25 level37)
(sum level13 level1 level14)
(sum level13 level3 level16)
(sum level13 level5 level18)
(sum level13 level9 level22)
(sum level13 level20 level33)
(sum level13 level22 level35)
(sum level13 level23 level36)
(sum level13 level24 level37)
(sum level13 level25 level38)
(sum level14 level1 level15)
(sum level14 level3 level17)
(sum level14 level5 level19)
(sum level14 level9 level23)
(sum level14 level20 level34)
(sum level14 level22 level36)
(sum level14 level23 level37)
(sum level14 level24 level38)
(sum level14 level25 level39)
(sum level15 level1 level16)
(sum level15 level3 level18)
(sum level15 level5 level20)
(sum level15 level9 level24)
(sum level15 level20 level35)
(sum level15 level22 level37)
(sum level15 level23 level38)
(sum level15 level24 level39)
(sum level15 level25 level40)
(sum level16 level1 level17)
(sum level16 level3 level19)
(sum level16 level5 level21)
(sum level16 level9 level25)
(sum level16 level20 level36)
(sum level16 level22 level38)
(sum level16 level23 level39)
(sum level16 level24 level40)
(sum level16 level25 level41)
(sum level17 level1 level18)
(sum level17 level3 level20)
(sum level17 level5 level22)
(sum level17 level9 level26)
(sum level17 level20 level37)
(sum level17 level22 level39)
(sum level17 level23 level40)
(sum level17 level24 level41)
(sum level17 level25 level42)
(sum level18 level1 level19)
(sum level18 level3 level21)
(sum level18 level5 level23)
(sum level18 level9 level27)
(sum level18 level20 level38)
(sum level18 level22 level40)
(sum level18 level23 level41)
(sum level18 level24 level42)
(sum level18 level25 level43)
(sum level19 level1 level20)
(sum level19 level3 level22)
(sum level19 level5 level24)
(sum level19 level9 level28)
(sum level19 level20 level39)
(sum level19 level22 level41)
(sum level19 level23 level42)
(sum level19 level24 level43)
(sum level19 level25 level44)
(sum level20 level1 level21)
(sum level20 level3 level23)
(sum level20 level5 level25)
(sum level20 level9 level29)
(sum level20 level20 level40)
(sum level20 level22 level42)
(sum level20 level23 level43)
(sum level20 level24 level44)
(sum level20 level25 level45)
(sum level21 level1 level22)
(sum level21 level3 level24)
(sum level21 level5 level26)
(sum level21 level9 level30)
(sum level21 level20 level41)
(sum level21 level22 level43)
(sum level21 level23 level44)
(sum level21 level24 level45)
(sum level21 level25 level46)
(sum level22 level1 level23)
(sum level22 level3 level25)
(sum level22 level5 level27)
(sum level22 level9 level31)
(sum level22 level20 level42)
(sum level22 level22 level44)
(sum level22 level23 level45)
(sum level22 level24 level46)
(sum level22 level25 level47)
(sum level23 level1 level24)
(sum level23 level3 level26)
(sum level23 level5 level28)
(sum level23 level9 level32)
(sum level23 level20 level43)
(sum level23 level22 level45)
(sum level23 level23 level46)
(sum level23 level24 level47)
(sum level23 level25 level48)
(sum level24 level1 level25)
(sum level24 level3 level27)
(sum level24 level5 level29)
(sum level24 level9 level33)
(sum level24 level20 level44)
(sum level24 level22 level46)
(sum level24 level23 level47)
(sum level24 level24 level48)
(sum level24 level25 level49)
(sum level25 level1 level26)
(sum level25 level3 level28)
(sum level25 level5 level30)
(sum level25 level9 level34)
(sum level25 level20 level45)
(sum level25 level22 level47)
(sum level25 level23 level48)
(sum level25 level24 level49)
(sum level25 level25 level50)
(sum level26 level1 level27)
(sum level26 level3 level29)
(sum level26 level5 level31)
(sum level26 level9 level35)
(sum level26 level20 level46)
(sum level26 level22 level48)
(sum level26 level23 level49)
(sum level26 level24 level50)
(sum level26 level25 level51)
(sum level27 level1 level28)
(sum level27 level3 level30)
(sum level27 level5 level32)
(sum level27 level9 level36)
(sum level27 level20 level47)
(sum level27 level22 level49)
(sum level27 level23 level50)
(sum level27 level24 level51)
(sum level27 level25 level52)
(sum level28 level1 level29)
(sum level28 level3 level31)
(sum level28 level5 level33)
(sum level28 level9 level37)
(sum level28 level20 level48)
(sum level28 level22 level50)
(sum level28 level23 level51)
(sum level28 level24 level52)
(sum level28 level25 level53)
(sum level29 level1 level30)
(sum level29 level3 level32)
(sum level29 level5 level34)
(sum level29 level9 level38)
(sum level29 level20 level49)
(sum level29 level22 level51)
(sum level29 level23 level52)
(sum level29 level24 level53)
(sum level29 level25 level54)
(sum level30 level1 level31)
(sum level30 level3 level33)
(sum level30 level5 level35)
(sum level30 level9 level39)
(sum level30 level20 level50)
(sum level30 level22 level52)
(sum level30 level23 level53)
(sum level30 level24 level54)
(sum level31 level1 level32)
(sum level31 level3 level34)
(sum level31 level5 level36)
(sum level31 level9 level40)
(sum level31 level20 level51)
(sum level31 level22 level53)
(sum level31 level23 level54)
(sum level32 level1 level33)
(sum level32 level3 level35)
(sum level32 level5 level37)
(sum level32 level9 level41)
(sum level32 level20 level52)
(sum level32 level22 level54)
(sum level33 level1 level34)
(sum level33 level3 level36)
(sum level33 level5 level38)
(sum level33 level9 level42)
(sum level33 level20 level53)
(sum level34 level1 level35)
(sum level34 level3 level37)
(sum level34 level5 level39)
(sum level34 level9 level43)
(sum level34 level20 level54)
(sum level35 level1 level36)
(sum level35 level3 level38)
(sum level35 level5 level40)
(sum level35 level9 level44)
(sum level36 level1 level37)
(sum level36 level3 level39)
(sum level36 level5 level41)
(sum level36 level9 level45)
(sum level37 level1 level38)
(sum level37 level3 level40)
(sum level37 level5 level42)
(sum level37 level9 level46)
(sum level38 level1 level39)
(sum level38 level3 level41)
(sum level38 level5 level43)
(sum level38 level9 level47)
(sum level39 level1 level40)
(sum level39 level3 level42)
(sum level39 level5 level44)
(sum level39 level9 level48)
(sum level40 level1 level41)
(sum level40 level3 level43)
(sum level40 level5 level45)
(sum level40 level9 level49)
(sum level41 level1 level42)
(sum level41 level3 level44)
(sum level41 level5 level46)
(sum level41 level9 level50)
(sum level42 level1 level43)
(sum level42 level3 level45)
(sum level42 level5 level47)
(sum level42 level9 level51)
(sum level43 level1 level44)
(sum level43 level3 level46)
(sum level43 level5 level48)
(sum level43 level9 level52)
(sum level44 level1 level45)
(sum level44 level3 level47)
(sum level44 level5 level49)
(sum level44 level9 level53)
(sum level45 level1 level46)
(sum level45 level3 level48)
(sum level45 level5 level50)
(sum level45 level9 level54)
(sum level46 level1 level47)
(sum level46 level3 level49)
(sum level46 level5 level51)
(sum level47 level1 level48)
(sum level47 level3 level50)
(sum level47 level5 level52)
(sum level48 level1 level49)
(sum level48 level3 level51)
(sum level48 level5 level53)
(sum level49 level1 level50)
(sum level49 level3 level52)
(sum level49 level5 level54)
(sum level50 level1 level51)
(sum level50 level3 level53)
(sum level51 level1 level52)
(sum level51 level3 level54)
(sum level52 level1 level53)
(sum level53 level1 level54)

(connected l0 l3)
(fuelcost level1 l0 l3)
(connected l0 l6)
(fuelcost level3 l0 l6)
(connected l0 l7)
(fuelcost level22 l0 l7)
(connected l1 l2)
(fuelcost level22 l1 l2)
(connected l1 l3)
(fuelcost level5 l1 l3)
(connected l1 l5)
(fuelcost level25 l1 l5)
(connected l1 l6)
(fuelcost level20 l1 l6)
(connected l1 l7)
(fuelcost level5 l1 l7)
(connected l2 l1)
(fuelcost level22 l2 l1)
(connected l2 l6)
(fuelcost level9 l2 l6)
(connected l3 l0)
(fuelcost level1 l3 l0)
(connected l3 l1)
(fuelcost level5 l3 l1)
(connected l3 l4)
(fuelcost level24 l3 l4)
(connected l4 l3)
(fuelcost level24 l4 l3)
(connected l4 l7)
(fuelcost level23 l4 l7)
(connected l5 l1)
(fuelcost level25 l5 l1)
(connected l5 l7)
(fuelcost level1 l5 l7)
(connected l6 l0)
(fuelcost level3 l6 l0)
(connected l6 l1)
(fuelcost level20 l6 l1)
(connected l6 l2)
(fuelcost level9 l6 l2)
(connected l7 l0)
(fuelcost level22 l7 l0)
(connected l7 l1)
(fuelcost level5 l7 l1)
(connected l7 l4)
(fuelcost level23 l7 l4)
(connected l7 l5)
(fuelcost level1 l7 l5)

(at t0 l0)
(fuel t0 level54)
(= (total-cost) 0)

(at p0 l5)
(at p1 l3)
(at p2 l0)
(at p3 l7)
(at p4 l7)
(at p5 l1)
(at p6 l7)
(at p7 l1)
(at p8 l1)
)

(:goal
(and
(at p0 l3)
(at p1 l0)
(at p2 l3)
(at p3 l2)
(at p4 l5)
(at p5 l7)
(at p6 l0)
(at p7 l7)
(at p8 l0)
)
)
(:metric minimize (total-cost)))