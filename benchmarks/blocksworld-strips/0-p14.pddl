

(define (problem BW-rand-18)
(:domain blocksworld)
(:objects b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 )
(:init
(arm-empty)
(on b1 b14)
(on b2 b9)
(on b3 b18)
(on-table b4)
(on b5 b4)
(on b6 b2)
(on b7 b3)
(on b8 b15)
(on b9 b16)
(on-table b10)
(on b11 b5)
(on b12 b1)
(on b13 b7)
(on b14 b17)
(on b15 b11)
(on b16 b8)
(on b17 b6)
(on b18 b10)
(clear b12)
(clear b13)
)
(:goal
(and
(on b2 b11)
(on b3 b4)
(on b4 b15)
(on b6 b9)
(on b7 b13)
(on b8 b5)
(on b9 b10)
(on b10 b17)
(on b11 b6)
(on b12 b16)
(on b13 b3)
(on b16 b1)
(on b17 b14)
(on b18 b8))
)
)

