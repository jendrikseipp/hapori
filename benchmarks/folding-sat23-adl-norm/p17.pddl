(define (problem folding-spiral-20-9-325260) (:domain folding)
(:init
  (next-direction up clockwise right)
  (next-direction up counterclockwise left)
  (next-direction down clockwise left)
  (next-direction down counterclockwise right)
  (next-direction left clockwise up)
  (next-direction left counterclockwise down)
  (next-direction right clockwise down)
  (next-direction right counterclockwise up)
  (coord-inc c1 c2)
  (coord-inc c2 c3)
  (coord-inc c3 c4)
  (coord-inc c4 c5)
  (coord-inc c5 c6)
  (coord-inc c6 c7)
  (coord-inc c7 c8)
  (coord-inc c8 c9)
  (coord-inc c9 c10)
  (coord-inc c10 c11)
  (coord-inc c11 c12)
  (coord-inc c12 c13)
  (coord-inc c13 c14)
  (coord-inc c14 c15)
  (coord-inc c15 c16)
  (coord-inc c16 c17)
  (coord-inc c17 c18)
  (coord-inc c18 c19)
  (coord-inc c19 c20)
  (coord-inc c20 c21)
  (coord-inc c21 c22)
  (coord-inc c22 c23)
  (coord-inc c23 c24)
  (coord-inc c24 c25)
  (coord-inc c25 c26)
  (coord-inc c26 c27)
  (coord-inc c27 c28)
  (coord-inc c28 c29)
  (coord-inc c29 c30)
  (coord-inc c30 c31)
  (coord-inc c31 c32)
  (coord-inc c32 c33)
  (coord-inc c33 c34)
  (coord-inc c34 c35)
  (coord-inc c35 c36)
  (coord-inc c36 c37)
  (coord-inc c37 c38)
  (coord-inc c38 c39)
  (connected n1 n2)
  (connected n2 n3)
  (connected n3 n4)
  (connected n4 n5)
  (connected n5 n6)
  (connected n6 n7)
  (connected n7 n8)
  (connected n8 n9)
  (connected n9 n10)
  (connected n10 n11)
  (connected n11 n12)
  (connected n12 n13)
  (connected n13 n14)
  (connected n14 n15)
  (connected n15 n16)
  (connected n16 n17)
  (connected n17 n18)
  (connected n18 n19)
  (connected n19 n20)
  (end-node n20)
  (at n1 c20 c20)
  (at n2 c20 c21)
  (at n3 c20 c22)
  (at n4 c20 c23)
  (at n5 c20 c24)
  (at n6 c20 c25)
  (at n7 c20 c26)
  (at n8 c20 c27)
  (at n9 c20 c28)
  (at n10 c20 c29)
  (at n11 c20 c30)
  (at n12 c20 c31)
  (at n13 c20 c32)
  (at n14 c20 c33)
  (at n15 c20 c34)
  (at n16 c20 c35)
  (at n17 c20 c36)
  (at n18 c20 c37)
  (at n19 c20 c38)
  (at n20 c20 c39)
  (heading n1 up)
  (heading n2 up)
  (heading n3 up)
  (heading n4 up)
  (heading n5 up)
  (heading n6 up)
  (heading n7 up)
  (heading n8 up)
  (heading n9 up)
  (heading n10 up)
  (heading n11 up)
  (heading n12 up)
  (heading n13 up)
  (heading n14 up)
  (heading n15 up)
  (heading n16 up)
  (heading n17 up)
  (heading n18 up)
  (heading n19 up)
  (free c1 c1)
  (free c1 c2)
  (free c1 c3)
  (free c1 c4)
  (free c1 c5)
  (free c1 c6)
  (free c1 c7)
  (free c1 c8)
  (free c1 c9)
  (free c1 c10)
  (free c1 c11)
  (free c1 c12)
  (free c1 c13)
  (free c1 c14)
  (free c1 c15)
  (free c1 c16)
  (free c1 c17)
  (free c1 c18)
  (free c1 c19)
  (free c1 c20)
  (free c1 c21)
  (free c1 c22)
  (free c1 c23)
  (free c1 c24)
  (free c1 c25)
  (free c1 c26)
  (free c1 c27)
  (free c1 c28)
  (free c1 c29)
  (free c1 c30)
  (free c1 c31)
  (free c1 c32)
  (free c1 c33)
  (free c1 c34)
  (free c1 c35)
  (free c1 c36)
  (free c1 c37)
  (free c1 c38)
  (free c1 c39)
  (free c2 c1)
  (free c2 c2)
  (free c2 c3)
  (free c2 c4)
  (free c2 c5)
  (free c2 c6)
  (free c2 c7)
  (free c2 c8)
  (free c2 c9)
  (free c2 c10)
  (free c2 c11)
  (free c2 c12)
  (free c2 c13)
  (free c2 c14)
  (free c2 c15)
  (free c2 c16)
  (free c2 c17)
  (free c2 c18)
  (free c2 c19)
  (free c2 c20)
  (free c2 c21)
  (free c2 c22)
  (free c2 c23)
  (free c2 c24)
  (free c2 c25)
  (free c2 c26)
  (free c2 c27)
  (free c2 c28)
  (free c2 c29)
  (free c2 c30)
  (free c2 c31)
  (free c2 c32)
  (free c2 c33)
  (free c2 c34)
  (free c2 c35)
  (free c2 c36)
  (free c2 c37)
  (free c2 c38)
  (free c2 c39)
  (free c3 c1)
  (free c3 c2)
  (free c3 c3)
  (free c3 c4)
  (free c3 c5)
  (free c3 c6)
  (free c3 c7)
  (free c3 c8)
  (free c3 c9)
  (free c3 c10)
  (free c3 c11)
  (free c3 c12)
  (free c3 c13)
  (free c3 c14)
  (free c3 c15)
  (free c3 c16)
  (free c3 c17)
  (free c3 c18)
  (free c3 c19)
  (free c3 c20)
  (free c3 c21)
  (free c3 c22)
  (free c3 c23)
  (free c3 c24)
  (free c3 c25)
  (free c3 c26)
  (free c3 c27)
  (free c3 c28)
  (free c3 c29)
  (free c3 c30)
  (free c3 c31)
  (free c3 c32)
  (free c3 c33)
  (free c3 c34)
  (free c3 c35)
  (free c3 c36)
  (free c3 c37)
  (free c3 c38)
  (free c3 c39)
  (free c4 c1)
  (free c4 c2)
  (free c4 c3)
  (free c4 c4)
  (free c4 c5)
  (free c4 c6)
  (free c4 c7)
  (free c4 c8)
  (free c4 c9)
  (free c4 c10)
  (free c4 c11)
  (free c4 c12)
  (free c4 c13)
  (free c4 c14)
  (free c4 c15)
  (free c4 c16)
  (free c4 c17)
  (free c4 c18)
  (free c4 c19)
  (free c4 c20)
  (free c4 c21)
  (free c4 c22)
  (free c4 c23)
  (free c4 c24)
  (free c4 c25)
  (free c4 c26)
  (free c4 c27)
  (free c4 c28)
  (free c4 c29)
  (free c4 c30)
  (free c4 c31)
  (free c4 c32)
  (free c4 c33)
  (free c4 c34)
  (free c4 c35)
  (free c4 c36)
  (free c4 c37)
  (free c4 c38)
  (free c4 c39)
  (free c5 c1)
  (free c5 c2)
  (free c5 c3)
  (free c5 c4)
  (free c5 c5)
  (free c5 c6)
  (free c5 c7)
  (free c5 c8)
  (free c5 c9)
  (free c5 c10)
  (free c5 c11)
  (free c5 c12)
  (free c5 c13)
  (free c5 c14)
  (free c5 c15)
  (free c5 c16)
  (free c5 c17)
  (free c5 c18)
  (free c5 c19)
  (free c5 c20)
  (free c5 c21)
  (free c5 c22)
  (free c5 c23)
  (free c5 c24)
  (free c5 c25)
  (free c5 c26)
  (free c5 c27)
  (free c5 c28)
  (free c5 c29)
  (free c5 c30)
  (free c5 c31)
  (free c5 c32)
  (free c5 c33)
  (free c5 c34)
  (free c5 c35)
  (free c5 c36)
  (free c5 c37)
  (free c5 c38)
  (free c5 c39)
  (free c6 c1)
  (free c6 c2)
  (free c6 c3)
  (free c6 c4)
  (free c6 c5)
  (free c6 c6)
  (free c6 c7)
  (free c6 c8)
  (free c6 c9)
  (free c6 c10)
  (free c6 c11)
  (free c6 c12)
  (free c6 c13)
  (free c6 c14)
  (free c6 c15)
  (free c6 c16)
  (free c6 c17)
  (free c6 c18)
  (free c6 c19)
  (free c6 c20)
  (free c6 c21)
  (free c6 c22)
  (free c6 c23)
  (free c6 c24)
  (free c6 c25)
  (free c6 c26)
  (free c6 c27)
  (free c6 c28)
  (free c6 c29)
  (free c6 c30)
  (free c6 c31)
  (free c6 c32)
  (free c6 c33)
  (free c6 c34)
  (free c6 c35)
  (free c6 c36)
  (free c6 c37)
  (free c6 c38)
  (free c6 c39)
  (free c7 c1)
  (free c7 c2)
  (free c7 c3)
  (free c7 c4)
  (free c7 c5)
  (free c7 c6)
  (free c7 c7)
  (free c7 c8)
  (free c7 c9)
  (free c7 c10)
  (free c7 c11)
  (free c7 c12)
  (free c7 c13)
  (free c7 c14)
  (free c7 c15)
  (free c7 c16)
  (free c7 c17)
  (free c7 c18)
  (free c7 c19)
  (free c7 c20)
  (free c7 c21)
  (free c7 c22)
  (free c7 c23)
  (free c7 c24)
  (free c7 c25)
  (free c7 c26)
  (free c7 c27)
  (free c7 c28)
  (free c7 c29)
  (free c7 c30)
  (free c7 c31)
  (free c7 c32)
  (free c7 c33)
  (free c7 c34)
  (free c7 c35)
  (free c7 c36)
  (free c7 c37)
  (free c7 c38)
  (free c7 c39)
  (free c8 c1)
  (free c8 c2)
  (free c8 c3)
  (free c8 c4)
  (free c8 c5)
  (free c8 c6)
  (free c8 c7)
  (free c8 c8)
  (free c8 c9)
  (free c8 c10)
  (free c8 c11)
  (free c8 c12)
  (free c8 c13)
  (free c8 c14)
  (free c8 c15)
  (free c8 c16)
  (free c8 c17)
  (free c8 c18)
  (free c8 c19)
  (free c8 c20)
  (free c8 c21)
  (free c8 c22)
  (free c8 c23)
  (free c8 c24)
  (free c8 c25)
  (free c8 c26)
  (free c8 c27)
  (free c8 c28)
  (free c8 c29)
  (free c8 c30)
  (free c8 c31)
  (free c8 c32)
  (free c8 c33)
  (free c8 c34)
  (free c8 c35)
  (free c8 c36)
  (free c8 c37)
  (free c8 c38)
  (free c8 c39)
  (free c9 c1)
  (free c9 c2)
  (free c9 c3)
  (free c9 c4)
  (free c9 c5)
  (free c9 c6)
  (free c9 c7)
  (free c9 c8)
  (free c9 c9)
  (free c9 c10)
  (free c9 c11)
  (free c9 c12)
  (free c9 c13)
  (free c9 c14)
  (free c9 c15)
  (free c9 c16)
  (free c9 c17)
  (free c9 c18)
  (free c9 c19)
  (free c9 c20)
  (free c9 c21)
  (free c9 c22)
  (free c9 c23)
  (free c9 c24)
  (free c9 c25)
  (free c9 c26)
  (free c9 c27)
  (free c9 c28)
  (free c9 c29)
  (free c9 c30)
  (free c9 c31)
  (free c9 c32)
  (free c9 c33)
  (free c9 c34)
  (free c9 c35)
  (free c9 c36)
  (free c9 c37)
  (free c9 c38)
  (free c9 c39)
  (free c10 c1)
  (free c10 c2)
  (free c10 c3)
  (free c10 c4)
  (free c10 c5)
  (free c10 c6)
  (free c10 c7)
  (free c10 c8)
  (free c10 c9)
  (free c10 c10)
  (free c10 c11)
  (free c10 c12)
  (free c10 c13)
  (free c10 c14)
  (free c10 c15)
  (free c10 c16)
  (free c10 c17)
  (free c10 c18)
  (free c10 c19)
  (free c10 c20)
  (free c10 c21)
  (free c10 c22)
  (free c10 c23)
  (free c10 c24)
  (free c10 c25)
  (free c10 c26)
  (free c10 c27)
  (free c10 c28)
  (free c10 c29)
  (free c10 c30)
  (free c10 c31)
  (free c10 c32)
  (free c10 c33)
  (free c10 c34)
  (free c10 c35)
  (free c10 c36)
  (free c10 c37)
  (free c10 c38)
  (free c10 c39)
  (free c11 c1)
  (free c11 c2)
  (free c11 c3)
  (free c11 c4)
  (free c11 c5)
  (free c11 c6)
  (free c11 c7)
  (free c11 c8)
  (free c11 c9)
  (free c11 c10)
  (free c11 c11)
  (free c11 c12)
  (free c11 c13)
  (free c11 c14)
  (free c11 c15)
  (free c11 c16)
  (free c11 c17)
  (free c11 c18)
  (free c11 c19)
  (free c11 c20)
  (free c11 c21)
  (free c11 c22)
  (free c11 c23)
  (free c11 c24)
  (free c11 c25)
  (free c11 c26)
  (free c11 c27)
  (free c11 c28)
  (free c11 c29)
  (free c11 c30)
  (free c11 c31)
  (free c11 c32)
  (free c11 c33)
  (free c11 c34)
  (free c11 c35)
  (free c11 c36)
  (free c11 c37)
  (free c11 c38)
  (free c11 c39)
  (free c12 c1)
  (free c12 c2)
  (free c12 c3)
  (free c12 c4)
  (free c12 c5)
  (free c12 c6)
  (free c12 c7)
  (free c12 c8)
  (free c12 c9)
  (free c12 c10)
  (free c12 c11)
  (free c12 c12)
  (free c12 c13)
  (free c12 c14)
  (free c12 c15)
  (free c12 c16)
  (free c12 c17)
  (free c12 c18)
  (free c12 c19)
  (free c12 c20)
  (free c12 c21)
  (free c12 c22)
  (free c12 c23)
  (free c12 c24)
  (free c12 c25)
  (free c12 c26)
  (free c12 c27)
  (free c12 c28)
  (free c12 c29)
  (free c12 c30)
  (free c12 c31)
  (free c12 c32)
  (free c12 c33)
  (free c12 c34)
  (free c12 c35)
  (free c12 c36)
  (free c12 c37)
  (free c12 c38)
  (free c12 c39)
  (free c13 c1)
  (free c13 c2)
  (free c13 c3)
  (free c13 c4)
  (free c13 c5)
  (free c13 c6)
  (free c13 c7)
  (free c13 c8)
  (free c13 c9)
  (free c13 c10)
  (free c13 c11)
  (free c13 c12)
  (free c13 c13)
  (free c13 c14)
  (free c13 c15)
  (free c13 c16)
  (free c13 c17)
  (free c13 c18)
  (free c13 c19)
  (free c13 c20)
  (free c13 c21)
  (free c13 c22)
  (free c13 c23)
  (free c13 c24)
  (free c13 c25)
  (free c13 c26)
  (free c13 c27)
  (free c13 c28)
  (free c13 c29)
  (free c13 c30)
  (free c13 c31)
  (free c13 c32)
  (free c13 c33)
  (free c13 c34)
  (free c13 c35)
  (free c13 c36)
  (free c13 c37)
  (free c13 c38)
  (free c13 c39)
  (free c14 c1)
  (free c14 c2)
  (free c14 c3)
  (free c14 c4)
  (free c14 c5)
  (free c14 c6)
  (free c14 c7)
  (free c14 c8)
  (free c14 c9)
  (free c14 c10)
  (free c14 c11)
  (free c14 c12)
  (free c14 c13)
  (free c14 c14)
  (free c14 c15)
  (free c14 c16)
  (free c14 c17)
  (free c14 c18)
  (free c14 c19)
  (free c14 c20)
  (free c14 c21)
  (free c14 c22)
  (free c14 c23)
  (free c14 c24)
  (free c14 c25)
  (free c14 c26)
  (free c14 c27)
  (free c14 c28)
  (free c14 c29)
  (free c14 c30)
  (free c14 c31)
  (free c14 c32)
  (free c14 c33)
  (free c14 c34)
  (free c14 c35)
  (free c14 c36)
  (free c14 c37)
  (free c14 c38)
  (free c14 c39)
  (free c15 c1)
  (free c15 c2)
  (free c15 c3)
  (free c15 c4)
  (free c15 c5)
  (free c15 c6)
  (free c15 c7)
  (free c15 c8)
  (free c15 c9)
  (free c15 c10)
  (free c15 c11)
  (free c15 c12)
  (free c15 c13)
  (free c15 c14)
  (free c15 c15)
  (free c15 c16)
  (free c15 c17)
  (free c15 c18)
  (free c15 c19)
  (free c15 c20)
  (free c15 c21)
  (free c15 c22)
  (free c15 c23)
  (free c15 c24)
  (free c15 c25)
  (free c15 c26)
  (free c15 c27)
  (free c15 c28)
  (free c15 c29)
  (free c15 c30)
  (free c15 c31)
  (free c15 c32)
  (free c15 c33)
  (free c15 c34)
  (free c15 c35)
  (free c15 c36)
  (free c15 c37)
  (free c15 c38)
  (free c15 c39)
  (free c16 c1)
  (free c16 c2)
  (free c16 c3)
  (free c16 c4)
  (free c16 c5)
  (free c16 c6)
  (free c16 c7)
  (free c16 c8)
  (free c16 c9)
  (free c16 c10)
  (free c16 c11)
  (free c16 c12)
  (free c16 c13)
  (free c16 c14)
  (free c16 c15)
  (free c16 c16)
  (free c16 c17)
  (free c16 c18)
  (free c16 c19)
  (free c16 c20)
  (free c16 c21)
  (free c16 c22)
  (free c16 c23)
  (free c16 c24)
  (free c16 c25)
  (free c16 c26)
  (free c16 c27)
  (free c16 c28)
  (free c16 c29)
  (free c16 c30)
  (free c16 c31)
  (free c16 c32)
  (free c16 c33)
  (free c16 c34)
  (free c16 c35)
  (free c16 c36)
  (free c16 c37)
  (free c16 c38)
  (free c16 c39)
  (free c17 c1)
  (free c17 c2)
  (free c17 c3)
  (free c17 c4)
  (free c17 c5)
  (free c17 c6)
  (free c17 c7)
  (free c17 c8)
  (free c17 c9)
  (free c17 c10)
  (free c17 c11)
  (free c17 c12)
  (free c17 c13)
  (free c17 c14)
  (free c17 c15)
  (free c17 c16)
  (free c17 c17)
  (free c17 c18)
  (free c17 c19)
  (free c17 c20)
  (free c17 c21)
  (free c17 c22)
  (free c17 c23)
  (free c17 c24)
  (free c17 c25)
  (free c17 c26)
  (free c17 c27)
  (free c17 c28)
  (free c17 c29)
  (free c17 c30)
  (free c17 c31)
  (free c17 c32)
  (free c17 c33)
  (free c17 c34)
  (free c17 c35)
  (free c17 c36)
  (free c17 c37)
  (free c17 c38)
  (free c17 c39)
  (free c18 c1)
  (free c18 c2)
  (free c18 c3)
  (free c18 c4)
  (free c18 c5)
  (free c18 c6)
  (free c18 c7)
  (free c18 c8)
  (free c18 c9)
  (free c18 c10)
  (free c18 c11)
  (free c18 c12)
  (free c18 c13)
  (free c18 c14)
  (free c18 c15)
  (free c18 c16)
  (free c18 c17)
  (free c18 c18)
  (free c18 c19)
  (free c18 c20)
  (free c18 c21)
  (free c18 c22)
  (free c18 c23)
  (free c18 c24)
  (free c18 c25)
  (free c18 c26)
  (free c18 c27)
  (free c18 c28)
  (free c18 c29)
  (free c18 c30)
  (free c18 c31)
  (free c18 c32)
  (free c18 c33)
  (free c18 c34)
  (free c18 c35)
  (free c18 c36)
  (free c18 c37)
  (free c18 c38)
  (free c18 c39)
  (free c19 c1)
  (free c19 c2)
  (free c19 c3)
  (free c19 c4)
  (free c19 c5)
  (free c19 c6)
  (free c19 c7)
  (free c19 c8)
  (free c19 c9)
  (free c19 c10)
  (free c19 c11)
  (free c19 c12)
  (free c19 c13)
  (free c19 c14)
  (free c19 c15)
  (free c19 c16)
  (free c19 c17)
  (free c19 c18)
  (free c19 c19)
  (free c19 c20)
  (free c19 c21)
  (free c19 c22)
  (free c19 c23)
  (free c19 c24)
  (free c19 c25)
  (free c19 c26)
  (free c19 c27)
  (free c19 c28)
  (free c19 c29)
  (free c19 c30)
  (free c19 c31)
  (free c19 c32)
  (free c19 c33)
  (free c19 c34)
  (free c19 c35)
  (free c19 c36)
  (free c19 c37)
  (free c19 c38)
  (free c19 c39)
  (free c20 c1)
  (free c20 c2)
  (free c20 c3)
  (free c20 c4)
  (free c20 c5)
  (free c20 c6)
  (free c20 c7)
  (free c20 c8)
  (free c20 c9)
  (free c20 c10)
  (free c20 c11)
  (free c20 c12)
  (free c20 c13)
  (free c20 c14)
  (free c20 c15)
  (free c20 c16)
  (free c20 c17)
  (free c20 c18)
  (free c20 c19)
  (free c21 c1)
  (free c21 c2)
  (free c21 c3)
  (free c21 c4)
  (free c21 c5)
  (free c21 c6)
  (free c21 c7)
  (free c21 c8)
  (free c21 c9)
  (free c21 c10)
  (free c21 c11)
  (free c21 c12)
  (free c21 c13)
  (free c21 c14)
  (free c21 c15)
  (free c21 c16)
  (free c21 c17)
  (free c21 c18)
  (free c21 c19)
  (free c21 c20)
  (free c21 c21)
  (free c21 c22)
  (free c21 c23)
  (free c21 c24)
  (free c21 c25)
  (free c21 c26)
  (free c21 c27)
  (free c21 c28)
  (free c21 c29)
  (free c21 c30)
  (free c21 c31)
  (free c21 c32)
  (free c21 c33)
  (free c21 c34)
  (free c21 c35)
  (free c21 c36)
  (free c21 c37)
  (free c21 c38)
  (free c21 c39)
  (free c22 c1)
  (free c22 c2)
  (free c22 c3)
  (free c22 c4)
  (free c22 c5)
  (free c22 c6)
  (free c22 c7)
  (free c22 c8)
  (free c22 c9)
  (free c22 c10)
  (free c22 c11)
  (free c22 c12)
  (free c22 c13)
  (free c22 c14)
  (free c22 c15)
  (free c22 c16)
  (free c22 c17)
  (free c22 c18)
  (free c22 c19)
  (free c22 c20)
  (free c22 c21)
  (free c22 c22)
  (free c22 c23)
  (free c22 c24)
  (free c22 c25)
  (free c22 c26)
  (free c22 c27)
  (free c22 c28)
  (free c22 c29)
  (free c22 c30)
  (free c22 c31)
  (free c22 c32)
  (free c22 c33)
  (free c22 c34)
  (free c22 c35)
  (free c22 c36)
  (free c22 c37)
  (free c22 c38)
  (free c22 c39)
  (free c23 c1)
  (free c23 c2)
  (free c23 c3)
  (free c23 c4)
  (free c23 c5)
  (free c23 c6)
  (free c23 c7)
  (free c23 c8)
  (free c23 c9)
  (free c23 c10)
  (free c23 c11)
  (free c23 c12)
  (free c23 c13)
  (free c23 c14)
  (free c23 c15)
  (free c23 c16)
  (free c23 c17)
  (free c23 c18)
  (free c23 c19)
  (free c23 c20)
  (free c23 c21)
  (free c23 c22)
  (free c23 c23)
  (free c23 c24)
  (free c23 c25)
  (free c23 c26)
  (free c23 c27)
  (free c23 c28)
  (free c23 c29)
  (free c23 c30)
  (free c23 c31)
  (free c23 c32)
  (free c23 c33)
  (free c23 c34)
  (free c23 c35)
  (free c23 c36)
  (free c23 c37)
  (free c23 c38)
  (free c23 c39)
  (free c24 c1)
  (free c24 c2)
  (free c24 c3)
  (free c24 c4)
  (free c24 c5)
  (free c24 c6)
  (free c24 c7)
  (free c24 c8)
  (free c24 c9)
  (free c24 c10)
  (free c24 c11)
  (free c24 c12)
  (free c24 c13)
  (free c24 c14)
  (free c24 c15)
  (free c24 c16)
  (free c24 c17)
  (free c24 c18)
  (free c24 c19)
  (free c24 c20)
  (free c24 c21)
  (free c24 c22)
  (free c24 c23)
  (free c24 c24)
  (free c24 c25)
  (free c24 c26)
  (free c24 c27)
  (free c24 c28)
  (free c24 c29)
  (free c24 c30)
  (free c24 c31)
  (free c24 c32)
  (free c24 c33)
  (free c24 c34)
  (free c24 c35)
  (free c24 c36)
  (free c24 c37)
  (free c24 c38)
  (free c24 c39)
  (free c25 c1)
  (free c25 c2)
  (free c25 c3)
  (free c25 c4)
  (free c25 c5)
  (free c25 c6)
  (free c25 c7)
  (free c25 c8)
  (free c25 c9)
  (free c25 c10)
  (free c25 c11)
  (free c25 c12)
  (free c25 c13)
  (free c25 c14)
  (free c25 c15)
  (free c25 c16)
  (free c25 c17)
  (free c25 c18)
  (free c25 c19)
  (free c25 c20)
  (free c25 c21)
  (free c25 c22)
  (free c25 c23)
  (free c25 c24)
  (free c25 c25)
  (free c25 c26)
  (free c25 c27)
  (free c25 c28)
  (free c25 c29)
  (free c25 c30)
  (free c25 c31)
  (free c25 c32)
  (free c25 c33)
  (free c25 c34)
  (free c25 c35)
  (free c25 c36)
  (free c25 c37)
  (free c25 c38)
  (free c25 c39)
  (free c26 c1)
  (free c26 c2)
  (free c26 c3)
  (free c26 c4)
  (free c26 c5)
  (free c26 c6)
  (free c26 c7)
  (free c26 c8)
  (free c26 c9)
  (free c26 c10)
  (free c26 c11)
  (free c26 c12)
  (free c26 c13)
  (free c26 c14)
  (free c26 c15)
  (free c26 c16)
  (free c26 c17)
  (free c26 c18)
  (free c26 c19)
  (free c26 c20)
  (free c26 c21)
  (free c26 c22)
  (free c26 c23)
  (free c26 c24)
  (free c26 c25)
  (free c26 c26)
  (free c26 c27)
  (free c26 c28)
  (free c26 c29)
  (free c26 c30)
  (free c26 c31)
  (free c26 c32)
  (free c26 c33)
  (free c26 c34)
  (free c26 c35)
  (free c26 c36)
  (free c26 c37)
  (free c26 c38)
  (free c26 c39)
  (free c27 c1)
  (free c27 c2)
  (free c27 c3)
  (free c27 c4)
  (free c27 c5)
  (free c27 c6)
  (free c27 c7)
  (free c27 c8)
  (free c27 c9)
  (free c27 c10)
  (free c27 c11)
  (free c27 c12)
  (free c27 c13)
  (free c27 c14)
  (free c27 c15)
  (free c27 c16)
  (free c27 c17)
  (free c27 c18)
  (free c27 c19)
  (free c27 c20)
  (free c27 c21)
  (free c27 c22)
  (free c27 c23)
  (free c27 c24)
  (free c27 c25)
  (free c27 c26)
  (free c27 c27)
  (free c27 c28)
  (free c27 c29)
  (free c27 c30)
  (free c27 c31)
  (free c27 c32)
  (free c27 c33)
  (free c27 c34)
  (free c27 c35)
  (free c27 c36)
  (free c27 c37)
  (free c27 c38)
  (free c27 c39)
  (free c28 c1)
  (free c28 c2)
  (free c28 c3)
  (free c28 c4)
  (free c28 c5)
  (free c28 c6)
  (free c28 c7)
  (free c28 c8)
  (free c28 c9)
  (free c28 c10)
  (free c28 c11)
  (free c28 c12)
  (free c28 c13)
  (free c28 c14)
  (free c28 c15)
  (free c28 c16)
  (free c28 c17)
  (free c28 c18)
  (free c28 c19)
  (free c28 c20)
  (free c28 c21)
  (free c28 c22)
  (free c28 c23)
  (free c28 c24)
  (free c28 c25)
  (free c28 c26)
  (free c28 c27)
  (free c28 c28)
  (free c28 c29)
  (free c28 c30)
  (free c28 c31)
  (free c28 c32)
  (free c28 c33)
  (free c28 c34)
  (free c28 c35)
  (free c28 c36)
  (free c28 c37)
  (free c28 c38)
  (free c28 c39)
  (free c29 c1)
  (free c29 c2)
  (free c29 c3)
  (free c29 c4)
  (free c29 c5)
  (free c29 c6)
  (free c29 c7)
  (free c29 c8)
  (free c29 c9)
  (free c29 c10)
  (free c29 c11)
  (free c29 c12)
  (free c29 c13)
  (free c29 c14)
  (free c29 c15)
  (free c29 c16)
  (free c29 c17)
  (free c29 c18)
  (free c29 c19)
  (free c29 c20)
  (free c29 c21)
  (free c29 c22)
  (free c29 c23)
  (free c29 c24)
  (free c29 c25)
  (free c29 c26)
  (free c29 c27)
  (free c29 c28)
  (free c29 c29)
  (free c29 c30)
  (free c29 c31)
  (free c29 c32)
  (free c29 c33)
  (free c29 c34)
  (free c29 c35)
  (free c29 c36)
  (free c29 c37)
  (free c29 c38)
  (free c29 c39)
  (free c30 c1)
  (free c30 c2)
  (free c30 c3)
  (free c30 c4)
  (free c30 c5)
  (free c30 c6)
  (free c30 c7)
  (free c30 c8)
  (free c30 c9)
  (free c30 c10)
  (free c30 c11)
  (free c30 c12)
  (free c30 c13)
  (free c30 c14)
  (free c30 c15)
  (free c30 c16)
  (free c30 c17)
  (free c30 c18)
  (free c30 c19)
  (free c30 c20)
  (free c30 c21)
  (free c30 c22)
  (free c30 c23)
  (free c30 c24)
  (free c30 c25)
  (free c30 c26)
  (free c30 c27)
  (free c30 c28)
  (free c30 c29)
  (free c30 c30)
  (free c30 c31)
  (free c30 c32)
  (free c30 c33)
  (free c30 c34)
  (free c30 c35)
  (free c30 c36)
  (free c30 c37)
  (free c30 c38)
  (free c30 c39)
  (free c31 c1)
  (free c31 c2)
  (free c31 c3)
  (free c31 c4)
  (free c31 c5)
  (free c31 c6)
  (free c31 c7)
  (free c31 c8)
  (free c31 c9)
  (free c31 c10)
  (free c31 c11)
  (free c31 c12)
  (free c31 c13)
  (free c31 c14)
  (free c31 c15)
  (free c31 c16)
  (free c31 c17)
  (free c31 c18)
  (free c31 c19)
  (free c31 c20)
  (free c31 c21)
  (free c31 c22)
  (free c31 c23)
  (free c31 c24)
  (free c31 c25)
  (free c31 c26)
  (free c31 c27)
  (free c31 c28)
  (free c31 c29)
  (free c31 c30)
  (free c31 c31)
  (free c31 c32)
  (free c31 c33)
  (free c31 c34)
  (free c31 c35)
  (free c31 c36)
  (free c31 c37)
  (free c31 c38)
  (free c31 c39)
  (free c32 c1)
  (free c32 c2)
  (free c32 c3)
  (free c32 c4)
  (free c32 c5)
  (free c32 c6)
  (free c32 c7)
  (free c32 c8)
  (free c32 c9)
  (free c32 c10)
  (free c32 c11)
  (free c32 c12)
  (free c32 c13)
  (free c32 c14)
  (free c32 c15)
  (free c32 c16)
  (free c32 c17)
  (free c32 c18)
  (free c32 c19)
  (free c32 c20)
  (free c32 c21)
  (free c32 c22)
  (free c32 c23)
  (free c32 c24)
  (free c32 c25)
  (free c32 c26)
  (free c32 c27)
  (free c32 c28)
  (free c32 c29)
  (free c32 c30)
  (free c32 c31)
  (free c32 c32)
  (free c32 c33)
  (free c32 c34)
  (free c32 c35)
  (free c32 c36)
  (free c32 c37)
  (free c32 c38)
  (free c32 c39)
  (free c33 c1)
  (free c33 c2)
  (free c33 c3)
  (free c33 c4)
  (free c33 c5)
  (free c33 c6)
  (free c33 c7)
  (free c33 c8)
  (free c33 c9)
  (free c33 c10)
  (free c33 c11)
  (free c33 c12)
  (free c33 c13)
  (free c33 c14)
  (free c33 c15)
  (free c33 c16)
  (free c33 c17)
  (free c33 c18)
  (free c33 c19)
  (free c33 c20)
  (free c33 c21)
  (free c33 c22)
  (free c33 c23)
  (free c33 c24)
  (free c33 c25)
  (free c33 c26)
  (free c33 c27)
  (free c33 c28)
  (free c33 c29)
  (free c33 c30)
  (free c33 c31)
  (free c33 c32)
  (free c33 c33)
  (free c33 c34)
  (free c33 c35)
  (free c33 c36)
  (free c33 c37)
  (free c33 c38)
  (free c33 c39)
  (free c34 c1)
  (free c34 c2)
  (free c34 c3)
  (free c34 c4)
  (free c34 c5)
  (free c34 c6)
  (free c34 c7)
  (free c34 c8)
  (free c34 c9)
  (free c34 c10)
  (free c34 c11)
  (free c34 c12)
  (free c34 c13)
  (free c34 c14)
  (free c34 c15)
  (free c34 c16)
  (free c34 c17)
  (free c34 c18)
  (free c34 c19)
  (free c34 c20)
  (free c34 c21)
  (free c34 c22)
  (free c34 c23)
  (free c34 c24)
  (free c34 c25)
  (free c34 c26)
  (free c34 c27)
  (free c34 c28)
  (free c34 c29)
  (free c34 c30)
  (free c34 c31)
  (free c34 c32)
  (free c34 c33)
  (free c34 c34)
  (free c34 c35)
  (free c34 c36)
  (free c34 c37)
  (free c34 c38)
  (free c34 c39)
  (free c35 c1)
  (free c35 c2)
  (free c35 c3)
  (free c35 c4)
  (free c35 c5)
  (free c35 c6)
  (free c35 c7)
  (free c35 c8)
  (free c35 c9)
  (free c35 c10)
  (free c35 c11)
  (free c35 c12)
  (free c35 c13)
  (free c35 c14)
  (free c35 c15)
  (free c35 c16)
  (free c35 c17)
  (free c35 c18)
  (free c35 c19)
  (free c35 c20)
  (free c35 c21)
  (free c35 c22)
  (free c35 c23)
  (free c35 c24)
  (free c35 c25)
  (free c35 c26)
  (free c35 c27)
  (free c35 c28)
  (free c35 c29)
  (free c35 c30)
  (free c35 c31)
  (free c35 c32)
  (free c35 c33)
  (free c35 c34)
  (free c35 c35)
  (free c35 c36)
  (free c35 c37)
  (free c35 c38)
  (free c35 c39)
  (free c36 c1)
  (free c36 c2)
  (free c36 c3)
  (free c36 c4)
  (free c36 c5)
  (free c36 c6)
  (free c36 c7)
  (free c36 c8)
  (free c36 c9)
  (free c36 c10)
  (free c36 c11)
  (free c36 c12)
  (free c36 c13)
  (free c36 c14)
  (free c36 c15)
  (free c36 c16)
  (free c36 c17)
  (free c36 c18)
  (free c36 c19)
  (free c36 c20)
  (free c36 c21)
  (free c36 c22)
  (free c36 c23)
  (free c36 c24)
  (free c36 c25)
  (free c36 c26)
  (free c36 c27)
  (free c36 c28)
  (free c36 c29)
  (free c36 c30)
  (free c36 c31)
  (free c36 c32)
  (free c36 c33)
  (free c36 c34)
  (free c36 c35)
  (free c36 c36)
  (free c36 c37)
  (free c36 c38)
  (free c36 c39)
  (free c37 c1)
  (free c37 c2)
  (free c37 c3)
  (free c37 c4)
  (free c37 c5)
  (free c37 c6)
  (free c37 c7)
  (free c37 c8)
  (free c37 c9)
  (free c37 c10)
  (free c37 c11)
  (free c37 c12)
  (free c37 c13)
  (free c37 c14)
  (free c37 c15)
  (free c37 c16)
  (free c37 c17)
  (free c37 c18)
  (free c37 c19)
  (free c37 c20)
  (free c37 c21)
  (free c37 c22)
  (free c37 c23)
  (free c37 c24)
  (free c37 c25)
  (free c37 c26)
  (free c37 c27)
  (free c37 c28)
  (free c37 c29)
  (free c37 c30)
  (free c37 c31)
  (free c37 c32)
  (free c37 c33)
  (free c37 c34)
  (free c37 c35)
  (free c37 c36)
  (free c37 c37)
  (free c37 c38)
  (free c37 c39)
  (free c38 c1)
  (free c38 c2)
  (free c38 c3)
  (free c38 c4)
  (free c38 c5)
  (free c38 c6)
  (free c38 c7)
  (free c38 c8)
  (free c38 c9)
  (free c38 c10)
  (free c38 c11)
  (free c38 c12)
  (free c38 c13)
  (free c38 c14)
  (free c38 c15)
  (free c38 c16)
  (free c38 c17)
  (free c38 c18)
  (free c38 c19)
  (free c38 c20)
  (free c38 c21)
  (free c38 c22)
  (free c38 c23)
  (free c38 c24)
  (free c38 c25)
  (free c38 c26)
  (free c38 c27)
  (free c38 c28)
  (free c38 c29)
  (free c38 c30)
  (free c38 c31)
  (free c38 c32)
  (free c38 c33)
  (free c38 c34)
  (free c38 c35)
  (free c38 c36)
  (free c38 c37)
  (free c38 c38)
  (free c38 c39)
  (free c39 c1)
  (free c39 c2)
  (free c39 c3)
  (free c39 c4)
  (free c39 c5)
  (free c39 c6)
  (free c39 c7)
  (free c39 c8)
  (free c39 c9)
  (free c39 c10)
  (free c39 c11)
  (free c39 c12)
  (free c39 c13)
  (free c39 c14)
  (free c39 c15)
  (free c39 c16)
  (free c39 c17)
  (free c39 c18)
  (free c39 c19)
  (free c39 c20)
  (free c39 c21)
  (free c39 c22)
  (free c39 c23)
  (free c39 c24)
  (free c39 c25)
  (free c39 c26)
  (free c39 c27)
  (free c39 c28)
  (free c39 c29)
  (free c39 c30)
  (free c39 c31)
  (free c39 c32)
  (free c39 c33)
  (free c39 c34)
  (free c39 c35)
  (free c39 c36)
  (free c39 c37)
  (free c39 c38)
  (free c39 c39)
  (= (total-cost) 0)
  (= (rotate-cost) 1)
  (= (update-cost) 0)
  (NOT-rotating)
)
(:goal (and (at n1 c20 c20) (at n2 c21 c20) (at n3 c21 c19) (at n4 c20 c19) (at n5 c19 c19) (at n6 c19 c20) (at n7 c19 c21) (at n8 c19 c22) (at n9 c20 c22) (at n10 c21 c22) (at n11 c22 c22) (at n12 c23 c22) (at n13 c24 c22) (at n14 c24 c21) (at n15 c24 c20) (at n16 c24 c19) (at n17 c23 c19) (at n18 c22 c19) (at n19 c22 c20) (at n20 c23 c20) (NOT-rotating)))
(:metric minimize (total-cost))
)
