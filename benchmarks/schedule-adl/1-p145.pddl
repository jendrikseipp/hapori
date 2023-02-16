


(define (problem schedule-p10-s1-c1-w1-o2)
(:domain schedule)
(:objects 
    P0
    P1
    P2
    P3
    P4
    P5
    P6
    P7
    P8
    P9
 - part
    CIRCULAR
 - ashape
    BLUE
 - colour
    ONE
 - width
    FRONT
    BACK
 - anorient
)
(:init
(HAS-PAINT IMMERSION-PAINTER BLUE)
(HAS-PAINT SPRAY-PAINTER BLUE)
(CAN-ORIENT DRILL-PRESS FRONT)
(CAN-ORIENT PUNCH FRONT)
(CAN-ORIENT DRILL-PRESS BACK)
(CAN-ORIENT PUNCH BACK)
(HAS-BIT DRILL-PRESS ONE)
(HAS-BIT PUNCH ONE)
(SHAPE P0 CYLINDRICAL)
(SURFACE-CONDITION P0 POLISHED)
(HAS-HOLE P0 ONE BACK)
(TEMPERATURE P0 COLD)
(SHAPE P1 CYLINDRICAL)
(SURFACE-CONDITION P1 POLISHED)
(TEMPERATURE P1 COLD)
(SHAPE P2 CYLINDRICAL)
(SURFACE-CONDITION P2 ROUGH)
(HAS-HOLE P2 ONE BACK)
(TEMPERATURE P2 COLD)
(SHAPE P3 CYLINDRICAL)
(SURFACE-CONDITION P3 ROUGH)
(HAS-HOLE P3 ONE FRONT)
(TEMPERATURE P3 COLD)
(SHAPE P4 CYLINDRICAL)
(SURFACE-CONDITION P4 POLISHED)
(TEMPERATURE P4 COLD)
(SHAPE P5 CYLINDRICAL)
(SURFACE-CONDITION P5 ROUGH)
(PAINTED P5 BLUE)
(TEMPERATURE P5 COLD)
(SHAPE P6 CIRCULAR)
(SURFACE-CONDITION P6 ROUGH)
(PAINTED P6 BLUE)
(HAS-HOLE P6 ONE FRONT)
(TEMPERATURE P6 COLD)
(SHAPE P7 CYLINDRICAL)
(SURFACE-CONDITION P7 POLISHED)
(PAINTED P7 BLUE)
(TEMPERATURE P7 COLD)
(SHAPE P8 CYLINDRICAL)
(SURFACE-CONDITION P8 SMOOTH)
(PAINTED P8 BLUE)
(TEMPERATURE P8 COLD)
(SHAPE P9 CYLINDRICAL)
(SURFACE-CONDITION P9 POLISHED)
(HAS-HOLE P9 ONE BACK)
(TEMPERATURE P9 COLD)
)
(:goal
(and
(SHAPE P0 CYLINDRICAL)
(PAINTED P0 BLUE)
(SHAPE P1 CYLINDRICAL)
(SURFACE-CONDITION P1 POLISHED)
(PAINTED P1 BLUE)
(HAS-HOLE P2 ONE FRONT)
(HAS-HOLE P3 ONE FRONT)
(SHAPE P4 CYLINDRICAL)
(SURFACE-CONDITION P4 ROUGH)
(HAS-HOLE P4 ONE FRONT)
(SHAPE P5 CYLINDRICAL)
(SURFACE-CONDITION P5 SMOOTH)
(PAINTED P5 BLUE)
(HAS-HOLE P5 ONE FRONT)
(SURFACE-CONDITION P7 SMOOTH)
(PAINTED P7 BLUE)
(SHAPE P8 CYLINDRICAL)
(SURFACE-CONDITION P8 POLISHED)
(SHAPE P9 CYLINDRICAL)
(SURFACE-CONDITION P9 POLISHED)
(HAS-HOLE P9 ONE FRONT)
)
)
)


