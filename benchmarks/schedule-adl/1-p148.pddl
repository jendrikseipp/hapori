


(define (problem schedule-p10-s1-c2-w1-o2)
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
    YELLOW
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
(HAS-PAINT IMMERSION-PAINTER YELLOW)
(HAS-PAINT SPRAY-PAINTER YELLOW)
(CAN-ORIENT DRILL-PRESS FRONT)
(CAN-ORIENT PUNCH FRONT)
(CAN-ORIENT DRILL-PRESS BACK)
(CAN-ORIENT PUNCH BACK)
(HAS-BIT DRILL-PRESS ONE)
(HAS-BIT PUNCH ONE)
(SHAPE P0 CYLINDRICAL)
(SURFACE-CONDITION P0 ROUGH)
(HAS-HOLE P0 ONE BACK)
(TEMPERATURE P0 COLD)
(SHAPE P1 CYLINDRICAL)
(SURFACE-CONDITION P1 SMOOTH)
(HAS-HOLE P1 ONE FRONT)
(TEMPERATURE P1 COLD)
(SHAPE P2 CIRCULAR)
(SURFACE-CONDITION P2 POLISHED)
(PAINTED P2 BLUE)
(HAS-HOLE P2 ONE FRONT)
(TEMPERATURE P2 COLD)
(SHAPE P3 CIRCULAR)
(SURFACE-CONDITION P3 SMOOTH)
(PAINTED P3 BLUE)
(TEMPERATURE P3 COLD)
(SHAPE P4 CYLINDRICAL)
(SURFACE-CONDITION P4 SMOOTH)
(TEMPERATURE P4 COLD)
(SHAPE P5 CYLINDRICAL)
(SURFACE-CONDITION P5 ROUGH)
(PAINTED P5 BLUE)
(HAS-HOLE P5 ONE FRONT)
(TEMPERATURE P5 COLD)
(SHAPE P6 CIRCULAR)
(SURFACE-CONDITION P6 SMOOTH)
(PAINTED P6 YELLOW)
(TEMPERATURE P6 COLD)
(SHAPE P7 CYLINDRICAL)
(SURFACE-CONDITION P7 POLISHED)
(TEMPERATURE P7 COLD)
(SHAPE P8 CYLINDRICAL)
(SURFACE-CONDITION P8 POLISHED)
(PAINTED P8 BLUE)
(HAS-HOLE P8 ONE BACK)
(TEMPERATURE P8 COLD)
(SHAPE P9 CIRCULAR)
(SURFACE-CONDITION P9 POLISHED)
(HAS-HOLE P9 ONE BACK)
(TEMPERATURE P9 COLD)
)
(:goal
(and
(SURFACE-CONDITION P0 SMOOTH)
(PAINTED P0 YELLOW)
(HAS-HOLE P0 ONE BACK)
(SHAPE P1 CYLINDRICAL)
(SURFACE-CONDITION P1 SMOOTH)
(SURFACE-CONDITION P2 POLISHED)
(PAINTED P2 YELLOW)
(SURFACE-CONDITION P3 ROUGH)
(HAS-HOLE P3 ONE FRONT)
(SHAPE P4 CYLINDRICAL)
(SHAPE P5 CYLINDRICAL)
(SURFACE-CONDITION P5 POLISHED)
(HAS-HOLE P5 ONE FRONT)
(SHAPE P6 CYLINDRICAL)
(SURFACE-CONDITION P6 POLISHED)
(PAINTED P6 YELLOW)
(HAS-HOLE P6 ONE FRONT)
(SURFACE-CONDITION P8 SMOOTH)
(HAS-HOLE P8 ONE BACK)
(SHAPE P9 CYLINDRICAL)
(SURFACE-CONDITION P9 SMOOTH)
)
)
)

