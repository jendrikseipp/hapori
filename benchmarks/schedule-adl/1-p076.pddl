


(define (problem schedule-p7-s1-c2-w1-o2)
(:domain schedule)
(:objects 
    P0
    P1
    P2
    P3
    P4
    P5
    P6
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
(SURFACE-CONDITION P0 POLISHED)
(TEMPERATURE P0 COLD)
(SHAPE P1 CYLINDRICAL)
(SURFACE-CONDITION P1 POLISHED)
(TEMPERATURE P1 COLD)
(SHAPE P2 CYLINDRICAL)
(SURFACE-CONDITION P2 POLISHED)
(PAINTED P2 BLUE)
(TEMPERATURE P2 COLD)
(SHAPE P3 CIRCULAR)
(SURFACE-CONDITION P3 SMOOTH)
(PAINTED P3 YELLOW)
(TEMPERATURE P3 COLD)
(SHAPE P4 CIRCULAR)
(SURFACE-CONDITION P4 ROUGH)
(PAINTED P4 YELLOW)
(HAS-HOLE P4 ONE FRONT)
(TEMPERATURE P4 COLD)
(SHAPE P5 CYLINDRICAL)
(SURFACE-CONDITION P5 SMOOTH)
(HAS-HOLE P5 ONE BACK)
(TEMPERATURE P5 COLD)
(SHAPE P6 CYLINDRICAL)
(SURFACE-CONDITION P6 POLISHED)
(PAINTED P6 BLUE)
(TEMPERATURE P6 COLD)
)
(:goal
(and
(SHAPE P0 CYLINDRICAL)
(PAINTED P0 BLUE)
(HAS-HOLE P0 ONE FRONT)
(SHAPE P1 CYLINDRICAL)
(SURFACE-CONDITION P1 SMOOTH)
(PAINTED P2 BLUE)
(HAS-HOLE P2 ONE FRONT)
(SHAPE P3 CYLINDRICAL)
(PAINTED P3 YELLOW)
(HAS-HOLE P3 ONE FRONT)
(SHAPE P4 CYLINDRICAL)
(SURFACE-CONDITION P4 POLISHED)
(SHAPE P5 CYLINDRICAL)
(SURFACE-CONDITION P5 POLISHED)
(PAINTED P5 BLUE)
(SURFACE-CONDITION P6 POLISHED)
(HAS-HOLE P6 ONE FRONT)
)
)
)


