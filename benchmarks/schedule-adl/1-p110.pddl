


(define (problem schedule-p8-s2-c1-w2-o2)
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
 - part
    CIRCULAR
    OBLONG
 - ashape
    BLUE
 - colour
    ONE
    TWO
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
(HAS-BIT DRILL-PRESS TWO)
(HAS-BIT PUNCH TWO)
(SHAPE P0 CIRCULAR)
(SURFACE-CONDITION P0 POLISHED)
(PAINTED P0 BLUE)
(HAS-HOLE P0 ONE FRONT)
(TEMPERATURE P0 COLD)
(SHAPE P1 OBLONG)
(SURFACE-CONDITION P1 SMOOTH)
(PAINTED P1 BLUE)
(TEMPERATURE P1 COLD)
(SHAPE P2 CYLINDRICAL)
(SURFACE-CONDITION P2 POLISHED)
(TEMPERATURE P2 COLD)
(SHAPE P3 OBLONG)
(SURFACE-CONDITION P3 SMOOTH)
(PAINTED P3 BLUE)
(HAS-HOLE P3 TWO BACK)
(TEMPERATURE P3 COLD)
(SHAPE P4 OBLONG)
(SURFACE-CONDITION P4 SMOOTH)
(PAINTED P4 BLUE)
(HAS-HOLE P4 ONE BACK)
(TEMPERATURE P4 COLD)
(SHAPE P5 OBLONG)
(SURFACE-CONDITION P5 ROUGH)
(PAINTED P5 BLUE)
(HAS-HOLE P5 ONE BACK)
(TEMPERATURE P5 COLD)
(SHAPE P6 CYLINDRICAL)
(SURFACE-CONDITION P6 ROUGH)
(PAINTED P6 BLUE)
(HAS-HOLE P6 ONE BACK)
(TEMPERATURE P6 COLD)
(SHAPE P7 CYLINDRICAL)
(SURFACE-CONDITION P7 SMOOTH)
(PAINTED P7 BLUE)
(TEMPERATURE P7 COLD)
)
(:goal
(and
(SURFACE-CONDITION P0 SMOOTH)
(PAINTED P0 BLUE)
(HAS-HOLE P0 ONE FRONT)
(PAINTED P1 BLUE)
(SURFACE-CONDITION P2 SMOOTH)
(PAINTED P2 BLUE)
(HAS-HOLE P2 ONE BACK)
(PAINTED P3 BLUE)
(HAS-HOLE P3 TWO FRONT)
(SHAPE P4 CYLINDRICAL)
(SURFACE-CONDITION P4 POLISHED)
(SHAPE P5 CYLINDRICAL)
(PAINTED P5 BLUE)
(SURFACE-CONDITION P6 ROUGH)
(PAINTED P6 BLUE)
(SHAPE P7 CYLINDRICAL)
)
)
)


