


(define (problem schedule-p10-s1-c2-w3-o2)
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
    TWO
    THREE
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
(HAS-BIT DRILL-PRESS TWO)
(HAS-BIT PUNCH TWO)
(HAS-BIT DRILL-PRESS THREE)
(HAS-BIT PUNCH THREE)
(SHAPE P0 CIRCULAR)
(SURFACE-CONDITION P0 SMOOTH)
(PAINTED P0 YELLOW)
(TEMPERATURE P0 COLD)
(SHAPE P1 CIRCULAR)
(SURFACE-CONDITION P1 ROUGH)
(HAS-HOLE P1 TWO FRONT)
(TEMPERATURE P1 COLD)
(SHAPE P2 CYLINDRICAL)
(SURFACE-CONDITION P2 SMOOTH)
(HAS-HOLE P2 ONE FRONT)
(TEMPERATURE P2 COLD)
(SHAPE P3 CIRCULAR)
(SURFACE-CONDITION P3 ROUGH)
(PAINTED P3 YELLOW)
(HAS-HOLE P3 TWO FRONT)
(TEMPERATURE P3 COLD)
(SHAPE P4 CIRCULAR)
(SURFACE-CONDITION P4 SMOOTH)
(PAINTED P4 BLUE)
(TEMPERATURE P4 COLD)
(SHAPE P5 CYLINDRICAL)
(SURFACE-CONDITION P5 ROUGH)
(HAS-HOLE P5 THREE BACK)
(TEMPERATURE P5 COLD)
(SHAPE P6 CIRCULAR)
(SURFACE-CONDITION P6 POLISHED)
(PAINTED P6 BLUE)
(TEMPERATURE P6 COLD)
(SHAPE P7 CIRCULAR)
(SURFACE-CONDITION P7 SMOOTH)
(TEMPERATURE P7 COLD)
(SHAPE P8 CIRCULAR)
(SURFACE-CONDITION P8 ROUGH)
(PAINTED P8 BLUE)
(TEMPERATURE P8 COLD)
(SHAPE P9 CYLINDRICAL)
(SURFACE-CONDITION P9 ROUGH)
(TEMPERATURE P9 COLD)
)
(:goal
(and
(SURFACE-CONDITION P0 SMOOTH)
(PAINTED P0 BLUE)
(SHAPE P1 CYLINDRICAL)
(HAS-HOLE P1 TWO BACK)
(SURFACE-CONDITION P2 SMOOTH)
(SURFACE-CONDITION P3 SMOOTH)
(PAINTED P3 BLUE)
(HAS-HOLE P3 TWO FRONT)
(PAINTED P4 YELLOW)
(HAS-HOLE P4 ONE FRONT)
(SHAPE P5 CYLINDRICAL)
(SURFACE-CONDITION P5 POLISHED)
(HAS-HOLE P5 ONE FRONT)
(HAS-HOLE P6 ONE FRONT)
(SHAPE P7 CYLINDRICAL)
(SURFACE-CONDITION P7 POLISHED)
(PAINTED P7 YELLOW)
(HAS-HOLE P7 TWO BACK)
(PAINTED P8 YELLOW)
(SURFACE-CONDITION P9 ROUGH)
)
)
)

