


(define (problem schedule-p5-s2-c2-w3-o2)
(:domain schedule)
(:objects 
    P0
    P1
    P2
    P3
    P4
 - part
    CIRCULAR
    OBLONG
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
(SHAPE P0 CYLINDRICAL)
(SURFACE-CONDITION P0 ROUGH)
(PAINTED P0 BLUE)
(HAS-HOLE P0 TWO FRONT)
(TEMPERATURE P0 COLD)
(SHAPE P1 CIRCULAR)
(SURFACE-CONDITION P1 POLISHED)
(HAS-HOLE P1 THREE FRONT)
(TEMPERATURE P1 COLD)
(SHAPE P2 OBLONG)
(SURFACE-CONDITION P2 SMOOTH)
(PAINTED P2 BLUE)
(TEMPERATURE P2 COLD)
(SHAPE P3 CIRCULAR)
(SURFACE-CONDITION P3 ROUGH)
(PAINTED P3 YELLOW)
(HAS-HOLE P3 TWO FRONT)
(TEMPERATURE P3 COLD)
(SHAPE P4 OBLONG)
(SURFACE-CONDITION P4 ROUGH)
(PAINTED P4 YELLOW)
(HAS-HOLE P4 TWO BACK)
(TEMPERATURE P4 COLD)
)
(:goal
(and
(HAS-HOLE P0 THREE FRONT)
(SURFACE-CONDITION P1 ROUGH)
(HAS-HOLE P1 ONE FRONT)
(HAS-HOLE P2 ONE BACK)
(SHAPE P3 CYLINDRICAL)
(PAINTED P3 BLUE)
(HAS-HOLE P3 ONE BACK)
(SHAPE P4 CYLINDRICAL)
)
)
)


