


(define (problem schedule-p6-s2-c4-w2-o2)
(:domain schedule)
(:objects 
    P0
    P1
    P2
    P3
    P4
    P5
 - part
    CIRCULAR
    OBLONG
 - ashape
    BLUE
    YELLOW
    RED
    BLACK
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
(HAS-PAINT IMMERSION-PAINTER YELLOW)
(HAS-PAINT SPRAY-PAINTER YELLOW)
(HAS-PAINT IMMERSION-PAINTER RED)
(HAS-PAINT SPRAY-PAINTER RED)
(HAS-PAINT IMMERSION-PAINTER BLACK)
(HAS-PAINT SPRAY-PAINTER BLACK)
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
(HAS-HOLE P0 ONE FRONT)
(TEMPERATURE P0 COLD)
(SHAPE P1 CIRCULAR)
(SURFACE-CONDITION P1 ROUGH)
(PAINTED P1 YELLOW)
(HAS-HOLE P1 ONE BACK)
(TEMPERATURE P1 COLD)
(SHAPE P2 CYLINDRICAL)
(SURFACE-CONDITION P2 POLISHED)
(HAS-HOLE P2 TWO FRONT)
(TEMPERATURE P2 COLD)
(SHAPE P3 CYLINDRICAL)
(SURFACE-CONDITION P3 ROUGH)
(TEMPERATURE P3 COLD)
(SHAPE P4 CYLINDRICAL)
(SURFACE-CONDITION P4 ROUGH)
(HAS-HOLE P4 TWO BACK)
(TEMPERATURE P4 COLD)
(SHAPE P5 CYLINDRICAL)
(SURFACE-CONDITION P5 SMOOTH)
(HAS-HOLE P5 ONE BACK)
(TEMPERATURE P5 COLD)
)
(:goal
(and
(SURFACE-CONDITION P0 ROUGH)
(HAS-HOLE P0 TWO FRONT)
(SHAPE P1 CYLINDRICAL)
(SURFACE-CONDITION P1 ROUGH)
(PAINTED P1 RED)
(SURFACE-CONDITION P2 SMOOTH)
(PAINTED P2 YELLOW)
(SURFACE-CONDITION P3 POLISHED)
(PAINTED P3 BLACK)
(SHAPE P4 CYLINDRICAL)
(PAINTED P4 RED)
(HAS-HOLE P4 ONE FRONT)
(SURFACE-CONDITION P5 SMOOTH)
(PAINTED P5 BLUE)
(HAS-HOLE P5 ONE BACK)
)
)
)


