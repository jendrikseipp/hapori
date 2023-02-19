; woodworking task with 19 parts and 150% wood
; Machines:
;   1 highspeed-saw
;   1 glazer
;   1 grinder
;   1 immersion-varnisher
;   1 planer
;   1 saw
;   1 spray-varnisher

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    highspeed-saw0 - highspeed-saw
    glazer0 - glazer
    grinder0 - grinder
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    saw0 - saw
    spray-varnisher0 - spray-varnisher
    white green black red blue mauve - acolour
    cherry teak pine mahogany walnut - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 - part
    b0 b1 b2 b3 b4 b5 b6 b7 - board
    s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 - aboardsize
  )
  (:init
    (grind-treatment-change varnished colourfragments)
    (grind-treatment-change glazed untreated)
    (grind-treatment-change untreated untreated)
    (grind-treatment-change colourfragments untreated)
    (is-smooth smooth)
    (is-smooth verysmooth)
    (= (total-cost) 0)
    (boardsize-successor s0 s1)
    (boardsize-successor s1 s2)
    (boardsize-successor s2 s3)
    (boardsize-successor s3 s4)
    (boardsize-successor s4 s5)
    (boardsize-successor s5 s6)
    (boardsize-successor s6 s7)
    (boardsize-successor s7 s8)
    (boardsize-successor s8 s9)
    (empty highspeed-saw0)
    (has-colour glazer0 natural)
    (has-colour glazer0 white)
    (has-colour glazer0 mauve)
    (has-colour glazer0 red)
    (has-colour glazer0 green)
    (has-colour glazer0 black)
    (has-colour immersion-varnisher0 red)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 natural)
    (has-colour immersion-varnisher0 black)
    (has-colour spray-varnisher0 red)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 natural)
    (has-colour spray-varnisher0 black)
    (unused p0)
    (goalsize p0 medium)
    (= (spray-varnish-cost p0) 10)
    (= (glaze-cost p0) 15)
    (= (grind-cost p0) 30)
    (= (plane-cost p0) 20)
    (unused p1)
    (goalsize p1 medium)
    (= (spray-varnish-cost p1) 10)
    (= (glaze-cost p1) 15)
    (= (grind-cost p1) 30)
    (= (plane-cost p1) 20)
    (unused p2)
    (goalsize p2 small)
    (= (spray-varnish-cost p2) 5)
    (= (glaze-cost p2) 10)
    (= (grind-cost p2) 15)
    (= (plane-cost p2) 10)
    (unused p3)
    (goalsize p3 medium)
    (= (spray-varnish-cost p3) 10)
    (= (glaze-cost p3) 15)
    (= (grind-cost p3) 30)
    (= (plane-cost p3) 20)
    (unused p4)
    (goalsize p4 large)
    (= (spray-varnish-cost p4) 15)
    (= (glaze-cost p4) 20)
    (= (grind-cost p4) 45)
    (= (plane-cost p4) 30)
    (unused p5)
    (goalsize p5 small)
    (= (spray-varnish-cost p5) 5)
    (= (glaze-cost p5) 10)
    (= (grind-cost p5) 15)
    (= (plane-cost p5) 10)
    (unused p6)
    (goalsize p6 medium)
    (= (spray-varnish-cost p6) 10)
    (= (glaze-cost p6) 15)
    (= (grind-cost p6) 30)
    (= (plane-cost p6) 20)
    (available p7)
    (treatment p7 colourfragments)
    (surface-condition p7 smooth)
    (wood p7 walnut)
    (colour p7 mauve)
    (goalsize p7 medium)
    (= (spray-varnish-cost p7) 10)
    (= (glaze-cost p7) 15)
    (= (grind-cost p7) 30)
    (= (plane-cost p7) 20)
    (unused p8)
    (goalsize p8 medium)
    (= (spray-varnish-cost p8) 10)
    (= (glaze-cost p8) 15)
    (= (grind-cost p8) 30)
    (= (plane-cost p8) 20)
    (unused p9)
    (goalsize p9 small)
    (= (spray-varnish-cost p9) 5)
    (= (glaze-cost p9) 10)
    (= (grind-cost p9) 15)
    (= (plane-cost p9) 10)
    (unused p10)
    (goalsize p10 medium)
    (= (spray-varnish-cost p10) 10)
    (= (glaze-cost p10) 15)
    (= (grind-cost p10) 30)
    (= (plane-cost p10) 20)
    (unused p11)
    (goalsize p11 medium)
    (= (spray-varnish-cost p11) 10)
    (= (glaze-cost p11) 15)
    (= (grind-cost p11) 30)
    (= (plane-cost p11) 20)
    (unused p12)
    (goalsize p12 small)
    (= (spray-varnish-cost p12) 5)
    (= (glaze-cost p12) 10)
    (= (grind-cost p12) 15)
    (= (plane-cost p12) 10)
    (unused p13)
    (goalsize p13 large)
    (= (spray-varnish-cost p13) 15)
    (= (glaze-cost p13) 20)
    (= (grind-cost p13) 45)
    (= (plane-cost p13) 30)
    (unused p14)
    (goalsize p14 large)
    (= (spray-varnish-cost p14) 15)
    (= (glaze-cost p14) 20)
    (= (grind-cost p14) 45)
    (= (plane-cost p14) 30)
    (unused p15)
    (goalsize p15 medium)
    (= (spray-varnish-cost p15) 10)
    (= (glaze-cost p15) 15)
    (= (grind-cost p15) 30)
    (= (plane-cost p15) 20)
    (unused p16)
    (goalsize p16 small)
    (= (spray-varnish-cost p16) 5)
    (= (glaze-cost p16) 10)
    (= (grind-cost p16) 15)
    (= (plane-cost p16) 10)
    (available p17)
    (treatment p17 glazed)
    (surface-condition p17 rough)
    (wood p17 pine)
    (colour p17 mauve)
    (goalsize p17 small)
    (= (spray-varnish-cost p17) 5)
    (= (glaze-cost p17) 10)
    (= (grind-cost p17) 15)
    (= (plane-cost p17) 10)
    (unused p18)
    (goalsize p18 medium)
    (= (spray-varnish-cost p18) 10)
    (= (glaze-cost p18) 15)
    (= (grind-cost p18) 30)
    (= (plane-cost p18) 20)
    (boardsize b0 s6)
    (wood b0 pine)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s8)
    (wood b1 teak)
    (surface-condition b1 smooth)
    (available b1)
    (boardsize b2 s7)
    (wood b2 teak)
    (surface-condition b2 rough)
    (available b2)
    (boardsize b3 s6)
    (wood b3 walnut)
    (surface-condition b3 smooth)
    (available b3)
    (boardsize b4 s9)
    (wood b4 mahogany)
    (surface-condition b4 rough)
    (available b4)
    (boardsize b5 s2)
    (wood b5 mahogany)
    (surface-condition b5 smooth)
    (available b5)
    (boardsize b6 s9)
    (wood b6 cherry)
    (surface-condition b6 smooth)
    (available b6)
    (boardsize b7 s2)
    (wood b7 cherry)
    (surface-condition b7 rough)
    (available b7)
  )
  (:goal
    (and
    (available p0)
    (wood p0 pine)
    (surface-condition p0 smooth)
    (colour p0 red)
    (treatment p0 glazed)
    (available p1)
    (wood p1 teak)
    (surface-condition p1 smooth)
    (colour p1 red)
    (treatment p1 glazed)
    (available p2)
    (surface-condition p2 verysmooth)
    (colour p2 red)
    (available p3)
    (surface-condition p3 verysmooth)
    (colour p3 red)
    (available p4)
    (surface-condition p4 verysmooth)
    (colour p4 red)
    (available p5)
    (wood p5 teak)
    (treatment p5 glazed)
    (available p6)
    (surface-condition p6 smooth)
    (colour p6 white)
    (available p7)
    (wood p7 walnut)
    (colour p7 red)
    (available p8)
    (wood p8 mahogany)
    (surface-condition p8 verysmooth)
    (colour p8 red)
    (treatment p8 varnished)
    (available p9)
    (surface-condition p9 verysmooth)
    (colour p9 black)
    (available p10)
    (wood p10 teak)
    (surface-condition p10 smooth)
    (available p11)
    (wood p11 mahogany)
    (surface-condition p11 verysmooth)
    (colour p11 black)
    (treatment p11 varnished)
    (available p12)
    (wood p12 cherry)
    (colour p12 natural)
    (available p13)
    (surface-condition p13 smooth)
    (colour p13 green)
    (treatment p13 glazed)
    (available p14)
    (wood p14 cherry)
    (treatment p14 glazed)
    (available p15)
    (colour p15 mauve)
    (treatment p15 glazed)
    (available p16)
    (colour p16 mauve)
    (treatment p16 glazed)
    (available p17)
    (surface-condition p17 verysmooth)
    (colour p17 red)
    (treatment p17 glazed)
    (available p18)
    (surface-condition p18 verysmooth)
    (colour p18 white)
    )
  )
  (:metric minimize (total-cost))
)