(define (problem recharge-single-source-move-to-locations-651) (:domain recharging-robots)
(:init
  (= (move-cost) 1)
  (= (recharge-cost) 1)
  (= (total-cost) 0)
  (connected location-0003 location-0013)
  (connected location-0012 location-0025)
  (connected location-0009 location-0017)
  (connected location-0009 location-0026)
  (connected location-0018 location-0019)
  (connected location-0014 location-0015)
  (connected location-0000 location-0016)
  (connected location-0010 location-0020)
  (connected location-0015 location-0016)
  (connected location-0002 location-0025)
  (connected location-0018 location-0021)
  (connected location-0014 location-0017)
  (connected location-0009 location-0021)
  (connected location-0016 location-0017)
  (connected location-0012 location-0013)
  (connected location-0003 location-0010)
  (connected location-0022 location-0023)
  (connected location-0014 location-0028)
  (connected location-0016 location-0019)
  (connected location-0022 location-0025)
  (connected location-0023 location-0024)
  (connected location-0003 location-0021)
  (connected location-0019 location-0020)
  (connected location-0002 location-0013)
  (connected location-0024 location-0025)
  (connected location-0001 location-0023)
  (connected location-0011 location-0025)
  (connected location-0003 location-0005)
  (connected location-0020 location-0021)
  (connected location-0006 location-0015)
  (connected location-0026 location-0027)
  (connected location-0015 location-0027)
  (connected location-0005 location-0013)
  (connected location-0017 location-0018)
  (connected location-0027 location-0028)
  (connected location-0000 location-0011)
  (connected location-0010 location-0021)
  (connected location-0011 location-0020)
  (connected location-0000 location-0020)
  (connected location-0026 location-0029)
  (connected location-0007 location-0025)
  (connected location-0004 location-0008)
  (connected location-0017 location-0029)
  (connected location-0028 location-0029)
  (connected location-0000 location-0022)
  (connected location-0011 location-0022)
  (connected location-0009 location-0018)
  (connected location-0000 location-0015)
  (connected location-0002 location-0012)
  (connected location-0003 location-0004)
  (connected location-0004 location-0021)
  (connected location-0001 location-0006)
  (connected location-0009 location-0029)
  (connected location-0004 location-0005)
  (connected location-0000 location-0001)
  (connected location-0010 location-0011)
  (connected location-0014 location-0027)
  (connected location-0002 location-0007)
  (connected location-0000 location-0019)
  (connected location-0016 location-0018)
  (connected location-0007 location-0024)
  (connected location-0005 location-0008)
  (connected location-0010 location-0013)
  (connected location-0011 location-0012)
  (connected location-0017 location-0028)
  (connected location-0001 location-0022)
  (battery-predecessor battery-0000 battery-0001)
  (battery-predecessor battery-0001 battery-0002)
  (battery-predecessor battery-0002 battery-0003)
  (battery-predecessor battery-0003 battery-0004)
  (battery-predecessor battery-0004 battery-0005)
  (battery-predecessor battery-0005 battery-0006)
  (battery-predecessor battery-0006 battery-0007)
  (battery-predecessor battery-0007 battery-0008)
  (battery-predecessor battery-0008 battery-0009)
  (at robot-00 location-0027)
  (battery robot-00 battery-0008)
  (at robot-01 location-0027)
  (battery robot-01 battery-0000)
  (at robot-02 location-0027)
  (battery robot-02 battery-0001)
)
(:goal (and (at robot-00 location-0028) (at robot-01 location-0003) (at robot-02 location-0023)))
(:metric minimize (total-cost))
)
