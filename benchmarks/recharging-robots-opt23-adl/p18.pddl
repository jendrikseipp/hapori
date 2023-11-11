;; Genearated with: ../../../domain-recharging-robots/generator.py covers --random-seed 1361 5 3 1 8 30 1 p18.pddl p18.plan
;; Random seed: 1361
(define (problem recharging-robots-cover-robots5-areas1-1361-9561)
(:domain recharging-robots)
(:objects
  location-0000 location-0001 location-0002 location-0003 location-0004 location-0005 location-0006 location-0007 location-0008 location-0009 location-0010 location-0011 location-0012 location-0013 location-0014 location-0015 location-0016 location-0017 location-0018 location-0019 location-0020 location-0021 location-0022 location-0023 location-0024 location-0025 location-0026 location-0027 location-0028 location-0029 location-0030 location-0031 location-0032 location-0033 location-0034 location-0035 location-0036 location-0037 location-0038 location-0039 location-0040 location-0041 location-0042 location-0043 location-0044 location-0045 location-0046 location-0047 location-0048 location-0049 location-0050 location-0051 location-0052 location-0053 location-0054 location-0055 location-0056 location-0057 location-0058 location-0059 location-0060 location-0061 - location
  robot-00 robot-01 robot-02 robot-03 robot-04 - robot
  battery-0000 battery-0001 battery-0002 battery-0003 battery-0004 battery-0005 battery-0006 battery-0007 battery-0008 battery-0009 battery-0010 battery-0011 battery-0012 battery-0013 battery-0014 battery-0015 battery-0016 battery-0017 battery-0018 battery-0019 battery-0020 - battery-level
  config-00 - config
)
(:init
  (= (move-cost) 1)
  (= (recharge-cost) 1)
  (= (total-cost) 0)
  (CONNECTED location-0015 location-0021)
  (CONNECTED location-0010 location-0034)
  (CONNECTED location-0027 location-0050)
  (CONNECTED location-0044 location-0045)
  (CONNECTED location-0018 location-0026)
  (CONNECTED location-0003 location-0013)
  (CONNECTED location-0040 location-0041)
  (CONNECTED location-0034 location-0037)
  (CONNECTED location-0004 location-0030)
  (CONNECTED location-0009 location-0026)
  (CONNECTED location-0011 location-0023)
  (CONNECTED location-0005 location-0037)
  (CONNECTED location-0024 location-0026)
  (CONNECTED location-0033 location-0038)
  (CONNECTED location-0013 location-0035)
  (CONNECTED location-0024 location-0044)
  (CONNECTED location-0019 location-0057)
  (CONNECTED location-0007 location-0037)
  (CONNECTED location-0016 location-0049)
  (CONNECTED location-0004 location-0023)
  (CONNECTED location-0031 location-0049)
  (CONNECTED location-0029 location-0061)
  (CONNECTED location-0010 location-0020)
  (CONNECTED location-0014 location-0051)
  (CONNECTED location-0001 location-0017)
  (CONNECTED location-0047 location-0048)
  (CONNECTED location-0002 location-0025)
  (CONNECTED location-0042 location-0043)
  (CONNECTED location-0004 location-0059)
  (CONNECTED location-0025 location-0036)
  (CONNECTED location-0011 location-0046)
  (CONNECTED location-0008 location-0059)
  (CONNECTED location-0030 location-0059)
  (CONNECTED location-0038 location-0039)
  (CONNECTED location-0048 location-0049)
  (CONNECTED location-0023 location-0029)
  (CONNECTED location-0009 location-0012)
  (CONNECTED location-0009 location-0021)
  (CONNECTED location-0003 location-0035)
  (CONNECTED location-0006 location-0061)
  (CONNECTED location-0027 location-0047)
  (CONNECTED location-0042 location-0045)
  (CONNECTED location-0038 location-0041)
  (CONNECTED location-0000 location-0002)
  (CONNECTED location-0005 location-0025)
  (CONNECTED location-0055 location-0056)
  (CONNECTED location-0028 location-0039)
  (CONNECTED location-0050 location-0051)
  (CONNECTED location-0016 location-0028)
  (CONNECTED location-0042 location-0056)
  (CONNECTED location-0051 location-0052)
  (CONNECTED location-0005 location-0009)
  (CONNECTED location-0020 location-0028)
  (CONNECTED location-0004 location-0011)
  (CONNECTED location-0012 location-0024)
  (CONNECTED location-0006 location-0038)
  (CONNECTED location-0048 location-0053)
  (CONNECTED location-0013 location-0053)
  (CONNECTED location-0046 location-0047)
  (CONNECTED location-0021 location-0045)
  (CONNECTED location-0028 location-0032)
  (CONNECTED location-0008 location-0029)
  (CONNECTED location-0000 location-0025)
  (CONNECTED location-0056 location-0057)
  (CONNECTED location-0022 location-0055)
  (CONNECTED location-0027 location-0051)
  (CONNECTED location-0003 location-0051)
  (CONNECTED location-0019 location-0056)
  (CONNECTED location-0052 location-0053)
  (CONNECTED location-0046 location-0049)
  (CONNECTED location-0012 location-0026)
  (CONNECTED location-0007 location-0039)
  (CONNECTED location-0004 location-0031)
  (CONNECTED location-0030 location-0031)
  (CONNECTED location-0010 location-0028)
  (CONNECTED location-0000 location-0036)
  (CONNECTED location-0017 location-0061)
  (CONNECTED location-0006 location-0033)
  (CONNECTED location-0031 location-0032)
  (CONNECTED location-0015 location-0045)
  (CONNECTED location-0033 location-0060)
  (CONNECTED location-0018 location-0041)
  (CONNECTED location-0001 location-0055)
  (CONNECTED location-0022 location-0041)
  (CONNECTED location-0054 location-0055)
  (CONNECTED location-0030 location-0033)
  (CONNECTED location-0017 location-0054)
  (CONNECTED location-0006 location-0017)
  (CONNECTED location-0060 location-0061)
  (CONNECTED location-0012 location-0021)
  (CONNECTED location-0007 location-0034)
  (CONNECTED location-0018 location-0043)
  (CONNECTED location-0015 location-0056)
  (CONNECTED location-0022 location-0043)
  (CONNECTED location-0029 location-0058)
  (CONNECTED location-0028 location-0038)
  (CONNECTED location-0054 location-0057)
  (CONNECTED location-0011 location-0031)
  (CONNECTED location-0032 location-0038)
  (CONNECTED location-0013 location-0034)
  (CONNECTED location-0050 location-0053)
  (CONNECTED location-0011 location-0049)
  (CONNECTED location-0024 location-0043)
  (CONNECTED location-0026 location-0040)
  (CONNECTED location-0010 location-0053)
  (CONNECTED location-0013 location-0052)
  (CONNECTED location-0016 location-0048)
  (CONNECTED location-0020 location-0039)
  (CONNECTED location-0010 location-0016)
  (CONNECTED location-0012 location-0044)
  (CONNECTED location-0041 location-0043)
  (CONNECTED location-0058 location-0059)
  (CONNECTED location-0007 location-0020)
  (CONNECTED location-0035 location-0036)
  (CONNECTED location-0032 location-0049)
  (CONNECTED location-0016 location-0032)
  (CONNECTED location-0015 location-0042)
  (CONNECTED location-0008 location-0058)
  (CONNECTED location-0001 location-0006)
  (CONNECTED location-0002 location-0005)
  (CONNECTED location-0019 location-0021)
  (CONNECTED location-0036 location-0037)
  (CONNECTED location-0022 location-0038)
  (CONNECTED location-0023 location-0046)
  (CONNECTED location-0006 location-0060)
  (CONNECTED location-0003 location-0052)
  (CONNECTED location-0032 location-0033)
  (CONNECTED location-0058 location-0061)
  (CONNECTED location-0059 location-0060)
  (CONNECTED location-0025 location-0037)
  (CONNECTED location-0010 location-0048)
  (CONNECTED location-0030 location-0060)
  (CONNECTED location-0007 location-0040)
  (CONNECTED location-0001 location-0054)
  (CONNECTED location-0018 location-0040)
  (CONNECTED location-0020 location-0034)
  (CONNECTED location-0048 location-0050)
  (CONNECTED location-0014 location-0027)
  (CONNECTED location-0008 location-0023)
  (CONNECTED location-0003 location-0036)
  (CONNECTED location-0015 location-0019)
  (CONNECTED location-0027 location-0048)
  (CONNECTED location-0043 location-0044)
  (CONNECTED location-0018 location-0024)
  (CONNECTED location-0001 location-0038)
  (CONNECTED location-0026 location-0037)
  (CONNECTED location-0042 location-0055)
  (CONNECTED location-0039 location-0040)
  (CONNECTED location-0034 location-0035)
  (CONNECTED location-0010 location-0013)
  (CONNECTED location-0002 location-0009)
  (CONNECTED location-0005 location-0026)
  (CONNECTED location-0021 location-0044)
  (CONNECTED location-0022 location-0042)
  (CONNECTED location-0037 location-0040)
  (CONNECTED location-0014 location-0047)
  (CONNECTED location-0023 location-0059)
  (CONNECTED location-0001 location-0022)

  (BATTERY-PREDECESSOR battery-0000 battery-0001)
  (BATTERY-PREDECESSOR battery-0001 battery-0002)
  (BATTERY-PREDECESSOR battery-0002 battery-0003)
  (BATTERY-PREDECESSOR battery-0003 battery-0004)
  (BATTERY-PREDECESSOR battery-0004 battery-0005)
  (BATTERY-PREDECESSOR battery-0005 battery-0006)
  (BATTERY-PREDECESSOR battery-0006 battery-0007)
  (BATTERY-PREDECESSOR battery-0007 battery-0008)
  (BATTERY-PREDECESSOR battery-0008 battery-0009)
  (BATTERY-PREDECESSOR battery-0009 battery-0010)
  (BATTERY-PREDECESSOR battery-0010 battery-0011)
  (BATTERY-PREDECESSOR battery-0011 battery-0012)
  (BATTERY-PREDECESSOR battery-0012 battery-0013)
  (BATTERY-PREDECESSOR battery-0013 battery-0014)
  (BATTERY-PREDECESSOR battery-0014 battery-0015)
  (BATTERY-PREDECESSOR battery-0015 battery-0016)
  (BATTERY-PREDECESSOR battery-0016 battery-0017)
  (BATTERY-PREDECESSOR battery-0017 battery-0018)
  (BATTERY-PREDECESSOR battery-0018 battery-0019)
  (BATTERY-PREDECESSOR battery-0019 battery-0020)

  (at robot-00 location-0001)
  (battery robot-00 battery-0002)
  (at robot-01 location-0015)
  (battery robot-01 battery-0005)
  (at robot-02 location-0025)
  (battery robot-02 battery-0004)
  (at robot-03 location-0016)
  (battery robot-03 battery-0001)
  (at robot-04 location-0028)
  (battery robot-04 battery-0008)

  (GUARD-CONFIG config-00 location-0000)
  (GUARD-CONFIG config-00 location-0003)
  (GUARD-CONFIG config-00 location-0010)
  (GUARD-CONFIG config-00 location-0013)
  (GUARD-CONFIG config-00 location-0014)
  (GUARD-CONFIG config-00 location-0027)
  (GUARD-CONFIG config-00 location-0034)
  (GUARD-CONFIG config-00 location-0035)
  (GUARD-CONFIG config-00 location-0036)
  (GUARD-CONFIG config-00 location-0048)
  (GUARD-CONFIG config-00 location-0050)
  (GUARD-CONFIG config-00 location-0051)
  (GUARD-CONFIG config-00 location-0052)
  (GUARD-CONFIG config-00 location-0053)

)
(:goal
  (and
    (config-fullfilled config-00)
  )
)
(:metric minimize (total-cost))
)

