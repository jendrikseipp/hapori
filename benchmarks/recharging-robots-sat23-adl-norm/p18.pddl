(define (problem recharging-robots-cover-robots5-areas2-1361-7142) (:domain recharging-robots)
(:init
  (= (move-cost) 1)
  (= (recharge-cost) 1)
  (= (total-cost) 0)
  (connected location-0015 location-0021)
  (connected location-0010 location-0034)
  (connected location-0027 location-0050)
  (connected location-0044 location-0045)
  (connected location-0018 location-0026)
  (connected location-0003 location-0013)
  (connected location-0040 location-0041)
  (connected location-0034 location-0037)
  (connected location-0004 location-0030)
  (connected location-0009 location-0026)
  (connected location-0011 location-0023)
  (connected location-0005 location-0037)
  (connected location-0024 location-0026)
  (connected location-0033 location-0038)
  (connected location-0013 location-0035)
  (connected location-0024 location-0044)
  (connected location-0019 location-0057)
  (connected location-0007 location-0037)
  (connected location-0016 location-0049)
  (connected location-0004 location-0023)
  (connected location-0031 location-0049)
  (connected location-0029 location-0061)
  (connected location-0010 location-0020)
  (connected location-0014 location-0051)
  (connected location-0001 location-0017)
  (connected location-0047 location-0048)
  (connected location-0002 location-0025)
  (connected location-0042 location-0043)
  (connected location-0004 location-0059)
  (connected location-0025 location-0036)
  (connected location-0011 location-0046)
  (connected location-0008 location-0059)
  (connected location-0030 location-0059)
  (connected location-0038 location-0039)
  (connected location-0048 location-0049)
  (connected location-0023 location-0029)
  (connected location-0009 location-0012)
  (connected location-0009 location-0021)
  (connected location-0003 location-0035)
  (connected location-0006 location-0061)
  (connected location-0027 location-0047)
  (connected location-0042 location-0045)
  (connected location-0038 location-0041)
  (connected location-0000 location-0002)
  (connected location-0005 location-0025)
  (connected location-0055 location-0056)
  (connected location-0028 location-0039)
  (connected location-0050 location-0051)
  (connected location-0016 location-0028)
  (connected location-0042 location-0056)
  (connected location-0051 location-0052)
  (connected location-0005 location-0009)
  (connected location-0020 location-0028)
  (connected location-0004 location-0011)
  (connected location-0012 location-0024)
  (connected location-0006 location-0038)
  (connected location-0048 location-0053)
  (connected location-0013 location-0053)
  (connected location-0046 location-0047)
  (connected location-0021 location-0045)
  (connected location-0028 location-0032)
  (connected location-0008 location-0029)
  (connected location-0000 location-0025)
  (connected location-0056 location-0057)
  (connected location-0022 location-0055)
  (connected location-0027 location-0051)
  (connected location-0003 location-0051)
  (connected location-0019 location-0056)
  (connected location-0052 location-0053)
  (connected location-0046 location-0049)
  (connected location-0012 location-0026)
  (connected location-0007 location-0039)
  (connected location-0004 location-0031)
  (connected location-0030 location-0031)
  (connected location-0010 location-0028)
  (connected location-0000 location-0036)
  (connected location-0017 location-0061)
  (connected location-0006 location-0033)
  (connected location-0031 location-0032)
  (connected location-0015 location-0045)
  (connected location-0033 location-0060)
  (connected location-0018 location-0041)
  (connected location-0001 location-0055)
  (connected location-0022 location-0041)
  (connected location-0054 location-0055)
  (connected location-0030 location-0033)
  (connected location-0017 location-0054)
  (connected location-0006 location-0017)
  (connected location-0060 location-0061)
  (connected location-0012 location-0021)
  (connected location-0007 location-0034)
  (connected location-0018 location-0043)
  (connected location-0015 location-0056)
  (connected location-0022 location-0043)
  (connected location-0029 location-0058)
  (connected location-0028 location-0038)
  (connected location-0054 location-0057)
  (connected location-0011 location-0031)
  (connected location-0032 location-0038)
  (connected location-0013 location-0034)
  (connected location-0050 location-0053)
  (connected location-0011 location-0049)
  (connected location-0024 location-0043)
  (connected location-0026 location-0040)
  (connected location-0010 location-0053)
  (connected location-0013 location-0052)
  (connected location-0016 location-0048)
  (connected location-0020 location-0039)
  (connected location-0010 location-0016)
  (connected location-0012 location-0044)
  (connected location-0041 location-0043)
  (connected location-0058 location-0059)
  (connected location-0007 location-0020)
  (connected location-0035 location-0036)
  (connected location-0032 location-0049)
  (connected location-0016 location-0032)
  (connected location-0015 location-0042)
  (connected location-0008 location-0058)
  (connected location-0001 location-0006)
  (connected location-0002 location-0005)
  (connected location-0019 location-0021)
  (connected location-0036 location-0037)
  (connected location-0022 location-0038)
  (connected location-0023 location-0046)
  (connected location-0006 location-0060)
  (connected location-0003 location-0052)
  (connected location-0032 location-0033)
  (connected location-0058 location-0061)
  (connected location-0059 location-0060)
  (connected location-0025 location-0037)
  (connected location-0010 location-0048)
  (connected location-0030 location-0060)
  (connected location-0007 location-0040)
  (connected location-0001 location-0054)
  (connected location-0018 location-0040)
  (connected location-0020 location-0034)
  (connected location-0048 location-0050)
  (connected location-0014 location-0027)
  (connected location-0008 location-0023)
  (connected location-0003 location-0036)
  (connected location-0015 location-0019)
  (connected location-0027 location-0048)
  (connected location-0043 location-0044)
  (connected location-0018 location-0024)
  (connected location-0001 location-0038)
  (connected location-0026 location-0037)
  (connected location-0042 location-0055)
  (connected location-0039 location-0040)
  (connected location-0034 location-0035)
  (connected location-0010 location-0013)
  (connected location-0002 location-0009)
  (connected location-0005 location-0026)
  (connected location-0021 location-0044)
  (connected location-0022 location-0042)
  (connected location-0037 location-0040)
  (connected location-0014 location-0047)
  (connected location-0023 location-0059)
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
  (battery-predecessor battery-0009 battery-0010)
  (battery-predecessor battery-0010 battery-0011)
  (battery-predecessor battery-0011 battery-0012)
  (battery-predecessor battery-0012 battery-0013)
  (battery-predecessor battery-0013 battery-0014)
  (battery-predecessor battery-0014 battery-0015)
  (battery-predecessor battery-0015 battery-0016)
  (battery-predecessor battery-0016 battery-0017)
  (battery-predecessor battery-0017 battery-0018)
  (battery-predecessor battery-0018 battery-0019)
  (battery-predecessor battery-0019 battery-0020)
  (battery-predecessor battery-0020 battery-0021)
  (battery-predecessor battery-0021 battery-0022)
  (battery-predecessor battery-0022 battery-0023)
  (battery-predecessor battery-0023 battery-0024)
  (battery-predecessor battery-0024 battery-0025)
  (battery-predecessor battery-0025 battery-0026)
  (battery-predecessor battery-0026 battery-0027)
  (battery-predecessor battery-0027 battery-0028)
  (battery-predecessor battery-0028 battery-0029)
  (battery-predecessor battery-0029 battery-0030)
  (battery-predecessor battery-0030 battery-0031)
  (battery-predecessor battery-0031 battery-0032)
  (battery-predecessor battery-0032 battery-0033)
  (battery-predecessor battery-0033 battery-0034)
  (battery-predecessor battery-0034 battery-0035)
  (battery-predecessor battery-0035 battery-0036)
  (battery-predecessor battery-0036 battery-0037)
  (battery-predecessor battery-0037 battery-0038)
  (battery-predecessor battery-0038 battery-0039)
  (battery-predecessor battery-0039 battery-0040)
  (battery-predecessor battery-0040 battery-0041)
  (battery-predecessor battery-0041 battery-0042)
  (battery-predecessor battery-0042 battery-0043)
  (battery-predecessor battery-0043 battery-0044)
  (battery-predecessor battery-0044 battery-0045)
  (at robot-00 location-0045)
  (battery robot-00 battery-0001)
  (at robot-01 location-0026)
  (battery robot-01 battery-0002)
  (at robot-02 location-0056)
  (battery robot-02 battery-0002)
  (at robot-03 location-0054)
  (battery robot-03 battery-0027)
  (at robot-04 location-0021)
  (battery robot-04 battery-0013)
  (guard-config config-00 location-0000)
  (guard-config config-00 location-0002)
  (guard-config config-00 location-0003)
  (guard-config config-00 location-0005)
  (guard-config config-00 location-0007)
  (guard-config config-00 location-0010)
  (guard-config config-00 location-0011)
  (guard-config config-00 location-0013)
  (guard-config config-00 location-0014)
  (guard-config config-00 location-0016)
  (guard-config config-00 location-0020)
  (guard-config config-00 location-0025)
  (guard-config config-00 location-0027)
  (guard-config config-00 location-0028)
  (guard-config config-00 location-0034)
  (guard-config config-00 location-0035)
  (guard-config config-00 location-0036)
  (guard-config config-00 location-0037)
  (guard-config config-00 location-0047)
  (guard-config config-00 location-0048)
  (guard-config config-00 location-0049)
  (guard-config config-00 location-0050)
  (guard-config config-00 location-0051)
  (guard-config config-00 location-0052)
  (guard-config config-00 location-0053)
  (guard-config config-01 location-0001)
  (guard-config config-01 location-0004)
  (guard-config config-01 location-0006)
  (guard-config config-01 location-0007)
  (guard-config config-01 location-0008)
  (guard-config config-01 location-0010)
  (guard-config config-01 location-0011)
  (guard-config config-01 location-0016)
  (guard-config config-01 location-0017)
  (guard-config config-01 location-0022)
  (guard-config config-01 location-0023)
  (guard-config config-01 location-0028)
  (guard-config config-01 location-0029)
  (guard-config config-01 location-0030)
  (guard-config config-01 location-0031)
  (guard-config config-01 location-0032)
  (guard-config config-01 location-0033)
  (guard-config config-01 location-0038)
  (guard-config config-01 location-0039)
  (guard-config config-01 location-0041)
  (guard-config config-01 location-0046)
  (guard-config config-01 location-0048)
  (guard-config config-01 location-0049)
  (guard-config config-01 location-0058)
  (guard-config config-01 location-0059)
  (guard-config config-01 location-0060)
  (guard-config config-01 location-0061)
)
(:goal (and (config-fullfilled config-00) (config-fullfilled config-01)))
(:metric minimize (total-cost))
)
