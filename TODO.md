1) rm dbcon ✅
2) add mapset / location
3) isolate grass session
4) Integrate SpeedMap + FrictionMap
5) Build inputSpeed + inputFrction inside module
6) Remove Input hf table, use selected id list instead

7) return path -> towardsFacilities 
8) maxSpeed -> useMaxSpeedMask + read table
9) maxCost -> maxTravelTime
10) outputCumulative -> outputTravelTime

amTravelTimeAnalysis
amCapacityAnalysis
amReferral
amScalingUp

dx = data.frame(a=c("a","b",'c'),id=c(1,2,3))
dx[c("a")] %>% order() %>% dx[., c("a", "id")]



