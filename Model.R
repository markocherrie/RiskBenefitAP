
OptimalMPA<-function(MPAduration, BackPM){
  
# static params
InhalSleep=0.3
InhalRest=0.6
InhalMPA=3

ConcFactor=1.5

MortalityRiskRatio=1.089

# These are the params to calculate the mortality risk ratio for 
# physical activity benefit
B0 = 0.0009438848 
B1 = -0.0357964234
B2 = 0.0008082915 
B3 = -0.0000055871
  
# eq 1
#MPAduration = 112 - WeekRestDur

# eq 2
ExposurePM = ConcFactor * BackPM

# eq 3
InhaledDoseWithoutOutdoorMPA = 
  (InhalSleep * 56 * BackPM) +
  (InhalRest  * 112 * BackPM)

# eq 4
InhaledDoseWithOutdoorMPA =
  (InhalSleep * 56 * BackPM) +
  (InhalRest  * (112 - MPAduration) * BackPM) +
  (InhalMPA   * MPAduration * ExposurePM)

# Eq 5
IncreasePM = ((InhaledDoseWithOutdoorMPA/InhaledDoseWithoutOutdoorMPA-1))*BackPM

# eq 6
MortalityRiskRatioPM = exp(log(MortalityRiskRatio)*(IncreasePM/10))

# eq 7 
MortalityRiskRatioMPA = exp(B0 + (B1*MPAduration) + (B2*MPAduration^2) + (B3*MPAduration^3))

# eq8 
MortalityRiskRatioOver = MortalityRiskRatioMPA * MortalityRiskRatioPM 

return(MortalityRiskRatioOver)

}


OptimalMPA(11, 45)
OptimalMPA(19, 20)
OptimalMPA(20, 20)
OptimalMPA(20, 40)
OptimalMPA(20, 80)


graphdata<-NULL

for(i in seq(2:501)){
opdf<-optimize(OptimalMPA, c(0, 40), tol = 0.01,  BackPM = i)
opout<-data.frame(PM=i, minimum=opdf$minimum, objective=opdf$objective)
graphdata<-rbind(graphdata, opout)
}



# minimum is the MPAduration thats been optimised 
# objective is the MortalityRiskRatioOver level
