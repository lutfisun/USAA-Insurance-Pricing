head(Busy_Data)

for (t in 1:49297) {
  if (Busy_Data$Gender[t] == "Female") {Busy_Data$GenderCurrent[t] <- 1.000}
  if (Busy_Data$Gender[t] == "Male") Busy_Data$GenderCurrent[t] <- 1.030
  if (Busy_Data$Gender[t] == "Female") Busy_Data$GenderProp[t] <- 1.000
  if (Busy_Data$Gender[t] == "Male") Busy_Data$GenderProp[t] <- 1.182
  
  if (Busy_Data$ProtectedNCD[t] == "N") Busy_Data$PRNCDCurr[t] <- 1.0
  if (Busy_Data$ProtectedNCD[t] == "Y") Busy_Data$PRNCDCurr[t] <- 0.850
  if (Busy_Data$ProtectedNCD[t] == "N") Busy_Data$PRNCDProp[t] <- 1.0
  if (Busy_Data$ProtectedNCD[t] == "Y") Busy_Data$PRNCDProp[t] <- 0.834

  if (Busy_Data$NCD[t] == 0) Busy_Data$NCDCurr[t] <- 1.000
  if (Busy_Data$NCD[t] == 1) Busy_Data$NCDCurr[t] <- 0.950
  if (Busy_Data$NCD[t] == 2) Busy_Data$NCDCurr[t] <- 0.920
  if (Busy_Data$NCD[t] == 3) Busy_Data$NCDCurr[t] <- 0.850
  if (Busy_Data$NCD[t] == 4) Busy_Data$NCDCurr[t] <- 0.750
  if (Busy_Data$NCD[t] == 3.5) Busy_Data$NCDCurr[t] <- 0.600
  if (Busy_Data$NCD[t] == 0) Busy_Data$NCDProp[t] <- 1.000
  if (Busy_Data$NCD[t] == 1) Busy_Data$NCDProp[t] <- 0.898
  if (Busy_Data$NCD[t] == 2) Busy_Data$NCDProp[t] <- 0.806
  if (Busy_Data$NCD[t] == 3) Busy_Data$NCDProp[t] <- 0.723
  if (Busy_Data$NCD[t] == 4) Busy_Data$NCDProp[t] <- 0.649
  if (Busy_Data$NCD[t] == 3.5) Busy_Data$NCDProp[t] <- 0.685
  
  if (Busy_Data$RatingArea[t] == "1A") Busy_Data$RatingAreaCurrent[t] <- 1.0
  if (Busy_Data$RatingArea[t] == "1B") Busy_Data$RatingAreaCurrent[t] <- 1.3
  if (Busy_Data$RatingArea[t] == "2A") Busy_Data$RatingAreaCurrent[t] <- 1.6
  if (Busy_Data$RatingArea[t] == "2B") Busy_Data$RatingAreaCurrent[t] <- 1.1
  if (Busy_Data$RatingArea[t] == "3A") Busy_Data$RatingAreaCurrent[t] <- 1.48
  if (Busy_Data$RatingArea[t] == "3B") Busy_Data$RatingAreaCurrent[t] <- 1.2
  if (Busy_Data$RatingArea[t] == "4A") Busy_Data$RatingAreaCurrent[t] <- 1.5
  if (Busy_Data$RatingArea[t] == "4B") Busy_Data$RatingAreaCurrent[t] <- 1.8
  if (Busy_Data$RatingArea[t] == "5A") Busy_Data$RatingAreaCurrent[t] <- 1.65
  if (Busy_Data$RatingArea[t] == "5B") Busy_Data$RatingAreaCurrent[t] <- 2.0
  if (Busy_Data$RatingArea[t] == "6A") Busy_Data$RatingAreaCurrent[t] <- 0.8
  if (Busy_Data$RatingArea[t] == "1A") Busy_Data$RatingAreaProp[t] <- 1.0
  if (Busy_Data$RatingArea[t] == "1B") Busy_Data$RatingAreaProp[t] <- 1.504
  if (Busy_Data$RatingArea[t] == "2A") Busy_Data$RatingAreaProp[t] <- 1.533
  if (Busy_Data$RatingArea[t] == "2B") Busy_Data$RatingAreaProp[t] <- 1.358
  if (Busy_Data$RatingArea[t] == "3A") Busy_Data$RatingAreaProp[t] <- 1.510
  if (Busy_Data$RatingArea[t] == "3B") Busy_Data$RatingAreaProp[t] <- 1.599
  if (Busy_Data$RatingArea[t] == "4A") Busy_Data$RatingAreaProp[t] <- 1.489
  if (Busy_Data$RatingArea[t] == "4B") Busy_Data$RatingAreaProp[t] <- 1.573
  if (Busy_Data$RatingArea[t] == "5A") Busy_Data$RatingAreaProp[t] <- 1.75
  if (Busy_Data$RatingArea[t] == "5B") Busy_Data$RatingAreaProp[t] <- 2.221
  if (Busy_Data$RatingArea[t] == "6A") Busy_Data$RatingAreaProp[t] <- 0.759
  
  if (Busy_Data$DrivingRestriction[t] == "Any") Busy_Data$DRCurr[t] <- 1.0
  if (Busy_Data$DrivingRestriction[t] == "Named") Busy_Data$DRCurr[t] <- 0.75
  if (Busy_Data$DrivingRestriction[t] == "Any") Busy_Data$DRProp[t] <- 1.0
  if (Busy_Data$DrivingRestriction[t] == "Named") Busy_Data$DRProp[t] <- 0.902
  
  if (Busy_Data$VehicleAge[t] == 0) Busy_Data$VACurr[t] <- 1.000
  if (Busy_Data$VehicleAge[t] == 1) Busy_Data$VACurr[t] <- 0.850
  if (Busy_Data$VehicleAge[t] == 2) Busy_Data$VACurr[t] <- 0.800
  if (Busy_Data$VehicleAge[t] == 3) Busy_Data$VACurr[t] <- 0.780
  if (Busy_Data$VehicleAge[t] == 4) Busy_Data$VACurr[t] <- 0.720
  if (Busy_Data$VehicleAge[t] == 5) Busy_Data$VACurr[t] <- 0.680
  if (Busy_Data$VehicleAge[t] == 6) Busy_Data$VACurr[t] <- 0.650
  if (Busy_Data$VehicleAge[t] == 7) Busy_Data$VACurr[t] <- 0.600
  if (Busy_Data$VehicleAge[t] == 8) Busy_Data$VACurr[t] <- 0.550
  if (Busy_Data$VehicleAge[t] == 9) Busy_Data$VACurr[t] <- 0.500
  if (Busy_Data$VehicleAge[t] == 10) Busy_Data$VACurr[t] <- 0.480
  if (Busy_Data$VehicleAge[t] == 11) Busy_Data$VACurr[t] <- 0.420
  if (Busy_Data$VehicleAge[t] == 12) Busy_Data$VACurr[t] <- 0.360
  if (Busy_Data$VehicleAge[t] == 13) Busy_Data$VACurr[t] <- 0.330
  if (Busy_Data$VehicleAge[t] == 14) Busy_Data$VACurr[t] <- 0.250
  if (Busy_Data$VehicleAge[t] == 15) Busy_Data$VACurr[t] <- 0.200

  if (Busy_Data$VehicleAge[t] == 0) Busy_Data$VAProp[t] <- 1.000
  if (Busy_Data$VehicleAge[t] == 1) Busy_Data$VAProp[t] <- 0.920
  if (Busy_Data$VehicleAge[t] == 2) Busy_Data$VAProp[t] <- 0.846
  if (Busy_Data$VehicleAge[t] == 3) Busy_Data$VAProp[t] <- 0.778
  if (Busy_Data$VehicleAge[t] == 4) Busy_Data$VAProp[t] <- 0.715
  if (Busy_Data$VehicleAge[t] == 5) Busy_Data$VAProp[t] <- 0.658
  if (Busy_Data$VehicleAge[t] == 6) Busy_Data$VAProp[t] <- 0.605
  if (Busy_Data$VehicleAge[t] == 7) Busy_Data$VAProp[t] <- 0.556
  if (Busy_Data$VehicleAge[t] == 8) Busy_Data$VAProp[t] <- 0.512
  if (Busy_Data$VehicleAge[t] == 9) Busy_Data$VAProp[t] <- 0.471
  if (Busy_Data$VehicleAge[t] == 10) Busy_Data$VAProp[t] <- 0.433
  if (Busy_Data$VehicleAge[t] == 11) Busy_Data$VAProp[t] <- 0.398
  if (Busy_Data$VehicleAge[t] == 12) Busy_Data$VAProp[t] <- 0.366
  if (Busy_Data$VehicleAge[t] == 13) Busy_Data$VAProp[t] <- 0.337
  if (Busy_Data$VehicleAge[t] == 14) Busy_Data$VAProp[t] <- 0.310
  if (Busy_Data$VehicleAge[t] == 15) Busy_Data$VAProp[t] <- 0.285
}

for (t in 1:49297) {
  Busy_Data$CurrPrem[t] <- 500*(Busy_Data$GenderCurrent[t] * Busy_Data$RatingAreaCurrent[t] * 
                                  Busy_Data$NCDCurr[t] * Busy_Data$PRNCDCurr[t] * Busy_Data$DRCurr[t] *
                                  Busy_Data$VACurr[t])
  Busy_Data$PropPremium[t] <- 500*(Busy_Data$GenderProp[t] * Busy_Data$RatingAreaProp[t] * 
                                     Busy_Data$NCDProp[t] * Busy_Data$PRNCDProp[t] * Busy_Data$DRProp[t] * 
                                     Busy_Data$VAProp[t])
}
names(Busy_Data)
attach(Busy_Data)

oldSum = sum(Busy_Data$CurrPrem)
newSum = sum(Busy_Data$PropPremium)

rate_change = 0.021

new_Base = (1+rate_change)*500*oldSum/newSum
Busy_Data$X25[2] = new_Base

?write_csv
write.table(Busy_Data, file = "Busy_Data.csv", sep=",")
