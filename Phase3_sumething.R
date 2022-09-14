
Busy_Data$newPrem = (Busy_Data$PropPremium / 500) * new_Base
sum(CurrPrem)
sum(PropPremium)
(sum(PropPremium)-sum(CurrPrem))/sum(CurrPrem)
(sum(newPrem)-sum(CurrPrem))/sum(CurrPrem)


attach(Busy_Data)
Old_sumMale = 0
Old_sum1B = 0
Old_sum2B = 0
Old_sum3B = 0
Old_sumNamed = 0
for (t in 1:49297) {
  if (Busy_Data$Gender[t] == "Male") Old_sumMale = Old_sumMale + CurrPrem[t]
  if (Busy_Data$RatingArea[t] == "1B") Old_sum1B = Old_sum1B + CurrPrem[t]
  if (Busy_Data$RatingArea[t] == "2B") Old_sum2B = Old_sum2B + CurrPrem[t]
  if (Busy_Data$RatingArea[t] == "3B") Old_sum3B = Old_sum3B + CurrPrem[t]
  if (Busy_Data$DrivingRestriction[t] == "Named") Old_sumNamed = Old_sumNamed + CurrPrem[t]
}

New_sumMale = 0
New_sum1B = 0
New_sum2B = 0
New_sum3B = 0
New_sumNamed = 0
for (t in 1:49297) {
  if (Busy_Data$Gender[t] == "Male") New_sumMale = New_sumMale + newPrem[t]
  if (Busy_Data$RatingArea[t] == "1B") New_sum1B = New_sum1B + newPrem[t]
  if (Busy_Data$RatingArea[t] == "2B") New_sum2B = New_sum2B + newPrem[t]
  if (Busy_Data$RatingArea[t] == "3B") New_sum3B = New_sum3B + newPrem[t]
  if (Busy_Data$DrivingRestriction[t] == "Named") New_sumNamed = New_sumNamed + newPrem[t]
}

ChangeMale = (New_sumMale - Old_sumMale) / Old_sumMale
Change1B = (New_sum1B - Old_sum1B) / Old_sum1B 
Change2B = (New_sum2B - Old_sum2B) / Old_sum2B 
Change3B = (New_sum3B - Old_sum3B) / Old_sum3B 
ChangeNamed = (New_sumNamed - Old_sumNamed) / Old_sumNamed 

Changes <- data.frame(
                      "Pool" = c("Male","Ra1B","Ra2B", "Ra3B", "Named"),
                      "OldSum" = c(Old_sumMale, Old_sum1B, Old_sum2B, Old_sum3B, Old_sumNamed),
                      "NewSum" = c(New_sumMale, New_sum1B, New_sum2B, New_sum3B, New_sumNamed),
                      "Change" = c(ChangeMale, Change1B, Change2B, Change3B, ChangeNamed)
)

summary(Changes$Change)
scalingfactor = max(Changes$Change) / 0.07
Changes$ScaledChange = Changes$Change / scalingfactor
scalingfactor









