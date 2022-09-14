# Lutfi Sun
# trying another way

ppdata_train <- read.csv("~/Desktop/Math Modelling Project on Insurance Pricing/Data/ppdata_train.csv")
ppdata_train$VehicleAge <- as.factor(ppdata_train$VehicleAge) #treating vehicle age as a categorical variable

ppdata_train$Intercept <- glmcoeffs$x[1]
ppdata_train$Gender.Factor <- 0
ppdata_train$RatingArea.Factor <- 0
ppdata_train$NCD.Factor <- 0
ppdata_train$ProtectedNCD.Factor <- 0
ppdata_train$DrivingRestriction.Factor <- 0
ppdata_train$VehicleAge.Factor <- 0

names(ppdata_train)
levels(ppdata_train$Gender)
levels(ppdata_train$RatingArea)
levels(ppdata_train$NCD)
levels(ppdata_train$ProtectedNCD)
levels(ppdata_train$DrivingRestriction)
levels(ppdata_train$VehicleAge)


for (t in 1:8000) {
  if (ppdata_train$Gender[t] == "Male") ppdata_train$Gender.Factor[t] <- glmcoeffs$x[2]
  
  if (ppdata_train$RatingArea[t] == "1B") ppdata_train$RatingArea.Factor[t] <- glmcoeffs$x[3]
  if (ppdata_train$RatingArea[t] == "2A") ppdata_train$RatingArea.Factor[t] <- glmcoeffs$x[4]
  if (ppdata_train$RatingArea[t] == "2B") ppdata_train$RatingArea.Factor[t] <- glmcoeffs$x[5]
  if (ppdata_train$RatingArea[t] == "3A") ppdata_train$RatingArea.Factor[t] <- glmcoeffs$x[6]
  if (ppdata_train$RatingArea[t] == "3B") ppdata_train$RatingArea.Factor[t] <- glmcoeffs$x[7]
  if (ppdata_train$RatingArea[t] == "4A") ppdata_train$RatingArea.Factor[t] <- glmcoeffs$x[8]
  if (ppdata_train$RatingArea[t] == "4B") ppdata_train$RatingArea.Factor[t] <- glmcoeffs$x[9]
  if (ppdata_train$RatingArea[t] == "5A") ppdata_train$RatingArea.Factor[t] <- glmcoeffs$x[10]
  if (ppdata_train$RatingArea[t] == "5B") ppdata_train$RatingArea.Factor[t] <- glmcoeffs$x[11]
  if (ppdata_train$RatingArea[t] == "6A") ppdata_train$RatingArea.Factor[t] <- glmcoeffs$x[12]
  
  if (ppdata_train$NCD[t] == "1") ppdata_train$NCD.Factor[t] <- glmcoeffs$x[13]
  if (ppdata_train$NCD[t] == "2") ppdata_train$NCD.Factor[t] <- glmcoeffs$x[14]
  if (ppdata_train$NCD[t] == "3") ppdata_train$NCD.Factor[t] <- glmcoeffs$x[15]
  if (ppdata_train$NCD[t] == "4+") ppdata_train$NCD.Factor[t] <- glmcoeffs$x[16]
  if (ppdata_train$NCD[t] == "Unknown") ppdata_train$NCD.Factor[t] <- glmcoeffs$x[17]
  
  if (ppdata_train$ProtectedNCD[t] == "Y") ppdata_train$ProtectedNCD.Factor[t] <- glmcoeffs$x[18]
  
  if (ppdata_train$DrivingRestriction[t] == "Named") ppdata_train$DrivingRestriction.Factor[t] <- glmcoeffs$x[19]
  
  if (ppdata_train$VehicleAge[t] == "1") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[20]
  if (ppdata_train$VehicleAge[t] == "2") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[21]
  if (ppdata_train$VehicleAge[t] == "3") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[22]
  if (ppdata_train$VehicleAge[t] == "4") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[23]
  if (ppdata_train$VehicleAge[t] == "5") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[24]
  if (ppdata_train$VehicleAge[t] == "6") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[25]
  if (ppdata_train$VehicleAge[t] == "7") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[26]
  if (ppdata_train$VehicleAge[t] == "8") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[27]
  if (ppdata_train$VehicleAge[t] == "9") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[28]
  if (ppdata_train$VehicleAge[t] == "10") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[29]
  if (ppdata_train$VehicleAge[t] == "11") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[30]
  if (ppdata_train$VehicleAge[t] == "12") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[31]
  if (ppdata_train$VehicleAge[t] == "13") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[32]
  if (ppdata_train$VehicleAge[t] == "14") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[33]
  if (ppdata_train$VehicleAge[t] == "15") ppdata_train$VehicleAge.Factor[t] <- glmcoeffs$x[34]
}

names(ppdata_train)

ppdata_train$ManualFit <- ( exp(ppdata_train$Intercept) * 
                            exp(ppdata_train$Gender.Factor) *
                            exp(ppdata_train$RatingArea.Factor) * 
                            exp(ppdata_train$NCD.Factor) * 
                            exp(ppdata_train$ProtectedNCD.Factor) * 
                            exp(ppdata_train$DrivingRestriction.Factor) * 
                            exp(ppdata_train$VehicleAge.Factor)
)

mean(ppdata_train$ManualFit - ppdata$ManualFit)
ppdata_train$AutoFit <- fitted(glm)

mean(ppdata$ManualFit - ppdata$AutoFit)












