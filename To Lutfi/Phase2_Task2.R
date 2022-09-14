#Housekeeping
rm(list=ls())
cat("\014") 

#Install and load tweedie & statmod packages
library(tweedie)
library(statmod)
library(broom)
library(plyr)

#Read in data
ppdata_new <- read.csv("business.csv")



#Phase II, Task 2: calculating new current and proposed premiums
ppdata_new$GenderCurrent = 1.03
ppdata_new$GenderProp = 1.182

levels(ppdata_new$NCD)

revalue(ppdata_new$NCD, c("4+" = "4")) -> ppdata_new$NCD
revalue(ppdata_new$NCD, c("Unknown" = "3.5")) -> ppdata_new$NCD
ppdata_new$NCD <- as.numeric(ppdata_new$NCD)

library(dummies)

ppdata3 <- dummy.data.frame(ppdata_new) #dummies help picking the right coefficient for the right variable.

names(ppdata3)

#lots of manual labor below. could have done this probably more neatly with loops but this also worked out.


ppdata3$CoeffBase <- glm_simple_coeffs$x[1]
ppdata3$CoeffMale <- glm_simple_coeffs$x[2]
ppdata3$CoeffRatingArea1B <- glm_simple_coeffs$x[3]
ppdata3$CoeffRatingArea2A <- glm_simple_coeffs$x[4]
ppdata3$CoeffRatingArea2B <- glm_simple_coeffs$x[5]
ppdata3$CoeffRatingArea3A <- glm_simple_coeffs$x[6]
ppdata3$CoeffRatingArea3B <- glm_simple_coeffs$x[7]
ppdata3$CoeffRatingArea4A <- glm_simple_coeffs$x[8]
ppdata3$CoeffRatingArea4B <- glm_simple_coeffs$x[9]
ppdata3$CoeffRatingArea5A <- glm_simple_coeffs$x[10]
ppdata3$CoeffRatingArea5B <- glm_simple_coeffs$x[11]
ppdata3$CoeffRatingArea6A <- glm_simple_coeffs$x[12]
ppdata3$CoeffNCD <- glm_simple_coeffs$x[13]
ppdata3$CoeffProtectedNCDY <- glm_simple_coeffs$x[14]
ppdata3$CoeffDrivingRestrictionNamed <- glm_simple_coeffs$x[15]
ppdata3$CoeffVehicleAge <- glm_simple_coeffs$x[16]

attach(ppdata3)
names(ppdata3)

NewPrem <- ( 500 * 
                      exp(ppdata3$CoeffMale * ppdata3$GenderMale ) *
                      exp(ppdata3$CoeffRatingArea1B * ppdata3$RatingArea1B ) * 
                      exp(ppdata3$CoeffRatingArea2A * ppdata3$RatingArea2A ) *
                      exp(ppdata3$CoeffRatingArea2B * ppdata3$RatingArea2B ) *
                      exp(ppdata3$CoeffRatingArea3A * ppdata3$RatingArea3A ) *
                      exp(ppdata3$CoeffRatingArea3B * ppdata3$RatingArea3B ) *
                      exp(ppdata3$CoeffRatingArea4A * ppdata3$RatingArea4A ) *
                      exp(ppdata3$CoeffRatingArea4B * ppdata3$RatingArea4B ) *
                      exp(ppdata3$CoeffRatingArea5A * ppdata3$RatingArea5A ) *
                      exp(ppdata3$CoeffRatingArea5B * ppdata3$RatingArea5B ) *
                      exp(ppdata3$CoeffRatingArea6A * ppdata3$RatingArea6A ) *
                      exp(ppdata3$CoeffNCD * ppdata3$NCD ) *
                      exp(ppdata3$CoeffProtectedNCDY * ppdata3$ProtectedNCDY ) *
                      exp(ppdata3$CoeffDrivingRestrictionNamed * ppdata3$DrivingRestrictionNamed ) *
                      exp(ppdata3$CoeffVehicleAge * ppdata3$VehicleAge )
)

#attach(ppdata2)
meanOld = mean(ppdata$PurePrem)
meanNew = mean(ppdata$NewPrem)

write.csv(ppdata, "Y:\\Insurance Pricing Project\\Phase2Deliverable.csv")