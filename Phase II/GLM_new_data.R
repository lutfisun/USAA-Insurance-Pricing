#Script Name: PurePremiumModel
#Created By: Michael McPhail
#Creation Date: 9/25/2015
#Last Updated By: Courtney Rohde
#Updated on: 9/4/2017

#Housekeeping
rm(list=ls())

#Install and load tweedie & statmod packages
library(tweedie)
library(statmod)
library(broom)
library(plyr)

#Read in data
ppdata_new <- read.csv("Y:\\Insurance Pricing Project\\business.csv")

levels(ppdata_new$NCD)

ppdata$VehicleAge <- as.numeric(ppdata$VehicleAge)
ppdata$NCD <- as.numeric(ppdata$NCD)

revalue(ppdata_simple$NCD, c("4+" = "4")) -> ppdata_simple$NCD
revalue(ppdata_simple$NCD, c("Unknown" = "3.5")) -> ppdata_simple$NCD
#R interprets + as a function instead of a string

ppdata_new$VehicleAge <- as.factor(ppdata_new$VehicleAge) #treating vehicle age as a categorical variable


# Manually calculate Pure Premium using GLM regression coefficients

library(dummies)

ppdata_new2 <- dummy.data.frame(ppdata_new) #dummies help picking the right coefficient for the right variable.

names(ppdata_new2)

#lots of manual labor below. could have done this probably more neatly with loops but this also worked out.

ppdata_new2$CoeffBase <- glm_simple_coeffs$x[1]
ppdata_new2$CoeffMale <- glm_simple_coeffs$x[2]
ppdata_new2$CoeffRatingArea1B <- glm_simple_coeffs$x[3]
ppdata_new2$CoeffRatingArea2A <- glm_simple_coeffs$x[4]
ppdata_new2$CoeffRatingArea2B <- glm_simple_coeffs$x[5]
ppdata_new2$CoeffRatingArea3A <- glm_simple_coeffs$x[6]
ppdata_new2$CoeffRatingArea3B <- glm_simple_coeffs$x[7]
ppdata_new2$CoeffRatingArea4A <- glm_simple_coeffs$x[8]
ppdata_new2$CoeffRatingArea4B <- glm_simple_coeffs$x[9]
ppdata_new2$CoeffRatingArea5A <- glm_simple_coeffs$x[10]
ppdata_new2$CoeffRatingArea5B <- glm_simple_coeffs$x[11]
ppdata_new2$CoeffRatingArea6A <- glm_simple_coeffs$x[12]
ppdata_new2$CoeffNCD <- glm_simple_coeffs$x[13]
ppdata_new2$CoeffProtectedNCDY <- glm_simple_coeffs$x[14]
ppdata_new2$CoeffDrivingRestrictionNamed <- glm_simple_coeffs$x[15]
ppdata_new2$CoeffVehicleAge <- glm_simple_coeffs$x[16]


attach(ppdata_new2)
ppdata_new$ManualFit <- ( 500 * 
                        exp(CoeffMale * GenderMale ) *
                        exp(CoeffRatingArea1B * RatingArea1B ) * 
                        exp(CoeffRatingArea2A * RatingArea2A ) *
                        exp(CoeffRatingArea2B * RatingArea2B ) *
                        exp(CoeffRatingArea3A * RatingArea3A ) *
                        exp(CoeffRatingArea3B * RatingArea3B ) *
                        exp(CoeffRatingArea4A * RatingArea4A ) *
                        exp(CoeffRatingArea4B * RatingArea4B ) *
                        exp(CoeffRatingArea5A * RatingArea5A ) *
                        exp(CoeffRatingArea5B * RatingArea5B ) *
                        exp(CoeffRatingArea6A * RatingArea6A ) *
                        exp(ppdata_new2$CoeffNCD * NCD0 * 0) *
                        exp(ppdata_new2$CoeffNCD * NCD1 * 1) *
                        exp(ppdata_new2$CoeffNCD * NCD2 * 2) *
                        exp(ppdata_new2$CoeffNCD * NCD3 * 3) *
                        exp(ppdata_new2$CoeffNCD * NCD4 * 4) *
                        exp(ppdata_new2$CoeffNCD * NCDUnknown * 3.5) *
                        exp(CoeffProtectedNCDY * ProtectedNCDY ) *
                        exp(CoeffDrivingRestrictionNamed * DrivingRestrictionNamed ) *
                        exp(ppdata_new2$CoeffVehicleAge* VehicleAge1 ) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge2 * 2) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge3 * 3) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge4 * 4) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge5 * 5) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge6 * 6) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge7 * 7) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge8 * 8) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge9 * 9) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge10 * 10) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge11 * 11) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge12 * 12) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge13 * 13) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge14 * 14) *
                        exp(ppdata_new2$CoeffVehicleAge * VehicleAge15 * 15)
)

ppdata_new2$CoeffBase <- lmcoeffs$x[1]
ppdata_new2$CoeffMale <- lmcoeffs$x[2]
ppdata_new2$CoeffRatingArea1B <- lmcoeffs$x[3]
ppdata_new2$CoeffRatingArea2A <- lmcoeffs$x[4]
ppdata_new2$CoeffRatingArea2B <- lmcoeffs$x[5]
ppdata_new2$CoeffRatingArea3A <- lmcoeffs$x[6]
ppdata_new2$CoeffRatingArea3B <- lmcoeffs$x[7]
ppdata_new2$CoeffRatingArea4A <- lmcoeffs$x[8]
ppdata_new2$CoeffRatingArea4B <- lmcoeffs$x[9]
ppdata_new2$CoeffRatingArea5A <- lmcoeffs$x[10]
ppdata_new2$CoeffRatingArea5B <- lmcoeffs$x[11]
ppdata_new2$CoeffRatingArea6A <- lmcoeffs$x[12]
ppdata_new2$CoeffNCD1 <- lmcoeffs$x[13]
ppdata_new2$CoeffNCD2 <- lmcoeffs$x[14]
ppdata_new2$CoeffNCD3 <- lmcoeffs$x[15]
ppdata_new2$CoeffNCD4 <- lmcoeffs$x[16]
ppdata_new2$CoeffNCDUnknown <- lmcoeffs$x[17]
ppdata_new2$CoeffProtectedNCDY <- lmcoeffs$x[18]
ppdata_new2$CoeffDrivingRestrictionNamed <- lmcoeffs$x[19]
ppdata_new2$CoeffVehicleAge1 <- lmcoeffs$x[20]
ppdata_new2$CoeffVehicleAge2 <- lmcoeffs$x[21]
ppdata_new2$CoeffVehicleAge3 <- lmcoeffs$x[22]
ppdata_new2$CoeffVehicleAge4 <- lmcoeffs$x[23]
ppdata_new2$CoeffVehicleAge5 <- lmcoeffs$x[24]
ppdata_new2$CoeffVehicleAge6 <- lmcoeffs$x[25]
ppdata_new2$CoeffVehicleAge7 <- lmcoeffs$x[26]
ppdata_new2$CoeffVehicleAge8 <- lmcoeffs$x[27]
ppdata_new2$CoeffVehicleAge9 <- lmcoeffs$x[28]
ppdata_new2$CoeffVehicleAge10 <- lmcoeffs$x[29]
ppdata_new2$CoeffVehicleAge11 <- lmcoeffs$x[30]
ppdata_new2$CoeffVehicleAge12 <- lmcoeffs$x[31]
ppdata_new2$CoeffVehicleAge13 <- lmcoeffs$x[32]
ppdata_new2$CoeffVehicleAge14 <- lmcoeffs$x[33]
ppdata_new2$CoeffVehicleAge15 <- lmcoeffs$x[34]

#attach(ppdata_new2)
ppdata_new$ManualFitLM <- ( (CoeffBase) + 
                          (CoeffMale * GenderMale ) +
                          (CoeffRatingArea1B * RatingArea1B ) + 
                          (CoeffRatingArea2A * RatingArea2A ) +
                          (CoeffRatingArea2B * RatingArea2B ) +
                          (CoeffRatingArea3A * RatingArea3A ) +
                          (CoeffRatingArea3B * RatingArea3B ) +
                          (CoeffRatingArea4A * RatingArea4A ) +
                          (CoeffRatingArea4B * RatingArea4B ) +
                          (CoeffRatingArea5A * RatingArea5A ) +
                          (CoeffRatingArea5B * RatingArea5B ) +
                          (CoeffRatingArea6A * RatingArea6A ) +
                          (CoeffNCD1 * NCD1 ) +
                          (CoeffNCD2 * NCD2 ) +
                          (CoeffNCD3 * NCD3 ) +
                          (CoeffNCD4 * NCD4 ) +
                          (CoeffNCDUnknown * NCDUnknown ) +
                          (CoeffProtectedNCDY * ProtectedNCDY ) +
                          (CoeffDrivingRestrictionNamed * DrivingRestrictionNamed ) +
                          (CoeffVehicleAge1 * VehicleAge1 ) +
                          (CoeffVehicleAge2 * VehicleAge2 ) +
                          (CoeffVehicleAge3 * VehicleAge3 ) +
                          (CoeffVehicleAge4 * VehicleAge4 ) +
                          (CoeffVehicleAge5 * VehicleAge5 ) +
                          (CoeffVehicleAge6 * VehicleAge6 ) +
                          (CoeffVehicleAge7 * VehicleAge7 ) +
                          (CoeffVehicleAge8 * VehicleAge8 ) +
                          (CoeffVehicleAge9 * VehicleAge9 ) +
                          (CoeffVehicleAge10 * VehicleAge10 ) +
                          (CoeffVehicleAge11 * VehicleAge11 ) +
                          (CoeffVehicleAge12 * VehicleAge12 ) +
                          (CoeffVehicleAge13 * VehicleAge13 ) +
                          (CoeffVehicleAge14 * VehicleAge14 ) +
                          (CoeffVehicleAge15 * VehicleAge15 )
)


ppdata_new$AutoFit <- fitted(glm)
ppdata_new$AutoFitLM <- fitted(lm)



#can manually check that they give the same result
mean((ppdata_new$AutoFit - ppdata_new$ManualFit)**2) # 4.147563e-23 is an extremely minimal difference and probably due to rounding.

#install.packages("xlsx")
#library(xlsx)
#write.xlsx(ppdata_new, "Y:\\Insurance Pricing Project\\Phase1Deliverable.xlsx", sheetName = "Fitted Values", sheetName = "Model Coefficients GLM", sheetName = "Model Coefficients (lm)", append = TRUE, row.names = FALSE)
write.csv(ppdata_new, "Y:\\Insurance Pricing Project\\Phase1Deliverable.csv")