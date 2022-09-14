#Script Name: PurePremiumModel
#Created By: Michael McPhail
#Creation Date: 9/25/2015
#Last Updated By: Courtney Rohde
#Updated on: 9/4/2017

# Lutfi Sun

#Housekeeping
rm(list=ls())

#Install and load tweedie & statmod packages
library(tweedie)
library(statmod)
library(broom)
library(plyr)

#Read in data
ppdata <- read.csv("~/Desktop/Math Modelling Project on Insurance Pricing/Data/ppdata_train.csv")

levels(ppdata$NCD)

revalue(ppdata$NCD, c("4+" = "4")) -> ppdata$NCD  #even though we use NCD as categorical variable 4+ becomes a trouble. 
                                                  #R interprets + as a function instead of a string

ppdata$VehicleAge <- as.factor(ppdata$VehicleAge) #treating vehicle age as a categorical variable

#Use tweedie.profle function to maximize p (this could take 30-60 minutes). 
#Only use the next two lines after you switch to the testing dataset.
#p <- tweedie.profile(ppdata$DevelopedLoss ~ 1, p.vec = seq(1.1,1.7,0.1), do.plot=TRUE, do.ci=FALSE) 
#p$p.max


#Fit tweedie GLM with log link
glm <- glm(PurePrem ~  Gender + RatingArea + NCD + ProtectedNCD + DrivingRestriction + VehicleAge, 
           data = ppdata,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)
#Using the glm object in R with family = tweedie(var.power=p, link.power=0) 
#and weights=Exposures to model PurePrem.  


#As we discussed earlier, make sure to use as.factor(VehicleAge) so that R does not try to fit VehicleAge with
#a curve.  You will be using all six rating factors (Gender, RatingArea, NCD, ProtectedNCD, DrivingRestriction, VehicleAge).

#Fit classical linear model
lm <- lm(PurePrem ~  Gender + RatingArea + NCD + ProtectedNCD + DrivingRestriction + VehicleAge, 
         data = ppdata, weights=Exposures)
#FIT LM WITH THE NAME lm.  It will include the exact same rating factors as your glm object.  Code this exactly how you
#coded the demo

#Summary of tweedie glm
summary(glm)

#Summary of classical linear model
summary(lm)

#Calculate mean squared errors for both model (Which mean squared error is smaller?)
mean(resid(glm)^2)
mean(resid(lm)^2)

#Pull coefficients from tweedie glm
glmcoeffs <- tidy(coef(glm))
lmcoeffs <-tidy(coef(lm))

#Export coefficients for rating
write.table(lmcoeffs, file = "lmcoeffs.csv", sep=",")
write.table(glmcoeffs, file = "glmcoeffs.csv", sep=",")

glmcoeffs$exp <- exp(glmcoeffs$x)

# Manually calculate Pure Premium using GLM regression coefficients

library(dummies)

ppdata2 <- dummy.data.frame(ppdata) #dummies help picking the right coefficient for the right variable.

names(ppdata2)

#lots of manual labor below. could have done this probably more neatly with loops but this also worked out.

ppdata2$CoeffBase <- glmcoeffs$x[1]
ppdata2$CoeffMale <- glmcoeffs$x[2]
ppdata2$CoeffRatingArea1B <- glmcoeffs$x[3]
ppdata2$CoeffRatingArea2A <- glmcoeffs$x[4]
ppdata2$CoeffRatingArea2B <- glmcoeffs$x[5]
ppdata2$CoeffRatingArea3A <- glmcoeffs$x[6]
ppdata2$CoeffRatingArea3B <- glmcoeffs$x[7]
ppdata2$CoeffRatingArea4A <- glmcoeffs$x[8]
ppdata2$CoeffRatingArea4B <- glmcoeffs$x[9]
ppdata2$CoeffRatingArea5A <- glmcoeffs$x[10]
ppdata2$CoeffRatingArea5B <- glmcoeffs$x[11]
ppdata2$CoeffRatingArea6A <- glmcoeffs$x[12]
ppdata2$CoeffNCD1 <- glmcoeffs$x[13]
ppdata2$CoeffNCD2 <- glmcoeffs$x[14]
ppdata2$CoeffNCD3 <- glmcoeffs$x[15]
ppdata2$CoeffNCD4 <- glmcoeffs$x[16]
ppdata2$CoeffNCDUnknown <- glmcoeffs$x[17]
ppdata2$CoeffProtectedNCDY <- glmcoeffs$x[18]
ppdata2$CoeffDrivingRestrictionNamed <- glmcoeffs$x[19]
ppdata2$CoeffVehicleAge1 <- glmcoeffs$x[20]
ppdata2$CoeffVehicleAge2 <- glmcoeffs$x[21]
ppdata2$CoeffVehicleAge3 <- glmcoeffs$x[22]
ppdata2$CoeffVehicleAge4 <- glmcoeffs$x[23]
ppdata2$CoeffVehicleAge5 <- glmcoeffs$x[24]
ppdata2$CoeffVehicleAge6 <- glmcoeffs$x[25]
ppdata2$CoeffVehicleAge7 <- glmcoeffs$x[26]
ppdata2$CoeffVehicleAge8 <- glmcoeffs$x[27]
ppdata2$CoeffVehicleAge9 <- glmcoeffs$x[28]
ppdata2$CoeffVehicleAge10 <- glmcoeffs$x[29]
ppdata2$CoeffVehicleAge11 <- glmcoeffs$x[30]
ppdata2$CoeffVehicleAge12 <- glmcoeffs$x[31]
ppdata2$CoeffVehicleAge13 <- glmcoeffs$x[32]
ppdata2$CoeffVehicleAge14 <- glmcoeffs$x[33]
ppdata2$CoeffVehicleAge15 <- glmcoeffs$x[34]

attach(ppdata2)

ppdata$ManualFit <- ( exp(CoeffBase) * 
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
                        exp(CoeffNCD1 * NCD1 ) *
                        exp(CoeffNCD2 * NCD2 ) *
                        exp(CoeffNCD3 * NCD3 ) *
                        exp(CoeffNCD4 * NCD4 ) *
                        exp(CoeffNCDUnknown * NCDUnknown ) *
                        exp(CoeffProtectedNCDY * ProtectedNCDY ) *
                        exp(CoeffDrivingRestrictionNamed * DrivingRestrictionNamed ) *
                        exp(CoeffVehicleAge1 * VehicleAge1 ) *
                        exp(CoeffVehicleAge2 * VehicleAge2 ) *
                        exp(CoeffVehicleAge3 * VehicleAge3 ) *
                        exp(CoeffVehicleAge4 * VehicleAge4 ) *
                        exp(CoeffVehicleAge5 * VehicleAge5 ) *
                        exp(CoeffVehicleAge6 * VehicleAge6 ) *
                        exp(CoeffVehicleAge7 * VehicleAge7 ) *
                        exp(CoeffVehicleAge8 * VehicleAge8 ) *
                        exp(CoeffVehicleAge9 * VehicleAge9 ) *
                        exp(CoeffVehicleAge10 * VehicleAge10 ) *
                        exp(CoeffVehicleAge11 * VehicleAge11 ) *
                        exp(CoeffVehicleAge12 * VehicleAge12 ) *
                        exp(CoeffVehicleAge13 * VehicleAge13 ) *
                        exp(CoeffVehicleAge14 * VehicleAge14 ) *
                        exp(CoeffVehicleAge15 * VehicleAge15 )
)

ppdata$AutoFit <- fitted(glm)
ppdata$lmFit <- fitted(lm)

#can manually check that they give the same result
mean((ppdata$AutoFit - ppdata$ManualFit)**2) # 4.147563e-23 is an extremely minimal difference and probably due to rounding.
mean(ppdata$AutoFit) # = 5829.353
mean(ppdata$ManualFit) # = 5829.353

write.table(ppdata, file = "GLM_Final.csv", sep=",")
write.table(lmcoeffs, file = "lmcoeffs.csv", sep=",")
write.table(glmcoeffs, file = "glmcoeffs.csv", sep=",")

