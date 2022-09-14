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
ppdata <- read.csv("~/Desktop/Math Modelling Project on Insurance Pricing/Data/ppdata_train.csv")

View(ppdata)

levels(ppdata$NCD)

revalue(ppdata$NCD, c("4+" = "5")) -> ppdata$NCD
revalue(ppdata$NCD, c("Unknown" = NA)) -> ppdata$NCD

levels(ppdata$NCD)
ppdata$NCD <- as.numeric(ppdata$NCD)

attach(ppdata)

#Use tweedie.profle function to maximize p (this could take 30-60 minutes). 
#Only use the next two lines after you switch to the testing dataset.
#p <- tweedie.profile(ppdata$DevelopedLoss ~ 1, p.vec = seq(1.1,1.7,0.1), do.plot=TRUE, do.ci=FALSE) 
#p$p.max

#Fit tweedie GLM with log link
glm <- glm(PurePrem ~  Gender + RatingArea + NCD + ProtectedNCD + DrivingRestriction + VehicleAge, 
        data = ppdata,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)
#FIT GLM WITH THE NAME glm.  Use the glm object in R with family = tweedie(var.power=p, link.power=0) and weights=Exposures
#to model PurePrem.  As we discussed earlier, make sure to use as.factor(VehicleAge) so that R does not try to fit VehicleAge with
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

ppdata$ExpBase <- glmcoeffs$exp[1]
ppdata$ExpMale <- glmcoeffs$exp[2]
ppdata$ExpRatingArea1B <- glmcoeffs$exp[3]
ppdata$ExpRatingArea2A <- glmcoeffs$exp[4]
ppdata$ExpRatingArea2B <- glmcoeffs$exp[5]
ppdata$ExpRatingArea3A <- glmcoeffs$exp[6]
ppdata$ExpRatingArea3B <- glmcoeffs$exp[7]
ppdata$ExpRatingArea4A <- glmcoeffs$exp[8]
ppdata$ExpRatingArea4B <- glmcoeffs$exp[9]
ppdata$ExpRatingArea5A <- glmcoeffs$exp[10]
ppdata$ExpRatingArea6A <- glmcoeffs$exp[11]
ppdata$ExpNCD <- glmcoeffs$exp[12]
ppdata$ExpProtectedNCDY <- glmcoeffs$exp[13]
ppdata$ExpDrivingRestrictionNamed <- glmcoeffs$exp[14]
ppdata$ExpVehicleAge <- glmcoeffs$exp[15]

install.packages("dummies")
library(dummies)

ppdata2 <- dummy.data.frame(ppdata, sep = ".")

names(ppdata2)

ppdata2$ManualFit <- ( ppdata2$ExpBase * 
                         (ppdata2$ExpMale ** ppdata2$Gender.Male ) *
                         (ppdata2$ExpRatingArea1B ** ppdata2$RatingArea.1B ) * 
                         (ppdata2$ExpRatingArea2A ** ppdata2$RatingArea.2A ) *
                         (ppdata2$ExpRatingArea2B ** ppdata2$RatingArea.2B ) *
                         (ppdata2$ExpRatingArea3A ** ppdata2$RatingArea.3A ) *
                         (ppdata2$ExpRatingArea3B ** ppdata2$RatingArea.3B ) *
                         (ppdata2$ExpRatingArea4A ** ppdata2$RatingArea.4A ) *
                         (ppdata2$ExpRatingArea4B ** ppdata2$RatingArea.4B ) *
                         (ppdata2$ExpRatingArea5A ** ppdata2$RatingArea.5A ) *
                         (ppdata2$ExpRatingArea6A ** ppdata2$RatingArea.6A ) *
                         (ppdata2$ExpNCD ** ppdata2$NCD ) *
                         (ppdata2$ExpProtectedNCDY ** ppdata2$ProtectedNCD.Y ) *
                         (ppdata2$ExpDrivingRestrictionNamed ** ppdata2$DrivingRestriction.Named ) *
                         (ppdata2$ExpVehicleAge ** ppdata2$VehicleAge )
                         )

ppdata2$AutoFit <- fitted(glm)

ppdata$CoBase <- glmcoeffs$x[1]
ppdata$CoMale <- glmcoeffs$x[2]
ppdata$CoRatingArea1B <- glmcoeffs$x[3]
ppdata$CoRatingArea2A <- glmcoeffs$x[4]
ppdata$CoRatingArea2B <- glmcoeffs$x[5]
ppdata$CoRatingArea3A <- glmcoeffs$x[6]
ppdata$CoRatingArea3B <- glmcoeffs$x[7]
ppdata$CoRatingArea4A <- glmcoeffs$x[8]
ppdata$CoRatingArea4B <- glmcoeffs$x[9]
ppdata$CoRatingArea5A <- glmcoeffs$x[10]
ppdata$CoRatingArea6A <- glmcoeffs$x[11]
ppdata$CoNCD <- glmcoeffs$x[12]
ppdata$CoProtectedNCDY <- glmcoeffs$x[13]
ppdata$CoDrivingRestrictionNamed <- glmcoeffs$x[14]
ppdata$CoVehicleAge <- glmcoeffs$x[15]

ppdata2$ManualFit2 <- ( ppdata2$ExpBase * 
                         (ppdata2$ExpMale ** ppdata2$Gender.Male ) *
                         (ppdata2$ExpRatingArea1B ** ppdata2$RatingArea.1B ) * 
                         (ppdata2$ExpRatingArea2A ** ppdata2$RatingArea.2A ) *
                         (ppdata2$ExpRatingArea2B ** ppdata2$RatingArea.2B ) *
                         (ppdata2$ExpRatingArea3A ** ppdata2$RatingArea.3A ) *
                         (ppdata2$ExpRatingArea3B ** ppdata2$RatingArea.3B ) *
                         (ppdata2$ExpRatingArea4A ** ppdata2$RatingArea.4A ) *
                         (ppdata2$ExpRatingArea4B ** ppdata2$RatingArea.4B ) *
                         (ppdata2$ExpRatingArea5A ** ppdata2$RatingArea.5A ) *
                         (ppdata2$ExpRatingArea6A ** ppdata2$RatingArea.6A ) *
                         (ppdata2$ExpNCD ** ppdata2$NCD ) *
                         (ppdata2$ExpProtectedNCDY ** ppdata2$ProtectedNCD.Y ) *
                         (ppdata2$ExpDrivingRestrictionNamed ** ppdata2$DrivingRestriction.Named ) *
                         (ppdata2$ExpVehicleAge ** ppdata2$VehicleAge )
)

ppdata2$AutoFit <- fitted(glm)

