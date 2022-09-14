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
ppdata <- read.csv("~/Desktop/Mathematical Modelling/Math Modelling Project on Insurance Pricing/Data/ppdata_train.csv")

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
write.table(glmcoeffs, file = "C:\\Users\\Courtney\\Project\\glmcoeffs.csv", sep=",")
write.table(lmcoeffs, file = "lmcoeffs.csv", sep=",")







