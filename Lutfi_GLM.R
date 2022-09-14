# Lutfi Sun

#Housekeeping
rm(list=ls())

#Install and load tweedie & statmod packages
library(tweedie)
library(statmod)
library(broom)
library(plyr)

#Read in data
ppdata_simple <- read.csv("~/Desktop/Math Modelling Project on Insurance Pricing/Data/ppdata_train.csv")

levels(ppdata_simple$NCD)

revalue(ppdata_simple$NCD, c("4+" = "4")) -> ppdata_simple$NCD
revalue(ppdata_simple$NCD, c("Unknown" = "3.5")) -> ppdata_simple$NCD

levels(ppdata_simple$NCD)

ppdata_simple$NCD <- as.numeric(as.character(ppdata_simple$NCD))

View(ppdata_simple)
unique(ppdata_simple$NCD, incomparables = FALSE)

#even though we use NCD as categorical variable 4+ becomes a trouble. 
#R interprets + as a function instead of a string

#Use tweedie.profle function to maximize p (this could take 30-60 minutes). 
#Only use the next two lines after you switch to the testing dataset.
#p <- tweedie.profile(ppdata$DevelopedLoss ~ 1, p.vec = seq(1.1,1.7,0.1), do.plot=TRUE, do.ci=FALSE) 
#p$p.max

#Fit tweedie GLM with log link
glm_simple <- glm(PurePrem ~  Gender + RatingArea + NCD + ProtectedNCD + DrivingRestriction + VehicleAge, 
           data = ppdata_simple,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)
#FIT GLM WITH THE NAME glm.  Use the glm object in R with family = tweedie(var.power=p, link.power=0) and weights=Exposures
#to model PurePrem.  As we discussed earlier, make sure to use as.factor(VehicleAge) so that R does not try to fit VehicleAge with
#a curve.  You will be using all six rating factors (Gender, RatingArea, NCD, ProtectedNCD, DrivingRestriction, VehicleAge).

#Summary of tweedie glm
summary(glm_simple)

#Summary of classical linear model

#Calculate mean squared errors for both model (Which mean squared error is smaller?)
mean(resid(glm_simple)^2)

#Pull coefficients from tweedie glm
glm_simple_coeffs <- tidy(coef(glm_simple))

glm_simple_coeffs$exp <- exp(glm_simple_coeffs$x)

NCDCoeffs <- exp(glm_simple_coeffs$x[13]*c(0,1,2,3,4,3.5))
NCDCoeffs

VAgeCoeffs <- exp(glm_simple_coeffs$x[16]*c(0:15))
VAgeCoeffs
