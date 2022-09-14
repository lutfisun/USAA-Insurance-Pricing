#Script Name: PurePremiumModel
#Created By: Michael McPhail
#Creation Date: 9/25/2015
#Last Updated By: Courtney Rohde
#Updated on: 9/4/2017

#Housekeeping
rm(list=ls())
cat("\014") 

#Install and load tweedie & statmod packages
library(tweedie)
library(statmod)
library(broom)
library(plyr)

#Read in data
ppdata_simple <- read.csv("ppdata_train.csv")

glm_simple <- glm(PurePrem ~  Gender + RatingArea + NCD + ProtectedNCD + DrivingRestriction + as.factor(VehicleAge), 
                  data = ppdata,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)
anova(glm, glm_simple, test="Chisq")

ppdata_simple$VehicleAge <- as.numeric(ppdata_simple$VehicleAge)
# test the glm against this simplification
glm_simple <- glm(PurePrem ~  Gender + RatingArea + NCD + ProtectedNCD + DrivingRestriction + VehicleAge, 
                  data = ppdata_simple,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)
anova(glm, glm_simple, test="Chisq")

#this revaluing gives an error
revalue(ppdata_simple$NCD, c("4+" = "4")) -> ppdata_simple$NCD
# test the glm against this simplification
glm_simple <- glm(PurePrem ~  Gender + RatingArea + NCD + ProtectedNCD + DrivingRestriction + VehicleAge, 
                  data = ppdata_simple,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)
anova(glm, glm_simple, test="Chisq")

ppdata_simple$NCD <- as.numeric(ppdata_simple$NCD)
# test the glm against this simplification
glm_simple <- glm(PurePrem ~  Gender + RatingArea + NCD + ProtectedNCD + DrivingRestriction + VehicleAge, 
                  data = ppdata_simple,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)
anova(glm, glm_simple, test="Chisq")


revalue(ppdata_simple$NCD, c("Unknown" = "3.5")) -> ppdata_simple$NCD
# test the glm against this simplification
glm_simple <- glm(PurePrem ~  Gender + RatingArea + NCD + ProtectedNCD + DrivingRestriction + VehicleAge, 
                  data = ppdata_simple,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)
anova(glm, glm_simple, test="Chisq")

levels(ppdata_simple$NCD)


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




#calculate new base rate
#rate change = 2.1%

#sum the old factors, sum the new factors (?seems strange but okay), multiply by base rates, plug into formula
sumOld = sum(glmcoeffs$x)
sumNew = sum(glm_simple_coeffs$x)
#3.1 might supposed to be 1.021 idk
num = (3.1)*(sumOld * 500)
newBase = num/sumNew
#bro why is the new base rate negative..... where should I put the absolute value within the calculation?



# another simplification
glm_gender <- glm(PurePrem ~  RatingArea + NCD + ProtectedNCD + DrivingRestriction + VehicleAge, 
                  data = ppdata_simple,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)
glm_RatingArea <- glm(PurePrem ~  Gender + NCD + ProtectedNCD + DrivingRestriction + VehicleAge, 
                  data = ppdata_simple,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)
glm_protected <- glm(PurePrem ~  Gender + RatingArea + NCD + DrivingRestriction + VehicleAge, 
                  data = ppdata_simple,family=tweedie(var.power= 1.487755, link.power=0), weights=Exposures)


anova(glm_simple, glm_protected, test="Chisq")


#the following isn't really working
xhp <- seq(-2, 10, 0.1)
plot(xhp, glm_simple_coeffs$x)


library(ggplot2)
theme_set(theme_bw())

### Rating Areas

Rating_Area <- glm_simple_coeffs$names[3:12]
Coefficient_RA <- glm_simple_coeffs$x[3:12]

rating_table <- data.frame("RatingYo" = 1:10, "Rating Area" = Rating_Area, "Coeff" = Coefficient_RA)

# Draw plot
ggplot(rating_table, aes(x=Rating_Area, y= Coefficient_RA)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Rating Area Coefficients") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

### 

NCD <- glm_simple_coeffs$names[13:17]
Coefficient_NCD <- glm_simple_coeffs$x[13:17]
NCD_exp <-glm_simple_coeffs$exp[13:17]

NCD_table <- data.frame("NCDyo" = 1:5, "NCD" = NCD, "Coeff" = Coefficient_NCD, "exp" = NCD_exp)

# Draw plot
ggplot(NCD_table, aes(x=NCD, y= Coefficient_NCD)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="NCD Coefficients") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Sophie's attempt to draw plot for exponential
ggplot(NCD_table, aes(x=NCD, y= NCD_exp)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="NCD Exp Coefficients") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#all factors plot
factors <- glm_simple_coeffs$names[2:34]
coefficients <- glm_simple_coeffs$x[2:34]

coeffs_table_og <- data.frame("m" = 2:34, "Factor" = factors, "Coeff" = coefficients)

# Draw plot without intercept factor in order to better see the rest
ggplot(coeffs_table_og, aes(x=factors, y= coefficients)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Original Coefficients") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))



#simplified factors plot
x <- glm_simple_coeffs$names[2:16]
y <- glm_simple_coeffs$x[2:16]
exp <-glm_simple_coeffs$exp[2:16]

coeffs_table <- data.frame("m" = 2:16, "Factor" = x, "Coeff" = y, "exp" = exp)

# Draw plot without intercept factor in order to better see the rest
ggplot(coeffs_table, aes(x=x, y= y)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Simplified Coefficients") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
