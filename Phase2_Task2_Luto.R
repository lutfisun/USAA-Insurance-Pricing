# Lutfi Sun

#Install and load packages
library(tweedie)
library(statmod)
library(broom)
library(plyr)
library(ggplot2)
library(dummies)
library(readr)

Busy_Data <- read_csv("RatingCopyLuto.xlsx - Business Data.csv")

head(Busy_Data) #Taking a quick look at the data

unique(Busy_Data$NCD) #Checking what type NCD is and what values it takes

revalue(Busy_Data$NCD, c("4+" = "4")) -> Busy_Data$NCD
revalue(Busy_Data$NCD, c("Unknown" = "3.5")) -> Busy_Data$NCD
unique(Busy_Data$NCD) #Checking if code above worked

Busy_Data$NCD <- as.numeric(Busy_Data$NCD)
unique(Busy_Data$NCD) #Checking if code above worked

names(Busy_Data)

#I want to focus on the factors for now. So below we are making a data set with only the factors
Busy_Data2 <- with(Busy_Data, data.frame(Gender = Gender, RatingArea = RatingArea, NCD = NCD, 
                                         ProtectedNCD = ProtectedNCD, DrivingRestriction = DrivingRestriction, 
                                         VehicleAge = VehicleAge ) )

Busy_Data3 <- dummy.data.frame(Busy_Data2) #dummies help picking the right coefficient for the right variable.

head(Busy_Data3) #Taking a quick look at what we just did
# We see NCD and Vehicleage are numeric and evrything else is a dummy/binary variable just like we wanted

# Now lets calculate the pure premimums (preminums that would be able to cover only the loss claims)

Busy_Data3$CoeffBase <- 500
Busy_Data3$CoeffMale <- glm_simple_coeffs$x[2]
Busy_Data3$CoeffRA1B <- glm_simple_coeffs$x[3]
Busy_Data3$CoeffRA2A <- glm_simple_coeffs$x[4]
Busy_Data3$CoeffRA2B <- glm_simple_coeffs$x[5]
Busy_Data3$CoeffRA3A <- glm_simple_coeffs$x[6]
Busy_Data3$CoeffRA3B <- glm_simple_coeffs$x[7]
Busy_Data3$CoeffRA4A <- glm_simple_coeffs$x[8]
Busy_Data3$CoeffRA4B <- glm_simple_coeffs$x[9]
Busy_Data3$CoeffRA5A <- glm_simple_coeffs$x[10]
Busy_Data3$CoeffRA5B <- glm_simple_coeffs$x[11]
Busy_Data3$CoeffRA6A <- glm_simple_coeffs$x[12]
Busy_Data3$CoeffNCD <- glm_simple_coeffs$x[13]
Busy_Data3$CoeffProY <- glm_simple_coeffs$x[14]
Busy_Data3$CoeffDrRN <- glm_simple_coeffs$x[15]
Busy_Data3$CoeffVAge <- glm_simple_coeffs$x[16]

attach(Busy_Data3)

Busy_Data2$ManualFit <- ( CoeffBase * 
                        exp(CoeffMale * GenderMale ) *
                        exp(CoeffRA1B * RatingArea1B ) * 
                        exp(CoeffRA2A * RatingArea2A ) *
                        exp(CoeffRA2B * RatingArea2B ) *
                        exp(CoeffRA3A * RatingArea3A ) *
                        exp(CoeffRA3B * RatingArea3B ) *
                        exp(CoeffRA4A * RatingArea4A ) *
                        exp(CoeffRA4B * RatingArea4B ) *
                        exp(CoeffRA5A * RatingArea5A ) *
                        exp(CoeffRA5B * RatingArea5B ) *
                        exp(CoeffRA6A * RatingArea6A ) *
                        exp(CoeffNCD * NCD ) *
                        exp(CoeffProY * ProtectedNCDY ) *
                        exp(CoeffDrRN * DrivingRestrictionNamed ) *
                        exp(CoeffVAge * VehicleAge )
                        )

# Now using R's built in predict function but we need to change the base from ~8095 to 500

Busy_Data2$AutoPred2 <- predict(glm_simple, Busy_Data2,type="response") * 500 / 8095.5770486




