# Lutfi Sun

#Install and load packages
library(tweedie)
library(statmod)
library(broom)
library(plyr)
library(ggplot2)
library(dummies)
library(readr)

# I don't like how business data is organized. 
# Will take only the factors into another table and make some name and ordering changes for the rest.
Busy_Data_Final <- with(Busy_Data, data.frame(Gender = Gender, RatingArea = RatingArea, NCD = NCD, 
                                         ProtectedNCD = ProtectedNCD, DrivingRestriction = DrivingRestriction, 
                                         VehicleAge = VehicleAge ) )

# we assigned **prop as factors from model before.
# now will call the raw factors from model indicated and modified ones proposed (similar to how we did in excel)
# FC: current factor, FI: indicated factor, FP: proposed factor
# For now, proposed will be same as indicated. we will change it once we calculate new base rates

Busy_Data_Final$FC_Gender <- Busy_Data$GenderCurrent
Busy_Data_Final$FI_Gender <- Busy_Data$GenderProp
Busy_Data_Final$FP_Gender <- Busy_Data$GenderProp

Busy_Data_Final$FC_RatingArea <- Busy_Data$RatingAreaCurrent
Busy_Data_Final$FI_RatingArea <- Busy_Data$RatingAreaProp
Busy_Data_Final$FP_RatingArea <- Busy_Data$RatingAreaProp

Busy_Data_Final$FC_NCD <- Busy_Data$NCDCurr
Busy_Data_Final$FI_NCD <- Busy_Data$NCDProp
Busy_Data_Final$FP_NCD <- Busy_Data$NCDProp

Busy_Data_Final$FC_PRNCD <- Busy_Data$PRNCDCurr
Busy_Data_Final$FI_PRNCD <- Busy_Data$PRNCDProp
Busy_Data_Final$FP_PRNCD <- Busy_Data$PRNCDProp

Busy_Data_Final$FC_DR <- Busy_Data$DRCurr
Busy_Data_Final$FI_DR <- Busy_Data$DRProp
Busy_Data_Final$FP_DR <- Busy_Data$DRProp

Busy_Data_Final$FC_VA <- Busy_Data$VACurr
Busy_Data_Final$FI_VA <- Busy_Data$VAProp
Busy_Data_Final$FP_VA <- Busy_Data$VAProp

attach(Busy_Data_Final)
Busy_Data_Final$Curr_Rels <- FC_Gender * FC_RatingArea * FC_NCD * FC_PRNCD * FC_DR * FC_VA
Busy_Data_Final$Indi_Rels <- FI_Gender * FI_RatingArea * FI_NCD * FI_PRNCD * FI_DR * FI_VA
Busy_Data_Final$Prop_Rels <- 0

# we have our new more organized and clear (I think) dataset. now calculate new base rates

CurrentSum = sum(Busy_Data_Final$Curr_Rels)
IndicatedSum = sum(Busy_Data_Final$Indi_Rels)
rate_change = 0.021

indicated_Base = (1+rate_change)*500*CurrentSum/IndicatedSum
indicated_Base

Busy_Data_Final$Curr_Base <- 500
Busy_Data_Final$Indi_Base <- indicated_Base
Busy_Data_Final$Prop_Base <- 0

Busy_Data_Final$Curr_Prem <- 500 * Busy_Data_Final$Curr_Rels
Busy_Data_Final$Indi_Prem <- indicated_Base * Busy_Data_Final$Indi_Rels
Busy_Data_Final$Prop_Prem <- 0

RevenueChange = (sum(Busy_Data_Final$Indi_Prem) - sum(Busy_Data_Final$Curr_Prem)) / sum(Busy_Data_Final$Curr_Prem)
RevenueChange # 0.021 just as we wanted

# Now Business Considerations (ie analyze dislocation)
Busy_Data_Final <- Busy_Final
attach(Busy_Data_Final)

Curr_PremMale = 0
Curr_Prem1B = 0
Curr_Prem2B = 0
Curr_Prem3B = 0
Curr_PremDRNamed = 0
for (t in 1:49297) {
  if (Busy_Data_Final$Gender[t] == "Male") Curr_PremMale = Curr_PremMale + Curr_Prem[t]
  if (Busy_Data_Final$RatingArea[t] == "1B") Curr_Prem1B = Curr_Prem1B + Curr_Prem[t]
  if (Busy_Data_Final$RatingArea[t] == "2B") Curr_Prem2B = Curr_Prem2B + Curr_Prem[t]
  if (Busy_Data_Final$RatingArea[t] == "3B") Curr_Prem3B = Curr_Prem3B + Curr_Prem[t]
  if (Busy_Data_Final$DrivingRestriction[t] == "Named") Curr_PremDRNamed = Curr_PremDRNamed + Curr_Prem[t]
}

Indi_PremMale = 0
Indi_Prem1B = 0
Indi_Prem2B = 0
Indi_Prem3B = 0
Indi_PremNamed = 0
for (t in 1:49297) {
  if (Busy_Data_Final$Gender[t] == "Male") Indi_PremMale = Indi_PremMale + Indi_Prem[t]
  if (Busy_Data_Final$RatingArea[t] == "1B") Indi_Prem1B = Indi_Prem1B + Indi_Prem[t]
  if (Busy_Data_Final$RatingArea[t] == "2B") Indi_Prem2B = Indi_Prem2B + Indi_Prem[t]
  if (Busy_Data_Final$RatingArea[t] == "3B") Indi_Prem3B = Indi_Prem3B + Indi_Prem[t]
  if (Busy_Data_Final$DrivingRestriction[t] == "Named") Indi_PremNamed = Indi_PremNamed + Indi_Prem[t]
}

ChangeMale = (Indi_PremMale - Curr_PremMale) / Curr_PremMale
Change1B = (Indi_Prem1B - Curr_Prem1B) / Curr_Prem1B 
Change2B = (Indi_Prem2B - Curr_Prem2B) / Curr_Prem2B 
Change3B = (Indi_Prem3B - Curr_Prem3B) / Curr_Prem3B 
ChangeNamed = (Indi_PremNamed - Curr_PremDRNamed) / Curr_PremDRNamed 

Changes_Final <- data.frame(
  "Pool" = c("Male","Ra1B","Ra2B", "Ra3B", "Named"),
  "OldSum" = c(Curr_PremMale, Curr_Prem1B, Curr_Prem2B, Curr_Prem3B, Curr_PremDRNamed),
  "NewSum" = c(Indi_PremMale, Indi_Prem1B, Indi_Prem2B, Indi_Prem3B, Indi_PremNamed),
  "Change" = c(ChangeMale, Change1B, Change2B, Change3B, ChangeNamed)
)

summary(Changes_Final$Change)
scalingfactor = max(Changes_Final$Change) / 0.06
# Capping at 5 percent instead to make sure when we change the base it does not go back up to 7
Changes_Final$ScaledChange = Changes_Final$Change / scalingfactor
Changes_Final$GrossChange = 1 + Changes_Final$ScaledChange
View(Changes_Final)


# Now applying these changes to the relevant proposed factors

names(Busy_Data_Final)

for (t in 1:49297) {
  if (Busy_Data_Final$Gender[t] == "Male") Busy_Data_Final$FP_Gender[t] = 
      Busy_Data_Final$FC_Gender[t] * Changes_Final$GrossChange[1]
  
  if (Busy_Data_Final$RatingArea[t] == "1B") Busy_Data_Final$FP_RatingArea[t] = 
      Busy_Data_Final$FC_RatingArea[t] * Changes_Final$GrossChange[2]
  
  if (Busy_Data_Final$RatingArea[t] == "2B") Busy_Data_Final$FP_RatingArea[t] = 
      (Busy_Data_Final$FC_RatingArea[t] * Changes_Final$GrossChange[3])
  
  if (Busy_Data_Final$RatingArea[t] == "3B") Busy_Data_Final$FP_RatingArea[t] = 
      Busy_Data_Final$FC_RatingArea[t] * Changes_Final$GrossChange[4]
  
  if (Busy_Data_Final$DrivingRestriction[t] == "Named") Busy_Data_Final$FP_DR[t] = 
      Busy_Data_Final$FC_DR * Changes_Final$GrossChange[5]
}

attach(Busy_Data_Final)

sum(FI_Gender - FP_Gender) #Proposed is different than indicated so the modification happened

# Now Rounding all the factors as she wanted
Busy_Data_Final$FI_Gender <- round(Busy_Data_Final$FI_Gender,digits=2)
Busy_Data_Final$FI_RatingArea <- round(Busy_Data_Final$FI_RatingArea,digits=2)
Busy_Data_Final$FI_NCD <- round(Busy_Data_Final$FI_NCD,digits=2)
Busy_Data_Final$FI_PRNCD <- round(Busy_Data_Final$FI_PRNCD,digits=2)
Busy_Data_Final$FI_DR <- round(Busy_Data_Final$FI_DR,digits=2)
Busy_Data_Final$FI_VA <- round(Busy_Data_Final$FI_VA,digits=2)

Busy_Data_Final$FP_Gender <- round(Busy_Data_Final$FP_Gender,digits=2)
Busy_Data_Final$FP_RatingArea <- round(Busy_Data_Final$FP_RatingArea,digits=2)
Busy_Data_Final$FP_NCD <- round(Busy_Data_Final$FP_NCD,digits=2)
Busy_Data_Final$FP_PRNCD <- round(Busy_Data_Final$FP_PRNCD,digits=2)
Busy_Data_Final$FP_DR <- round(Busy_Data_Final$FP_DR,digits=2)
Busy_Data_Final$FP_VA <- round(Busy_Data_Final$FP_VA,digits=2)

#Updating Rels after rounding

Busy_Data_Final$Indi_Rels <- FI_Gender * FI_RatingArea * FI_NCD * FI_PRNCD * FI_DR * FI_VA
Busy_Data_Final$Prop_Rels <- FP_Gender * FP_RatingArea * FP_NCD * FP_PRNCD * FP_DR * FP_VA

attach(Busy_Data_Final)

sum(Indi_Rels - Prop_Rels) #again modification happened

CurrentSum = sum(Busy_Data_Final$Curr_Rels)
ProposedSum = sum(Busy_Data_Final$Prop_Rels)
ProposedSum
IndicatedSum
rate_change = 0.021

Proposed_Base = (1+rate_change)*500*CurrentSum/ProposedSum

Busy_Data_Final$Prop_Base <- Proposed_Base
Proposed_Base #went up as expected (others subsidizing the competitive categories)

Busy_Data_Final$Prop_Prem <- Proposed_Base * Busy_Data_Final$Prop_Rels

ProposedRevenueChange = (sum(Busy_Data_Final$Prop_Prem) - sum(Busy_Data_Final$Curr_Prem)) / sum(Busy_Data_Final$Curr_Prem)
ProposedRevenueChange # 0.021 just as we wanted

# checking one last time whether we are still competitive in those categories with the new base rate
attach(Busy_Data_Final)

Prop_PremMale = 0
Prop_Prem1B = 0
Prop_Prem2B = 0
Prop_Prem3B = 0
Prop_PremNamed = 0
for (t in 1:49297) {
  if (Busy_Data_Final$Gender[t] == "Male") Prop_PremMale = Prop_PremMale + Prop_Prem[t]
  if (Busy_Data_Final$RatingArea[t] == "1B") Prop_Prem1B = Prop_Prem1B + Prop_Prem[t]
  if (Busy_Data_Final$RatingArea[t] == "2B") Prop_Prem2B = Prop_Prem2B + Prop_Prem[t]
  if (Busy_Data_Final$RatingArea[t] == "3B") Prop_Prem3B = Prop_Prem3B + Prop_Prem[t]
  if (Busy_Data_Final$DrivingRestriction[t] == "Named") Prop_PremNamed = Prop_PremNamed + Prop_Prem[t]
}

Changes_Final$Dislocation = c(
(Prop_PremMale - Curr_PremMale) / Curr_PremMale,
(Prop_Prem1B - Curr_Prem1B) / Curr_Prem1B, 
(Prop_Prem2B - Curr_Prem2B) / Curr_Prem2B, 
(Prop_Prem3B - Curr_Prem3B) / Curr_Prem3B, # only this one incresed by a little above 7 percent 0.07291299
(Prop_PremNamed - Curr_PremDRNamed) / Curr_PremDRNamed )

write.table(Changes_Final, file = "Changes_Final.csv", sep=",")


write.table(Busy_Data_Final, file = "Busy_Data_Final2.csv", sep=",")





