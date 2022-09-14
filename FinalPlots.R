#Plotplotplot

library(ggplot2)
install.packages("jtools")
library(jtools)

#Plot Prems
head(Busy_Final)
ggplot() + geom_line(aes(y = Busy_Final$Curr_Prem, x = Busy_Final$VehicleAge, colour = NCD),
                     data = Busy_Final, stat="identity")


ggplot() + geom_bar(aes(y = Curr_Prem, x = RatingArea, colour = RatingArea),
                    data = Busy_Final, stat="identity")

ggplot() + geom_bar(aes(y = Prop_Prem, x = RatingArea, colour = RatingArea),
                    data = Busy_Final, stat="identity")


#PlotCoefs
head(factors)
RelsStacked <- with(factors, data.frame(Factor = rep(Factor, 3), Category = rep(Category, 3),
                                        Model = c(rep("Current", 39), rep("Indicated", 39), rep("Proposed", 39)),
                                        Relativity = c(Current, Indicated, Proposed) ) )

ggplot(data=RelsStacked, aes(x=Category, y=Relativity, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())

GenderRels <- subset(RelsStacked, Factor == "Gender")

ggplot(data=GenderRels, aes(x=Category, y=Relativity, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())

RARels <- subset(RelsStacked, Factor == "RatingArea")

ggplot(data=RARels, aes(x=Category, y=Relativity, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())

VARels <- subset(RelsStacked, Factor == "VehicleAge")

ggplot(data=VARels, aes(x=Category, y=Relativity, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())












