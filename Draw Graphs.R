# Lutfi Sun

library(ggplot2)
theme_set(theme_bw())

Rating_Area <- glmcoeffs$names[3:12]
Coefficient_RA <- glmcoeffs$x[3:12]

rating_table <- data.frame("RatingYo" = 1:10, "Rating Area" = Rating_Area, "Coeff" = Coefficient_RA)

# Draw plot
ggplot(rating_table, aes(x=Rating_Area, y= Coefficient_RA)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Rating Area Coefficients") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

### 

NCD <- glmcoeffs$names[13:17]
Coefficient_NCD <- glmcoeffs$x[13:17]

NCD_table <- data.frame("NCDyo" = 1:5, "NCD" = NCD, "Coeff" = Coefficient_NCD)

# Draw plot
ggplot(NCD_table, aes(x=NCD, y= Coefficient_NCD)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="NCD Coefficients") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

### 

VehicleAge <- c("VehicleAge01",  "VehicleAge02", "VehicleAge03", "VehicleAge04", "VehicleAge05",
                "VehicleAge06", "VehicleAge07", "VehicleAge08",  "VehicleAge09" , "VehicleAge10" ,
                "VehicleAge11" , "VehicleAge12" , "VehicleAge13" , "VehicleAge14" , "VehicleAge15" )

Coefficient_VehicleAge <- glmcoeffs$x[20:34]
VehicleAge_table <- data.frame("NCDyo" = 1:5, "VehicleAge" = VehicleAge, "Coeff" = Coefficient_VehicleAge)

# Draw plot
ggplot(VehicleAge_table, aes(x=VehicleAge, y= Coefficient_VehicleAge)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Vehicle Age Coefficients") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

plot((resid(glm)^2))
plot((resid(glm)))
plot(glm$residuals)

plot((ppdata$VehicleAge), (ppdata$PurePrem)-(ppdata$AutoFit))
plot((ppdata$VehicleAge), (resid(glm)))

plot((ppdata$PurePrem)-(ppdata$ManualFit), ylim = range(-1500000: 1500000))

?plot


