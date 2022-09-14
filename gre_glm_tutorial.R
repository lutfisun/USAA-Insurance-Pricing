# Script Name: Math Modelling | R GLM Tutorial
# Created By: Sophie & Lutfi
# Inspired by: https://stats.idre.ucla.edu/r/dae/logit-regression/
# Creation Date: 10/16/2019

# Install and load glm & ggplot & statmod packages

# install.packages(c("aod", "broom", "dummies", "ggplot2", "plyr", "statmod"))

library(statmod)
library(broom)
library(plyr)
library(ggplot2)
library(aod)

# Read data (freely available thanks to UCLA Stats)

gradschool <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
attach(gradschool)

# Let's take a look at what the data looks like
head(gradschool)

# Which variable do you think we are interested in? What is its range?
range(admit)

# Which factor do you think should be treated as a categorical variable in our model?

gradschool$rank <- factor(gradschool$rank)

# Let's run a linear regression using least squares method we learned in class

ols_model <- lm(admit ~ gre + gpa + rank, data = gradschool)
summary(ols_model)

# What would be an issue if we use a linear model to explain that variable

# Now Let's run a generalized linear regression
# We will again use least squares, but we can allow for different distributions
# What do you think the distibution of our variable admit would look like?

log_model <- glm(admit ~ gre + gpa + rank, data = gradschool, family = "binomial")
summary(log_model)

# Now let's compare the two models. How do you think we can go about this?

mean((admit - fitted(ols_model))^2)
mean((admit - fitted(log_model))^2)

# Now let's graph our model. Since we have multiple explanatory variables, 
# We will keep all of them constant except one. Let's pick one x and vary it.
# That way we will be able to visualize how that x impacts the y

range(gre)

# Joining a large number of closely spaced points will give a smooth appearance to our model.

xgre <- seq(220, 800, 1) 

# Now we make up a new dataset varying gre by small margins throughout all its range
# and keeping other xs constant 
newdata1 <- with(gradschool, data.frame(gre = xgre, gpa = max(gpa), rank = factor(1)))

# below is how you get the predicted values using your model
yadmit_ols <- predict(ols_model, newdata1,type="response")

# and now we graph it
plot(admit ~ gre, gradschool)
lines(xgre, yadmit_ols)

# here we do the same using glm model
yadmit_log <- predict(log_model, newdata1,type="response")

plot(admit ~ gre, gradschool)
lines(xgre, yadmit_log)