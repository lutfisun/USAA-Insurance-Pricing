# Script Name: Math Modelling | R GLM Tutorial
# Created By: Sophie & Lutfi
# Inspired by: https://www.theanalysisfactor.com/r-tutorial-glm1/
# Creation Date: 10/16/2019

# Install and load glm & ggplot & statmod packages

# install.packages(c("aod", "broom", "dummies", "ggplot2", "plyr", "statmod"))

library(statmod)
library(broom)
library(plyr)
library(dummies)
library(aod)

# R has built in datasets. Today we will use one about cars 
?mtcars

# Let's take a look at what the data looks like
head(mtcars)

# Let's say we are interested in the variable "vs" which tells us about the engine type. 
# Looking at the data, what kind of a variable do you think it is?
View(mtcars)

# Which factor do you think should be treated as a categorical variable in our model?
unique(cyl)
unique(gear)
unique(carb)
mtcars_2 <- mtcars
mtcars_2$cyl <- factor(mtcars$cyl)
mtcars_2$gear <- factor(mtcars$gear)
mtcars_2$carb <- factor(mtcars$carb)
attach(mtcars_2)

# Let's run a linear regression using least squares method we learned in class.
# What do you think such a regression would tell us about the dummy response variable
# Answer: PROBABILITY of the engine being straight given certain info about the car

ols_simple <- lm(y ~ xk, data=mtcars_2)
summary(ols_simple)

# Now Let's visulaize our model. Similar to what we did in python with linspace
# Joining a large number of closely spaced points will give a smooth appearance to our model.

xhat <- seq(min(xk), max(xk), spacing)
coef(ols_simple) # gives intercept and slope

yhat <- (coef(ols_simple)[1] + coef(ols_simple)[2] * xhat) # our fitted line

# and lets graph our line with the actual values

plot(y ~ xk, mtcars_2, main="Ordinary Least Squares Simple")
lines(xhat, yhat)

range(yhat) # Do you see any issue with the range of y?

# Now let's spice things up and run a regression with everything we have

ols_draft <- lm(vs ~ mpg + cyl + disp + hp + drat + wt + qsec + am + gear + carb, data=mtcars_2)
summary(ols_draft)

# Although we have 0.88 R-squared, which means the model explains 88 percent of the variation,
# the results don't look good. We have low p values and counter intuitive coefficients.
# So, throwing everything into a model as you can see here is not good modelling.

# SIMPLIFICATION

# The high R-squared is most likely due to overfitting, which happens when a model
# is very specific. ie only good with the data it is trained in.
# Let's pick some variables with relatively low p values and that we think are more relevant.

ols_model <- lm(vs ~ disp + hp, data=mtcars_2)
summary(ols_model)

# What do you think is an issue with using a linear model to explain the variable vs?
# Answer: linear model can go off the boundaries of probability (0,1)

# Now Let's run a generalized linear regression
# We will again use least squares, but we can allow for different distributions
# What do you think the distibution of our variable "vs" would look like?
# Answer: Just like BINOMIAL distribution, we are asking yes-no questions in n trials.

log_model <- glm(y ~ x1 + x2, 
                 data=mtcars_2, family = "binomial")
summary(log_model)

# This is called a Logistic regression and commonly used to estimate probability.
# Now let's compare the two models. How do you think we can go about this?

mean((vs - fitted(ols_model))^2)
mean((vs - fitted(log_model))^2)

# Now let's graph our model. Since we have multiple explanatory variables, 
# We will keep all of them constant except one. Let's pick one x and vary it.
# That way we will be able to visualize how that x impacts the y

range(hp)

# Joining a large number of closely spaced points will give a smooth appearance to our model.

xhp <- seq(52, 335, 1)

# Now we make up a new dataset varying hp by small margins throughout all its range
# and keeping other xs constant 
vary_hp <- with(mtcars_2, data.frame(disp = mean(disp), hp = xhp ))

# below is how you get the predicted values using your model
y_ols <- predict(ols_model, vary_hp, type="response")

# and now we graph it
plot(vs ~ hp, mtcars_2, main="Ordinary Least Squares Regression")
lines(xhp, y_ols)

# here we do the same using glm model
y_log <- predict(log_model, vary_hp, type="response")

plot(vs ~ hp, mtcars_2, main="Generalized Model: Logistic")
lines(xhp, y_log)

# Having seen the merits of glm models, now let's try to understand what is going on
# in the background of our logistic regression model. A logistic regression has the form:
# p/(1-p) = e^{b0 + b1x1 + ...}. Then, we can recover odds by p = 1/(1 + e^-{b0 + b1x1 + ...})

coef(log_model)

mtcars_2$b_0 <- coef(log_model)[1] # ie intercept
mtcars_2$b_dis <- # INSERT corresponding coefficient
mtcars_2$b_hp <- # INSERT

attach(mtcars_2) 
# attach function lets you refer to values in a dataset without pointing to them with dollar sign
# so you can say
b_0 # instead of mtcars_2$b_0

mtcars_2$manual_fit <- # INSERT the formula as a function of disp and hp
mtcars_2$auto_fit <- fitted(log_model) # R's built in function that gives fitted values of a model

View(mtcars_2)

## Thank you for completing the tutorial. We hope it was helpful. ##