#  Insurance Pricing Project Phase I Demo on R #

# Reading Data from a csv (comma separated values) file

demo <- read.csv("~/Desktop/Mathematical Modelling/Math Modelling Project on Insurance Pricing/Phase I/02_Demo/demo.csv", header=TRUE)

# Using lm (linear model) function to fit a line on y using xs

fit <- lm(y ~ x1 + x2 + x3, demo)

# Using summar() function to see the coefficients and significance of predictor variables

summary(fit)

par(mfrow=c(2,2))  #to display 4 plots on a 2 x 2 grid
plot(fit) #to see visually the strengths and weaknesses of our model

