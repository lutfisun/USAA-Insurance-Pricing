data <- read.csv("C:\\Users\\Courtney\\Documents\\Project\\demo.csv",header=TRUE)
fit <- lm(y ~ x1 + x2 + x3, data)
summary(fit)

par(mfrow=c(2,2))
plot(fit)


