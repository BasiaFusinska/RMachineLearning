# Data simulation 
regressionSampleData <- function(n, a, b) {
  x <- sample(1:n, n)
  noise <- runif(n=n, min=-n/5, max=n/5)
  y <- b + a*x + noise
  
  data.frame(x,y)
}

a <- 2.7
b <- 3.25

xyData <- regressionSampleData(100, a, b)
xyData

cor(xyData$x,xyData$y)

# Using Linear regression
fit <- lm(y ~ x, data=xyData)
fit
attributes(fit)

coef(fit)
fit$coefficients

residuals(fit)
fit$residuals

xyData$pred <- predict(fit)

# Plotting data points
library(ggplot2)
xy_points <- ggplot(xyData, aes(x, y)) + geom_point() + ggtitle("y ~ x")
xy_points

# Plot simulation line
sim_line <- xy_points + geom_abline(intercept = b, slope = a, color="blue")
sim_line

# Plot prediction line
pred_line1 <- sim_line + geom_abline(intercept = fit$coefficients[[1]], slope = fit$coefficients[[2]], color="red")
pred_line1

#Using predictions
pred_line2 <- sim_line + geom_line(aes(y = pred), color="red")
pred_line2
