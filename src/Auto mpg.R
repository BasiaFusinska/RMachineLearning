# Read cars data
carsFilePath <- '../Data/Cars/auto-mpg.data'
carsData <- read.csv(carsFilePath, sep = '')

# Explore dataset
head(carsData)

# Change data frame headers
colnames(carsData) <- c('mpg', 'cyl', 'disp', 'hp', 'wt', 'acc', 'my', 'or', 'name')
head(carsData)

# Summary statistics
summary(carsData)

# Changing data types
typeof(carsData$cyl)
typeof(carsData$hp)
carsData$cyl <- as.double(carsData$cyl)
carsData$hp <- as.double(carsData$hp)

original <- carsData
carsData <- data.frame(original[,c(2:6, 1)])

cor(carsData, method="pearson")

# Linear regression
fit <- lm(mpg ~ cyl + disp + hp + wt + acc, data=carsData)
fit

summary(fit)

# Check linearity
par(mfrow = c(2, 2))
plot(fit)

# Metrics
predicted <- predict(fit)

library('Metrics')
# Mean Absolute Error
mae(carsData$mpg, predicted)
# Root Mean Squared Error
rmse(carsData$mpg, predicted)
# Relative Absolute Error
rae(carsData$mpg, predicted)
# Relative Squared Error
rse(carsData$mpg, predicted)
# Coefficient of Determination (R^2)
1 - rse(carsData$mpg, predicted)

# Train - test split
smp_size <- floor(0.75 * nrow(carsData))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(carsData)), size = smp_size)

train <- carsData[train_ind, ]
test <- carsData[-train_ind, ]

fit <- lm(mpg ~ cyl + disp + hp + wt + acc, data=train)
fit

predicted <- predict(fit, test)

# Mean Absolute Error
mae(test$mpg, predicted)
# Root Mean Squared Error
rmse(test$mpg, predicted)
# Relative Absolute Error
rae(test$mpg, predicted)
# Relative Squared Error
rse(test$mpg, predicted)
# Coefficient of Determination
1 - rse(test$mpg, predicted)
