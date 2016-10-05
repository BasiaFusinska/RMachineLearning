# Read cars data
carsFilePath <- '../Data/Cars/imports-85.data'
carsData <- read.csv(carsFilePath, header = F)

# Explore dataset
head(carsData)

# Change data frame headers
colnames(carsData) <- c('symboling', 'normalized-losses', 'make', 'fuel-type', 'aspiration', 'nr-of-doors', 'body-style',
                        'drive-wheels', 'engine-location', 'wheel-base', 'length', 'width', 'height', 'curb-weight', 'engine-type',
                        'num-of-cylinders', 'engine-size', 'fuel-system', 'bore', 'stroke', 'compression-ratio', 'horsepower',
                        'peak-rpm', 'city-mpg', 'highway-mpg', 'price')
head(carsData)

# Removing empty column
carsData$`normalized-losses`<- NULL
head(carsData)

# Selecting columns
carsData <- carsData[, c('make', 'body-style', 'wheel-base', 'engine-size', 'horsepower', 'peak-rpm','highway-mpg', 'price')]
head(carsData)

# Summary statistics
summary(carsData)

# Changing data types
typeof(carsData$`wheel-base`)
typeof(carsData$`body-style`)
carsData$make <- as.double(carsData$make)
carsData$`body-style` <- as.double(carsData$`body-style`)
carsData$horsepower <- as.double(carsData$horsepower)
carsData$`peak-rpm` <- as.double(carsData$`peak-rpm`)
carsData$price <- as.double(carsData$price)

# Corellaction check
cor(carsData, method="pearson")

# Linear regression
fit <- lm(price ~ make +`body-style` + `wheel-base` + `engine-size` + horsepower + `peak-rpm` + `highway-mpg`, data=carsData)
fit

summary(fit)

# Check linearity
par(mfrow = c(2, 2))
plot(fit)

# Metrics
predicted <- predict(fit)

library('Metrics')
# Mean Absolute Error
mae(carsData$price, predicted)
# Root Mean Squared Error
rmse(carsData$price, predicted)
# Relative Absolute Error
rae(carsData$price, predicted)
# Relative Squared Error
rse(carsData$price, predicted)
# Coefficient of Determination
1 - rse(carsData$horsepower, predicted)

