# Read data from files
datasetsDir <- '../Data/Handwritten digits'

trainingFile <- paste(datasetsDir, 'optdigits.tra', sep = '/')
testFile <- paste(datasetsDir, 'optdigits.tes', sep = '/')

# Read training and test data
trainingSet <- read.csv(trainingFile, header = FALSE)
testSet <- read.csv(testFile, header = FALSE)

# Set labels as factor
trainingSet$V65 <- factor(trainingSet$V65)
testSet$V65 <- factor(testSet$V65) 

# Data classification
library(caret)

# Use knn algorithm
knn.fit <- knn3(V65~., data=trainingSet, k=5)
knn.fit
summary(knn.fit)

pred.test <- predict(knn.fit, testSet[,1:64], type="class")

cmatrix <- confusionMatrix(pred.test, testSet[,65])
cmatrix
confusion.matrix.visual(cmatrix$table, 10)
