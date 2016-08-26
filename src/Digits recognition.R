datasetsDir <- '../Datasets/Handwritten digits'

trainingFile <- paste(datasetsDir, 'optdigits.tra', sep = '/')
testFile <- paste(datasetsDir, 'optdigits.tes', sep = '/')

trainingSet <- read.csv(trainingFile, header = FALSE)
testSet <- read.csv(testFile, header = FALSE)

library(class)
knnModel <- knn(trainingSet[,1:64], testSet[,1:64], trainingSet[,65], k = 3, prob = TRUE)

xtab <- table(knnModel, testSet[,65])

library(caret) 
confusionMatrix(xtab)

library(imager)
myImageFile <- paste(datasetsDir, 'myDigit.png', sep = '/')
im <- load.image(myImageFile)
plot(im)

res <- NULL

for (i in 1:64)
{
  m <- (i-1) %% 8
  d <- as.integer((i-1)/8)
  il <- m*4 + 1
  ip <- m*4 + 4
  jl <- d*4 + 1
  jp <- d*4 + 4
  
  zeros <- sum(c(im[il:ip,jl:jp,1,1]))
  res <- c(res, 16-zeros)
}

res

knn(trainingSet[,1:64], res, trainingSet[,65], k = 3, prob = TRUE)
