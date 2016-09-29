# Read files
library(imager)
myImageFile <- paste(datasetsDir, 'myDigit.png', sep = '/')
im <- load.image(myImageFile)

# Show the image
plot(im)

# Prepare classification data
compressDigit <- function(im) {
  res <- NULL
  
  for (i in 1:64)
  {
    m <- (i-1) %% 8
    d <- as.integer((i-1)/8)
    il <- m*4 + 1
    ip <- m*4 + 4
    jl <- d*4 + 1
    jp <- d*4 + 4
    
    zeros <- sum(c(im[il:ip, jl:jp, 1, 1]))
    res <- c(res, 16-zeros)
  }
  
  res
}

compressedIm <- compressDigit(im)

# Predict digit
knn(trainingSet[,1:64], compressedIm, trainingSet[,65], k = 3, prob = TRUE)
