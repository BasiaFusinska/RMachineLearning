# Read data from files
datasetsDir <- '../Data/Handwritten digits'

trainingFile <- paste(datasetsDir, 'optdigits.tra', sep = '/')
testFile <- paste(datasetsDir, 'optdigits.tes', sep = '/')

trainingSet <- read.csv(trainingFile, header = FALSE)
testSet <- read.csv(testFile, header = FALSE)

# Data classification
library(class)
knnModel <- knn(trainingSet[,1:64], testSet[,1:64], trainingSet[,65], k = 3, prob = TRUE)

# Metrics
xtab <- table(knnModel, testSet[,65])

library(caret) 
cMatrix <- confusionMatrix(xtab)
cMatrix

# Overall Accuracy
cMatrix$overall['Accuracy']

# Macro-averaged precision
ma_aprecision(cMatrix)

#Macro-averaged recall
ma_arecall(cMatrix)

sum_All <- sumAll(cMatrix)
sum_All

#Average accuracy
avg_accuracy(sum_All)

#Micro-averaged precision/recall
mi_precision(sum_All)
mi_recall(sum_All)

# Confusion Matrix Visualisation
confusion <- as.data.frame(cMatrix$table)
confusion

colnames(confusion) <- c('ActualClass', 'PredictedClass', 'Freq')
head(confusion)

confusion$fill <- 'TN'
confusion$fill[confusion$Freq > 0] <- 'FP'
confusion$fill[confusion$ActualClass == confusion$PredictedClass] <- 'TP'

a <- aggregate(Freq ~ ActualClass, data = confusion, sum)
a

confusion$sum <- rep(a$Freq, 10)

confusion$prob <- round(confusion$Freq/confusion$sum*100, digits=1)
confusion$probText <- paste(confusion$prob, " %")
confusion$probText[confusion$prob == 0.0] <- ''

library(ggplot2)

plot <- ggplot(confusion)
plot + geom_tile(aes(x=ActualClass, y=PredictedClass, fill=fill)) + geom_text(aes(x=ActualClass, y=PredictedClass,label=probText)) +
  scale_fill_manual(values=c("#999999", "#FFFFFF", "#56B4E9")) +
  scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + 
  labs(fill="Normalized\nFrequency")
