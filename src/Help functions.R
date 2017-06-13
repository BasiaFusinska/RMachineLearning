library('Metrics')

# Relative Squared Error
rse <- function (actual, predicted)
{
  sum(se(actual, predicted)) / sum(se(actual, mean(actual)))
}

# Relative Absolute error
rae <- function (actual, predicted)
{
  sum(ae(predicted, actual)) / sum(ae(actual, mean(actual)))
}

confusion.matrix.visual <- function(conf.table, labels.num){
  confusion <- as.data.frame(conf.table)
  colnames(confusion) <- c('PredictedClass', 'ActualClass', 'Freq')

  confusion$fill <- 'TN'
  confusion$fill[confusion$Freq > 0] <- 'FP'
  confusion$fill[confusion$ActualClass == confusion$PredictedClass] <- 'TP'
  
  a.confusion <- aggregate(Freq ~ ActualClass, data = confusion, sum)
  confusion$sum <- rep(a.confusion$Freq, labels.num)
  
  confusion$prob <- round(confusion$Freq/confusion$sum*100, digits=1)
  confusion$probText <- paste(confusion$prob, " %")
  confusion$probText[confusion$prob == 0.0] <- ''
  
  require(ggplot2)
  
  plot <- ggplot(confusion)
  plot + geom_tile(aes(x=ActualClass, y=PredictedClass, fill=fill)) + geom_text(aes(x=ActualClass, y=PredictedClass,label=probText)) +
    scale_fill_manual(values=c("#999999", "#FFFFFF", "#56B4E9")) +
    scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + 
    labs(fill="Normalized\nFrequency")
}

