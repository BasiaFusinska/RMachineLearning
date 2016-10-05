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

# Macro-averaged precision
ma_aprecision <- function (cMatrix) { 
  mean(cMatrix$byClass[,'Precision'])
}

#Macro-averaged recall
ma_arecall <- function (cMatrix) {
  mean(cMatrix$byClass[,'Recall'])
}

# One-vs-All
sumAll <- function (cMatrix) {
  cm <- cMatrix$table
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  
  oneVsAll <- lapply(1 : nc,
      function(i){
        v = c(cm[i,i],
          rowsums[i] - cm[i,i],
          colsums[i] - cm[i,i],
          n-rowsums[i] - colsums[i] + cm[i,i]);
        return(matrix(v, nrow = 2, byrow = T))})
  
  s = matrix(0, nrow = 2, ncol = 2)
  for(i in 1 : nc){s = s + oneVsAll[[i]]}
  s
}

#Average accuracy
avg_accuracy <- function(sumAll) {
  sum(diag(sumAll)) / sum(sumAll)
}

#Micro-averaged precision
mi_precision <- function(sumAll) {
  (diag(sumAll) / apply(sumAll,1, sum))[1]
}

#Micro-averaged recall
mi_recall <- function(sumAll) {
  (diag(sumAll) / apply(sumAll,2, sum))[1]
}


