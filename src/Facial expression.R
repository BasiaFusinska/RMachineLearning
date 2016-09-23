datasetsDir <- '../Data/Grammatical facial expression'

getExpressions <- function(filename, expression) {
  dpFilePath <- paste(datasetsDir, '/', filename, '_datapoints.txt', sep = '')
  dtFilePath <- paste(datasetsDir, '/', filename, '_targets.txt', sep = '')
  
  dataSet <- read.csv(dpFilePath, header = TRUE, sep = ' ')
  targets <- read.csv(dtFilePath, header = FALSE)
  
  res <- dataSet[targets$V1 == 1, ]
  res$ex <- as.factor(rep(expression, nrow(res)))
  
  res
}

res <- getExpressions('a_yn_question', 'yn')
res <- rbind(res, getExpressions('b_yn_question', 'yn'))
res <- rbind(res, getExpressions('a_emphasis', 'emp'))
res <- rbind(res, getExpressions('b_emphasis', 'emp'))
res <- rbind(res, getExpressions('a_wh_question', 'wh'))
res <- rbind(res, getExpressions('b_wh_question', 'wh'))
res <- rbind(res, getExpressions('a_topics', 'top'))
res <- rbind(res, getExpressions('b_topics', 'top'))
res <- rbind(res, getExpressions('a_relative', 'rel'))
res <- rbind(res, getExpressions('b_relative', 'rel'))
res <- rbind(res, getExpressions('a_negative', 'neg'))
res <- rbind(res, getExpressions('b_negative', 'neg'))
res <- rbind(res, getExpressions('a_doubt_question', 'dq'))
res <- rbind(res, getExpressions('b_doubt_question', 'dq'))
res <- rbind(res, getExpressions('a_conditional', 'con'))
res <- rbind(res, getExpressions('b_conditional', 'con'))
res <- rbind(res, getExpressions('a_affirmative', 'aff'))
res <- rbind(res, getExpressions('b_affirmative', 'aff'))

plot(res$X1x, res$X36y, col = res$ex)

results <- kmeans(res[, 2:301], 9, 
                  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))

results
cluster <- results$cluster
centers <- results$centers

plot(res$X1x, res$X36y, col = results$cluster + 1)
points(centers[, c('X1x', 'X36y')], pch=10)

res2 <- res[res$ex=='yn',]
res2 <- rbind(res2, res[res$ex=='con',])
res2 <- rbind(res2, res[res$ex=='top',])

plot(res2$X1x, res2$X36y, col = res2$ex)
results2 <- kmeans(res2[, 2:301], 3, 
                  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))

plot(res2$X1x, res2$X36y, col = results2$cluster)
