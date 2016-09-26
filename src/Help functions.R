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
