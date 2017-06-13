# Load Prestige dataset
library(car)
?Prestige

# 5 features, 150 examples
dim(Prestige)
names(Prestige)

# Looking at the data
head(Prestige)
summary(Prestige)

# Pairs for the numeric data
pairs(Prestige[,-c(5,6)], pch=21, bg=Prestige$type)

# Linear regression by all numerical features
all.model <- lm(prestige ~ education + income + women, Prestige)
summary(all.model)

# Linear regression with log income
num.model <- lm(prestige ~ education + log2(income) + women, Prestige)
summary(num.model)

# categorical data
cat.model <- lm(prestige ~ education + log2(income) + type, Prestige)
summary(cat.model)

# Education:type
et.fit <- lm(prestige ~ type*education, Prestige)
summary(et.fit)

# Visualise prestige by Education:Type
cf <- et.fit$coefficients
ggplot(Prestige, aes(education, prestige)) + geom_point(aes(col=type)) +
  geom_abline(slope=cf[4], intercept = cf[1], colour='red') +
  geom_abline(slope=cf[4] + cf[5], intercept = cf[1] + cf[2], colour='green') +
  geom_abline(slope=cf[4] + cf[6], intercept = cf[1] + cf[3], colour='blue')
