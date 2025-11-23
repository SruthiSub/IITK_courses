# MTH441 Lab 9

# Problem 1
library(glmnet)
titanic <- read.csv("https://dvats.github.io/assets/data/titanic.csv")
# Convert to factor
titanic$Sexmale <- as.factor(titanic$Sexmale)
# Fit MLE logistic regression
mle_fit <- glm(Survived ~ Sexmale + Age + SibSp + Parch + Fare, data = titanic, family = "binomial")
summary(mle_fit)
AIC(mle_fit)
BIC(mle_fit)

# LASSO fit
titanic <- read.csv("https://dvats.github.io/assets/data/titanic.csv")
X <- as.matrix(titanic[,-c(1,2)])
fit <- cv.glmnet(X, titanic$Survived, family = "binomial", alpha = 1)
l <- fit$lambda.min
fit <- glmnet(X, titanic$Survived, family = "binomial", alpha = 1, lambda = l)
beta_hat <- coef(fit)
beta_hat
# values are close to that of mle_fit!
pred <- predict(fit, newx = X, type = "response")
pred
ll_lasso <- sum(titanic$Survived*log(pred)+(1-titanic$Survived)*log(1-pred))
AIC_lasso <- -2*(ll_lasso) + 2*6
AIC_lasso

# Problem 2
library(CatDataAnalysis)
library(MASS)

data(table_4.3, package = "CatDataAnalysis")
crabs <- table_4.3
str(crabs)
poisson_fit <- glm(satell ~ color+spine+width+weight,crabs,family = "poisson")
summary(poisson_fit)
AIC(poisson_fit)
BIC(poisson_fit)

# Problem 3
# fitting a negative binomial model
nb_fit <- glm.nb(satell ~ color+spine+width+weight,crabs)
summary(nb_fit)
AIC(nb_fit)
BIC(nb_fit) # both AIC and BIC are lower in this case
# negative binomial seems to be doing a better job at modelling this dataset