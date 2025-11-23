# MTH441 Lab 7

# Cross-Validation

## Choosing lambda in ridge regression
## using LOOCV
data(mtcars)
y <- mtcars$mpg # response
# adding intercept
X <- cbind(1, as.matrix(mtcars[ ,-1]))
n <- dim(X)[1]
p <- dim(X)[2]
Ip <- diag(rep(1, length = p))
# vector of lambdas
lam.vec <- c(10^(seq(-8, 8, by = .1)))
# Will store CV error in this.
CV.error <- numeric(length = length(lam.vec))
# for each lambda we will do ridge
for(l in 1:length(lam.vec))
{
  track.cv <- 0
  lam <- lam.vec[l]
  for(i in 1:n)
  {
    # Making training data
    X.train <- X[-i,] # removing ith X
    y.train <- y[-i] #removing ith y
    # fitting model for training data
    beta.train <- solve(t(X.train)%*%X.train + lam*Ip) %*% (t(X.train) %*% y.train)
    # test error
    e <- y.train - X.train %*% beta.train
    track.cv <- track.cv + t(e)%*%e
  }
  CV.error[l] <- track.cv/n
}
chosen.lam <- lam.vec[which.min(CV.error)]
beta.final <- solve(t(X) %*% X + chosen.lam*diag(p)) %*% t(X) %*% y

# Repeating the process for K-fold CV:
permutation <- sample(1:n, replace = FALSE)
K <- 4
# Making a list of indices for each split (K-fold)
test.index <- split(permutation, rep(1:K, length = n, each = n/K))
test.index
# instead of LOOCV, we can also find error for each split and add and cross validate using that

# Model Selection
# Problem 1
library(glmnet)

library(MASS)
library(ISLR)
Auto$origin <- as.factor(Auto$origin)
Auto
# including all two way interactions
full_model <- lm(mpg ~ (cylinders + I(cylinders^2)+ displacement + horsepower + weight
                        + I(weight^2) + I(acceleration^2) + I(displacement^2)
                        + I(horsepower^2) + acceleration + year)*origin, data = Auto)
summary(full_model)
stepAIC(full_model)

y <- Auto$mpg
X <- model.matrix(full_model)
ridge_model <- glmnet(X, y,  alpha = 0, lambda = 1)
lasso_model <- glmnet(X, y, alpha = 1, lambda = 1)
ridge_model$beta
lasso_model$beta
# observe that lasso does variable selection!!

# Problem 2
cv.glmnet(X,y, alpha = 0)
cv.glmnet(X,y, alpha = 1)
# put in lambda.min as the lambda values in the models!
ridge_model <- glmnet(X, y, alpha = 0, lambda = 0.6487, data = Auto)
lasso_model <- glmnet(X, y, alpha = 1, lambda = 0.00664, data = Auto)
ridge_model$beta
lasso_model$beta

