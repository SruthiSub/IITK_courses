# MTH441 Lab 2

# Problem 1
# Fit linear regression model
# automatically fits with intercept
fit <- lm(dist ~ speed, data = cars)

# view summary
summary(fit)

# verifying information in the summary:
y <- cars$dist
x <- cars$speed
n <- length(x)
p <- 1
oneN <- matrix(1, nrow = n, ncol = 1)
X <- cbind(oneN, x)

# a. A summary of the residuals
B_ols <- fit$coefficients
residuals <- fit$residuals
y_pred <- X %*% B_ols
e <- y - y_pred
if (max(residuals - e) < 1e-6){
  print("residuals and e match in values!")
}
print("Residuals: ")
quantile(e,probs = seq(0, 1, 0.25))

# b. A table with OLS coefficients, with estimate, standard error, t-value and p-value
# Estimate of coefficients is B_ols
B_ols
# Variance of B_ols is sigsq*inv(XtX)
# Estimate of sigsq = ete/(n-p-1)
solve(t(X) %*% X) %*% (t(X) %*% y)
sig <- ((t(e) %*% e)/(n-p-1))^0.5
XtXinv <- solve(t(X) %*% X)
varB0 <- sig*(XtXinv[1,1])^0.5
varB1 <- sig*(XtXinv[2,2])^0.5
varB0
varB1
# t-value and p-value for coefficients: - HOW??
# We know that each coefficient follows a normal distribution with mean same as that in estimated B_ols, and variance corr to the error we calculated
# B0 ~ N(B_ols[1], varB0), B1 ~ N(B_ols[2], varB1)
B0_t_value <- B_ols[1]/varB0
B1_t_value <- B_ols[2]/varB1
B0_t_value 
B1_t_value
B0_p_value <- 2*pt(B0_t_value, n-p-1, lower.tail = TRUE)
B0_p_value
B1_p_value <- 2*pt(B1_t_value, n-p-1, lower.tail = FALSE)
B1_p_value

# c. Residual standard error with degrees of freedom
SSRes <- t(y-y_pred)%*%(y-y_pred)
dfRes <- n-p-1
MSRes <- SSRes/dfRes
error <- sqrt(MSRes)
error
dfRes

# d. Rsquare and adjusted Rsquare
y_bar <- mean(y)*oneN
SST <- t(y-y_bar)%*%(y-y_bar)
SSR <- t(y_pred-y_bar)%*%(y_pred-y_bar)
Rsquare <- SSR/SST
adjRsquare <- 1 - (SSRes/dfRes)/(SST/(n-1))
Rsquare
adjRsquare

# e. F-statistic for model fit, with degrees of freedom and p-value
dfR <- p
MSR <- SSR/(dfR)
F0 <- MSR/MSRes
pval <- pf(F0, dfR, dfRes, lower.tail = FALSE)
F0
dfR
dfRes
pval

# Problem 2
library(MASS)
?Boston
plot(Boston)
fit_Boston <- lm(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, data = Boston)
# can also do medv ~. to fit a general model with all covariates)
summary(fit_Boston)
# small p-value of the model indicates that it is a useful model
# factors with neg coefficients lead to lower median value of the homes, and those with positive coeff lead to higher values (only the significant ones contribute)
# rm and lstat have the least p-values, hence they seem to most significantly affect the value of the home
# From the B values, rm seems to affect prices the most => large coeff value as well as significant. (2 factors to look at - B values and significance, or p-values).
# We could get a better idea if we scaled and centered the model, however it does not make practical sense as we cannot have a 'fractional' room.

# Problem 3
# least useful covariate - that with highest p value => age
fit_Boston_age <- lm(medv ~ age, Boston)
summary(fit_Boston_age)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(Boston)
# this is because there is a correlation between age and dis
fit_Boston_nodis <- lm(medv ~ crim + zn + indus + chas + nox + rm + age + rad + tax + ptratio + black + lstat, data = Boston)
summary(fit_Boston_nodis)
# observe that the significance increased, showing that our hypothesis is correct.

# Problem 4 - Multiple comparisons and Bonferonni Correction
# We want to demonstrate how the marginal tests for significance used in the lm functions are not always great
# First we will generate data from a model where we know that the null hypothesis, H0 is true:
n <- 1e3
p <- 99
test <- numeric(length = 100)
# true beta is 0
beta <- rep(0, p+1)
for (o in 1:100){
# Generate arbitrary X
X <- matrix(rnorm(n*p), ncol = p, nrow = n)
X <- cbind(1, X)
# Generate y: response with variance = 1
y <- rnorm(n, mean = X %*% beta, sd = 1)
C <- solve(t(X)%*%X)
B_ols <- C%*%(t(X)%*%y)
y_hat <- X %*% B_ols
e <- y - y_hat
sig_sq <- (t(e) %*% e)/(n-p-1)
# do a t-test for each of the 100 components with a size of 0.05
res <- numeric(length = 100)
for (i in 1:100)
{
  c <- C[i,i]
  s <- sqrt(sig_sq*c)
  t_val <- B_ols[i]/s
  prob <- 2*pt(-abs(t_val), n-p-1) # to find the corr p-value
  if (prob < 0.05){
    res[i] <- 1
  }
  else{
    res[i] <- 0
  }
}
tot <- sum(res)/100
# even if the null hypothesis is rejected for one covariate, B_ols as 0 is rejected. So we are looking for cases in which none of the marginal tests reject the null hypothesis. 
if (tot == 0)
{
  test[o] <- 1
}
else{
  test[o] <- 0
}
}
test
sum(test)/100
# only 2% do not reject the null hypothesis! => the test is not effective

# Problem 5
# Trying the same with Bonferonni Corrections => change 0.05 to 0.05/(p+1) and repeat previous analysis
test <- numeric(length = 100)
# true beta is 0
for (o in 1:100){
  # Generate arbitrary X
  X <- matrix(rnorm(n*p), ncol = p, nrow = n)
  X <- cbind(1, X)
  # Generate y: response with variance = 1
  y <- rnorm(n, mean = X %*% beta, sd = 1)
  C <- solve(t(X)%*%X)
  B_ols <- C%*%(t(X)%*%y)
  y_hat <- X %*% B_ols
  e <- y - y_hat
  sig_sq <- (t(e) %*% e)/(n-p-1)
  # do a t-test for each of the 100 components with a size of 0.05
  res <- numeric(length = 100)
  for (i in 1:100)
  {
    c <- C[i,i]
    s <- sqrt(sig_sq*c)
    t_val <- B_ols[i]/s
    prob <- 2*pt(-abs(t_val), n-p-1) # to find the corr p-value
    if (prob < 0.05/(p+1)){
      res[i] <- 1
    }
    else{
      res[i] <- 0
    }
  }
  tot <- sum(res)/100
  # even if the null hypothesis is rejected for one covariate, B_ols as 0 is rejected. So we are looking for cases in which none of the marginal tests reject the null hypothesis. 
  if (tot == 0)
  {
    test[o] <- 1
  }
  else{
    test[o] <- 0
  }
}
test
sum(test)/100
# Notice that now the test is very useful, and does not reject the null hypothesis almost all the time!