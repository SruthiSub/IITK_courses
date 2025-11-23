# MTH441 Lab 3

# Problem 1

# Fit linear regression model
# automatically fits with intercept
fit <- lm(dist ~ speed, data = cars)
# view summary
plot(fit)

# reproduce all the four plots by extracting the appropriate residuals:
# plot 1 : Residuals vs Fitted
B_ols <-fit$coefficients
y_hat <- fit$fitted.values
y <- cars$dist
x <- cars$speed
n <- length(y)
oneN <- matrix(1, nrow = n, ncol = 1)
X <- cbind(oneN, x)
H <- X %*% solve(t(X) %*% X) %*% t(X)
lev <- matrix(0, nrow = n, ncol = 1)
for (i in 1:n)
{
  lev[i] <- H[i,i]
}
p <- 1
e <- y - y_hat # this is a vector with all the residuals
plot(y_hat,e)
lines(lowess(y_hat, e), col = "blue")
# Errors are not randomly spread out. The blue line is supposed to be a straight line. This indicates that the relation between speed and dist is not necessarily linear. We might need a quadratic term in the model.

# plot 2 : Normal Q-Q : Standardized residuals vs Theoretical Quantiles
sig_est <- sqrt((t(e)%*%e)/(n-p-1))
std_res <- e/(sig_est[1])
stud_res <- std_res
for (i in 1:length(std_res)){
  stud_res[i] <- std_res[i]/sqrt(1-lev[i])
}
u <- (1:n - 0.5)/n
q <- qnorm(u)
plot(q,sort(stud_res))
abline(0,1, col = "blue")
# This line shows skew in the data - not perfectly normally distributed.

# plot 3 : root(|Standardized residuals|) vs Fitted values
root_std_res <- matrix(0, nrow = n, ncol = 1)
for (i in 1:n)
{
  root_std_res[i] <- sqrt(abs(std_res[i]))
}
plot(y_hat, root_std_res)
lines(lowess(y_hat, root_std_res), col = "blue")
# This graph shows that the assumption of constant variance is not satisfied. Variance increases slightly as y_hat increases, indicating heteroscedacity.

# plot 4 : Standardized residuals vs Leverage
plot(lev, std_res)
lines(lowess(lev, std_res), col = "blue")
# No points cross cooks distance, although some points have high lev, and some have high residuals.
# Conclusion - diagnostic plots are not satisfactory, with both the linear relation assumption and the assumption of constant variance not checking out.

# Problem 2 

library(tidyverse)
library(MASS)
?Boston
?lm
Boston_fit <- lm(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, data = Boston)
B_ols <- Boston_fit$coefficient

# centering and scaling X and refitting the model without the intercept:
X <- as.matrix(Boston)[,-14]
n <- dim(X)[1]
p <- dim(X)[2]
In <- diag(x = 1, nrow = n, ncol = n)
onen <- numeric(n) + 1
y <- Boston$medv
ybar <- mean(y)
yc <- y - ybar*onen
Xc <- (In - onen %*% solve(t(onen)%*%onen)%*%t(onen))%*%X
B_tilde <- solve(t(Xc) %*% Xc) %*% (t(Xc)%*%yc)
S <- numeric(p)
for (j in 1:p){
  S[j] <- sqrt(var(X[,j]))
}
Sx <- diag(S, nrow = p, ncol = p)
Boston_SC_X <- scale(X, center = TRUE, scale = TRUE)
Boston_SC_y <- scale(y, center = TRUE, scale = FALSE)
Boston_SC <- data.frame(cbind(Boston_SC_X, Boston_SC_y))
Boston_SC
Boston_SCfit <- lm(V14 ~.-1, data = Boston_SC)
B_SC_ols <- Boston_SCfit$coefficients
B_SC_ols
B_t <- Sx %*% B_tilde
B_t
# Coefficients of the model fitted without intercept match what we derived in class!
# Repeating the above analysis with intercept:
Boston_SCfit <- lm(V14 ~., data = Boston_SC)
Boston_SCfit$coefficients
B_t
# Observe that B0 = 0 (effectively), and rest of the coefficients match
# B0 = ybar - t(xbar)B_t = 0 because both y and x are centered, making them 0 here!

# Problem 3

plot(Boston_fit)

# Residuals vs Fitted => u-shaped => model relations not correct. May need higher order terms
# Q-Q plot is skewed
# Errors don't follow constant variance
# No points cross cook's distance. not many points with high leverage, while there are points with high residuals.
# Points 365, 369, 373 are close to Cook's distance (as marked)

# using residual studied, identify possible outliers:
?residuals
# standardized residuals => outlier if mod is more than 3
e_i <- resid(Boston_fit)
sigma_hat <- summary(Boston_fit)$sigma
d_i <- e_i/sigma_hat
outliers_std <- which(abs(d_i)> 3)
cat("Identified outliers using Standarized Residuals(|di|>3):\n",outliers_std, "\n")

# studentized residuals => outlier if more than 3
h <- hatvalues(Boston_fit)
studentized_residuals <- e_i
for (i in 1:n){
  studentized_residuals[i] <- d_i[i]/(1-h[i])
}
outliers <- which(abs(studentized_residuals)> 3)
cat("Identified outliers using Studentized Residuals(|ri|>3):\n",outliers, "\n")

# k-student residuals
t_i <- e_i
for (i in 1:n){
  s2 <- ((n-p)*sigma_hat^2 - e_i[i]^2/(1-h[i]))/(n-p-1)
  t_i[i] <- e_i[i]/sqrt((1-h[i])*s2)
}
outliers <- which(abs(t_i)> 3)
cat("Identified outliers using k-Student Residuals(|ti|>3):\n",outliers, "\n")


# Leverage points:
for (i in 1:n){
  if (h[i]>2*(p+1)/n){
    print(i)
  }
}

# Influence points:
D <- cooks.distance(Boston_fit)
for (i in 1:n){
  if (abs(D[i]) > 1){
    print(i)
  }
}
# no highly influential points
# can also calculate Cook's distance as follows:
r_i <- studentized_residuals
term1 <- r_i^2 / (p + 1)
term2 <- h / (1- h)
# Calculate Cook's distance
cooks_d <- term1 * term2
cooks_d - D