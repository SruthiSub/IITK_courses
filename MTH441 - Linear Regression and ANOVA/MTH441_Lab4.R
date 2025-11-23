# MTH441 Lab 4

# Problem 1

boxcox_transformation <- function(lambda, y)
{
  g <- exp(mean(log(y)))
  if (lambda == 0)
  {
    ynew <- g*log(y)
    return(ynew)
  }
  ynew <- (y^(lambda) - 1)/(lambda*g^(lambda - 1))
  return(ynew)
}

fit <- lm(dist ~ speed, data = cars)
e_0 <- residuals(fit)
SS_Res_0 <- t(e_0) %*% e_0
summary(fit)
l<- 10
n <- length(cars$dist)

lambda_grid <- seq(from = -2, to = 2, by = 0.05)
err <- numeric(length = length(lambda_grid))
for (i in 1:(length(lambda_grid)))
{
  lambda <- lambda_grid[i]
  y <- boxcox_transformation(lambda,cars$dist)
  fit1 <- lm(y~cars$speed)
  e <- residuals(fit1)
  SS_Res <- t(e) %*% e
  if (SS_Res < SS_Res_0)
  {
    SS_Res_0 <- SS_Res
    l <- lambda
  }
  err[i] <- -(n/2)*log(SS_Res/n)
}

# Now SS_Res_0 will have the least possible errors, among this grid and lambda, the optimal value of lambda
# Note that if all these transformations are worse than no transformation, lambda will be 10, and that implies this case
l
# err is profile log likelihood
plot(lambda_grid,err, type = "l", col = "red")

boxcox(fit)
# Observe that the graph we obtained is a scaled version of this! (there is a constant term unaccounted for - doesn't matter in finding optimal)
# Both methods give approximately the same l value!

# Problem 2

# install.packages("MPV")
library("MPV")

table.b15
data <- table.b15[,-1] # Remove city from dataset
plot(data)
fit_0 <- lm(Mort ~ ., data = data)
summary(fit_0)
par(mfrow = c(2,2))
plot(fit_0)
par(mfrow = c(1,1))
boxcox(fit_0, lambda = seq(-5,5,1/10))
l <- 1.5 # around 1.5 gives least errors => corresponds to appropriate transformation

# to find optimal lambda according to highest adjusted R^2
Y <- boxcox_transformation(l, table.b15$Mort)
fit_1 <- lm(Mort ~ ., data)
summary(fit_1)
par(mfrow = c(2,2))
plot(fit_1)

# box - cox transformation does not significantly improve adjusted R^2.
# from plot(data), we can observe that a quadratic term for SO2, NoX, and Educ may be useful, so let us try fitting models with quadratic terms and comparing the adjusted R^2
# we also can take log(Nox), seeing how cramped the observations are

data$Nox <- log(data$Nox)
SO2_sq <- data$SO2^2
Nox_sq <- data$Nox^2
Educ_sq <- data$Educ^2
Precip_sq <- data$Precip^2
Nonwhite_sq <- data$Nonwhite^2

data1 <- cbind(data, SO2_sq)
fit_2 <- lm(Mort ~ ., data = data1)
summary(fit_2)
# since adjusted R^2 increases, we will include this term!
data <- data1

data2 <- cbind(data, Nox_sq)
fit_3 <- lm(Mort ~., data = data2)
summary(fit_3)
data <- data2

data3 <- cbind(data, Educ_sq)
fit_4 <- lm(Mort ~., data = data3)
summary(fit_4)
data <- data3

data4 <- cbind(data, Precip_sq)
fit_5 <- lm(Mort ~ ., data = data4)
summary(fit_5)
data <- data4

data5 <- cbind(data, Nonwhite_sq)
fit_6 <- lm(Mort ~ ., data = data5)
summary(fit_6)
data <- data5
fit <- fit_6

# Problem 3

test_perform <- read.csv("test_perform.csv")
plot(test_perform)
# Fit a linear regression model with y as the response that maximizes the adjusted R^2
fit_tests <- lm(y~., test_perform)
summary(fit_tests)
RRsq_base <- 0.7597
# looks like everything except studs is significant

boxcox(fit_tests) # => does not make sense to do the boxcox transformation

fit_1 <- lm(y~. - studs, test_perform)
summary(fit_1)
plot(fit_1)
# Q-Q plot doesn't push for a transformation
boxcox(fit_1)
# 1 in confidence interval => transformation not needed

# from here looks like nothing else to do. But is this the right model in the first place?

# add a weights in lm function after figuring them out
# notice that the variance of each school is sig2/n (varies)
# since y is an average (divided by n), we should take weighted least squares to minimize, where weights is the number of students
weights <- test_perform$studs
fit <- lm(y~., data = test_perform, weights = weights)
summary(fit)
plot(fit)