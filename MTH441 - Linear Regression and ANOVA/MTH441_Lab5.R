# MTH441 Lab 5

# Problem 1
# We first generate some data from a Logistic Regression Model:
set.seed(123)
n <- 50
p <- 4
# Making model matrix
Xtilde <- matrix(rnorm(n*p), ncol = p, nrow = n)
X <- cbind(1, Xtilde)
# True betas
beta.truth <- 1:(p+1)
# calculating probability of success
pi <- 1/(1 + exp(-X %*% beta.truth))
# generating Bernoulli response
y <- rbinom(n, 1, prob = pi)
# Using the Newton-Raphson Algorithm, we want to write a function that calculates the MLE of Beta and find Beta using that
del <- function(prob){
  f <- t(X) %*% (y - prob)
  return(f)
}
hess <- function(prob){
  Q <- prob
  for (i in 1:n){
    Q[i] <- prob[i]*(1-prob[i])
  }
  W <- matrix(0, nrow = n, ncol = n)
  diag(W) <- Q
  h <- -t(X) %*% W %*% X
  return(h)
}
B <- rep(0, p+1)
for (k in 1:10){
  prob <- 1/(1+exp(-X %*% B))
  prob
  B <- B - solve(hess(prob)) %*% del(prob)
}
B # estimated beta
prob <- 1/(1+exp(-X %*% B))
H <- hess(prob)
var_covar_matrix <- -solve(H) # variance of B
var_covar_matrix
std_err <- sqrt(diag(var_covar_matrix))
# z-tests for all regression coefficients:
t <- B/(std_err)
prob <- 2 * pnorm(abs(t), lower.tail = FALSE) # to find the corr p-value
t(B)
t(prob)
t(t)
std_err
# from the p values we can conclude that B2, B3, and B4 are significant at a 0.05 alpha value
## removing intercept since X already has intercept
fit <- glm(y ~ X- 1, family = "binomial")
summary(fit)

# Problem 2
# understanding the concavity of the log-likelihood
x<-c(-2,-1,-3,-4, 1, 2, 3, 4)
y<-c( 0, 1, 0, 0, 1, 1, 1, 1)
#Stable log-likelihood for logistic regression
loglik<-function(beta,x,y){
  eta <-x * beta
  # use log1p for numerical stability : log(1+exp(eta))
  sum(y*eta-log1p(exp(eta)))
}
beta <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10)
Y <- beta
for (i in 1:16){
  Y[i] <- loglik(beta[i],x,y)
}
plot(beta,Y)
# maximum at beta = 1!
x<-c(-2,-1,-3,-4, 1, 2, 3, 4)
y<-c( 0, 0, 0, 0, 1, 1, 1, 1)
Y <- beta
for (i in 1:16){
  Y[i] <- loglik(beta[i],x,y)
}
plot(beta,Y)
# maximum at x = inf, from around 2 near 0
# since here y can be perfectly seperated by x (unlike the first case), all beta above 1 will work
# MLE doesn't exist. Log Likelihood keeps increasing as B increases.

# Problem 4
set.seed(44)
n <- 50
p <- 4
# Making model matrix
Xtilde <- matrix(rnorm(n*p), ncol = p, nrow = n)
X <- cbind(1, Xtilde)
# True betas
beta.truth <- 1:(p+1)
# calculating probability of success
pi <- 1/(1 + exp(-X %*% beta.truth))
# generating Bernoulli response
y <- rbinom(n, 1, prob = pi)
# Using the Newton-Raphson Algorithm, we want to write a function that calculates the MLE of Beta and find Beta using that
del <- function(prob){
  f <- t(X) %*% (y - prob)
  return(f)
}
hess <- function(prob){
  Q <- prob
  for (i in 1:n){
    Q[i] <- prob[i]*(1-prob[i])
  }
  Q
  W <- matrix(0, nrow = n, ncol = n)
  diag(W) <- Q
  W
  h <- -t(X) %*% W %*% X
  return(h)
}
B <- rep(0, p+1)
for (k in 1:10){
  prob <- 1/(1+exp(-X %*% B))
  prob
  B <- B - solve(hess(prob)) %*% del(prob)
}
B # estimated beta
prob <- 1/(1+exp(-X %*% B))
H <- hess(prob)
var_covar_matrix <- -solve(H) # variance of B
var_covar_matrix
std_err <- sqrt(diag(var_covar_matrix))
# z-tests for all regression coefficients:
t <- B/(std_err)
prob <- 2 * pnorm(abs(t), lower.tail = FALSE) # to find the corr p-value
t(B)
t(prob)
t(t)
std_err
# from the p values we can conclude that B2, B3, and B4 are significant at a 0.05 alpha value
## removing intercept since X already has intercept
fit <- glm(y ~ X- 1, family = "binomial")
summary(fit)
# similar to problem 3, there is a hyperplane seperating both y values. This is why we get a convergence error.
# Get the fitted probabilities from the model
fitted_probs <-predict(fit,type= "response")
plot_data <-data.frame(
  Probs= fitted_probs,
  ActualOutcome= as.factor(y)
)
plot(plot_data$Probs~ jitter(as.numeric(plot_data$ActualOutcome)),
     pch= 19, col= c("red", "blue")[plot_data$ActualOutcome],
     xlab= "ActualOutcome(y)", ylab= "FittedProbability",
     main= "FittedProbabilities",
     xaxt= "n")
axis(1, at=1:2, labels=c("0", "1"))

# Problem 5
titanic <- read.csv("https://dvats.github.io/assets/data/titanic.csv")
titanic
fit_log <- glm(Survived ~.-1,titanic, family = binomial(link = "logit"))
summary(fit_log)
B <- fit_log$coefficients
B
# Sex most significantly impacts the probability of survival
# Interpretation of B_Fare:
# for every pound paid, the probability of survival increased by a prop of 0.017 in XB term. increases prob.
names(titanic)
x_jack <- c(1, 1, 20, 0, 0, 7.5)
x_rose <- c(1, 0, 19, 1, 1, 512)
p_jack <- 1/(1+ exp(-x_jack %*% B))
p_rose <- 1/(1+exp(-x_rose %*% B))
p_jack
p_rose