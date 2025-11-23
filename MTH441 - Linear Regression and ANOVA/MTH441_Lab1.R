# Problem 1

#install.packages("matlib")
library(matlib)
#install.packages("MPV")
library(MPV)
data(table.b1)
?table.b1

# OLS estimator for the regression coefficients (with intercept)
p <- dim(table.b1)[2]-1
n <- dim(table.b1)[1]
X <- as.matrix(table.b1[1:n,1:p+1])
OneN <- matrix(1, nrow = n, ncol = 1)
Xtil <- cbind(OneN,X)
Y <- as.matrix(table.b1[1:n,1])
BOLS <- solve(t(Xtil) %*% Xtil) %*% (t(Xtil) %*% Y)
# note: use () for faster calculation
Yhat <- Xtil %*% BOLS
plot(Yhat,Y)

# OLS estimator for sig^2
e <- Y - Yhat
sigSqHat <- (t(e) %*% e)/(n-p-1)
sigSqHat

# verify that sum of residuals is 0
e
res <- t(OneN) %*% e
round(res, 10)

# verify that t(e)X = 0
eTX <- t(e) %*% Xtil
eTX

# verify that SST = SSRes + SSR
Ybar <- mean(Y) * OneN
SST <- t(Y-Ybar) %*% (Y-Ybar)
SST
SSRes <- t(e) %*% e
SSRes
SSR <- t(Yhat - Ybar) %*% (Yhat - Ybar)
SSR
SSRes+SSR-SST

# OLS Estimator of beta for a model without intercept (just use X)  

BOLS_bar <- solve(t(X) %*% X) %*% (t(X) %*% Y)
Yhat_bar <- X %*% BOLS_bar

# verify that sum of residuals is 0 => does not hold!
e <- Y - Yhat_bar 
res <- t(OneN) %*% e
res

# verify that t(e)X = 0 => holds!
# The OLS projects y onto the column space of X. The fitted values lie in the column space of X and the residuals lie in its orthogonal complement, so the residual vector is orthogonal to every column of X!
eTX <- t(e) %*% X
eTX

# Problem 2
data(p7.2)
p7.2

#Fit a simple linear regression model with intercept
x <- p7.2[,1]
y <- p7.2[,2]
n <- length(x)
p <- 1
OneN <- matrix(1, nrow = n, ncol = 1)
X <- cbind(OneN, x)
B_ols <- solve(t(X) %*% X) %*% (t(X) %*% y)
B_ols
Y_hat <- X %*% B_ols
plot(x, y)
lines(x,Y_hat)

# Problem 3

# A "line" is not a good fit here

# Problem 4

x2 <- matrix(0, nrow = n, ncol = 1)
for (i in 1:n)
{
  x2[i] = x[i]^2
}
X_q <- cbind(OneN, x, x2)
H_q <- solve(t(X_q) %*% X_q) %*% t(X_q)
B_q <- H_q %*% y
B_q
Yhat_q <- X_q %*% B_q
plot(x, y)
lines(x,Yhat_q)
e <- y - Yhat_q
e
plot(x,e)

# Problem 5
# Add linear and quadratic fits to the data and see which one is better:

plot(x,y)
lines(x, Y_hat, col = "red")
lines(x, Yhat_q, col = "blue")

# Problem 6
chisquare <- function(k)
{
  s <- numeric(length = 1000)
  for (i in 1:1000)
  {
    s[i] <- 0
    for (j in 1:k)
    {
      s[i] <- s[i] + (rnorm(n=1, mean = 0, sd = 1))^2
    }
  }
  hist(s)
}

chisquarenor <- function(k)
{
  s <- numeric(length = 1000)
  for (i in 1:1000)
  {
    s[i] <- 0
    for (j in 1:k)
    {
      s[i] <- s[i] + (rnorm(n=1, mean = 0, sd = 1))^2
    }
    s[i] <- s[i]/k
  }
  hist(s)
}

par(mfrow = c(2,3))
chisquare(3)
chisquare(5)
chisquare(10)
chisquarenor(3)
chisquarenor(5)
chisquarenor(10)

