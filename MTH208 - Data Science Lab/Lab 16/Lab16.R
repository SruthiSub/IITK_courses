# Worksheet 16
library(Rcpp)
sourceCpp("cpp1.cpp")

# Problem 1

# Redo worksheet 15 using sourceCpp()

EucR <-function(x,y)
{
  rtn<-sqrt(sum((x-y)^2))
  return(rtn)
}

x <-1:10
y <-2:11
all.equal(EucR(x, y),EucC(x, y))

x <- runif(n = 1e6, 0, 10)
y <- runif(n = 1e6, 0, 10)
benchmark(EucR(x,y), EucC(x,y))
x <- c(1,2,8,3)
func <- function(vec)
{
  n <- length(vec)
  # for tracking sum and log
  sum.log <- 0
  log.of.vec <- numeric(length(n))
  # calculating logs and sum for each element
  for(i in 1:n)
  {
    log.of.vec[i] <- log(vec[i])
    sum.log <- sum.log + log.of.vec[i]
  }
  # fraction
  frac <- log.of.vec/sum.log
  return(frac)
}
funcR <- function(vec)
{
  n <- length(vec)
  # calculating logs and sum for each element
  log.of.vec <- log(vec)
  sum.log <- sum(log(vec))
  # fraction
  frac <- log.of.vec/sum.log
  return(frac)
}
func(x)
funcR(x)
funcC(x)
x <- runif(2000,min=-100, max = 100)
benchmark(func(x),funcR(x), funcC(x), replications = 25)

A <- matrix(1:5, nrow = 5, ncol = 1)
B <- matrix(3:7, nrow = 5, ncol = 1)
A+B
matAddition(A,B)
all.equal(A+B, matAddition(A,B))

M1 <- matrix(1:9, nrow = 3, ncol = 3)
all.equal(columnSum(M1),colSums(M1))
M2 <- matrix(runif(n =200,0,1), nrow = 20, ncol = 10)
all.equal(columnSum(M2),colSums(M2))
benchmark(columnSum(M2), colSums(M2), rep = 25)

v <- c(1,2,-3,0,-4)
pos(v)

# Problem 2

# Redo Problem 3 in Worksheet 3

exceedR <- function()
{
  count <- 0
  sum <- 0
  while(sum < 1){
    draw <- runif(n = 1, min = 0, max = 1)
    sum <- draw + sum
    count <- count + 1
  }
  return(count)
}
benchmark({store <- numeric(length = 1000)
for (r in 1:1000){
  store[r] <- exceedR()
}
mean(store)},trialsC(1000), rep = 25)

# Problem 3
sourceCpp("cpp2.cpp")

# Redo Problem 4 in Worksheet 3
A <- matrix(1:5, nrow = 5, ncol = 1)
B <- matrix(3:7, nrow = 5, ncol = 1)
A %*% B
matrixMultiplicationC(A,B)
benchmark(A %*% B, matrixMultiplicationC(A,B), rep = 25)

benchmark(A + B, matrixAdditionC(A,B), rep = 25)