# Worksheet 14

library(rbenchmark)

# Problem 1
num1 <- numeric(length = 1e3) # roughly 8000
num2 <- numeric(length = 1e6) # roughly 8000000
mat1 <- matrix(runif(100*1000), nrow = 100, ncol = 1000) # roughly 800000 
mat2 <- matrix(0, nrow = 100, ncol = 1000) # roughly 800000
arr <- array(0, dim = c(100,100,100)) # roughly 8000000
object.size(num1)
object.size(num2)
object.size(mat1)
object.size(mat2)
object.size(arr)

# Problem 2
n <- 1e4
p <- 1e2
dat <- matrix(runif(n*p), nrow = n, ncol = p)
# saving the data as a csv file, and loading it:
write.csv(dat, file = "bigData.csv", row.names = FALSE)
temp <- read.csv("bigData.csv")
# saving the data as Rdata, and loading it:
save(dat, file = "largeData.Rdata")
load("largeData.Rdata")
# Note: we can save any number of R objects we want separated by commas in the save command
benchmark(read.csv("bigData.csv"), load("largeData.Rdata"), rep = 25)

# Problem 3
x <- c(1,2,3,4,5,6,7,8,9,10)
benchmark(sqrt(sum(x^2)), norm(x,"2"), rep = 20)

# Problem 4
matgenloop <- function(n, rho)
{
  M <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      M[i,j] <- rho^(abs(i-j))
    }
  }
}
matgen <- function(n, rho)
{
  mat <- matrix(rho, nrow = n, ncol = n)
  mat <- mat^(abs(col(mat) - row(mat)))
  return(mat)
}
benchmark(matgenloop(10,2), matgen(10,2), rep = 20)
benchmark(matgenloop(100,2), matgen(100,2), rep = 20)
benchmark(matgenloop(1000,2), matgen(1000,2), rep = 20)

# Problem 5
f <- function(n)
{
  k <- n/exp(1)
  a <- 0
  for (i in 1:n){
    a <- a+log10(i/k)
  }
  ans <- (10^a)/sqrt(2*pi*n)
  return(ans)
}
x <- c(10,100,1e4,1e5,1e6)
y <- c(0,0,0,0,0)
for (i in 1:5)
{
  y[i] <- f(x[i])
}
plot(x,y)
y

# Problem 6

func <- function(n = 1e3)
{
  nums <- 1:(n^2)
  mat <- matrix(nums, nrow = n, ncol = n)
  means <- apply(mat, 2, mean)
  norm.means <- sqrt(sum(means^2))
  return(norm.means)
}
# func returns the root of the sum of squares of the means of the columns of the matrix

funcfast <- function(n = 1e3)
{
  nums <- 1:(n^2)
  mat <- matrix(nums, nrow = n, ncol = n)
  means <- colMeans(mat)
  norm.means <- norm(means, "2")
  return(norm.means)
}
func()
funcfast()
benchmark(func(), funcfast(), rep = 20)

# Problem 7

func1 <- function(vec)
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

func2 <- function(vec)
{
  log.of.vec <- log(vec)
  m <- sum(log.of.vec)
  log.of.vec <- log.of.vec/m
  return(log.of.vec)
}

x <- 1:1e4
func1(x)
func2(x)
func1(x) - func2(x) # very close to each other! 
# Why is it not exactly 0? - Numerical Instability. 
benchmark(func1(1:1e4), func2(1:1e4))
