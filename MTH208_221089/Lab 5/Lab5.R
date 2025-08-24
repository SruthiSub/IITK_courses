# Lab 5

# Vectorizing operations for code efficiency and measuring efficiency of our code

# Problem 1
attempts <- function(age) {
  count <- 0
  remain <- age
  while (remain > 0){
    count <- count + 1
    blown_out <- sample(1:remain, size = 1)
    remain <- remain - blown_out
  }
  return(count)
}

att_vec <- replicate(1e3, attempts(25))
att_vec
?replicate

# Problem 2
install.packages("rbenchmark")
library(rbenchmark)
benchmark("for loop" = {
  att_vec <- numeric(length = 1e3)
  for(i in 1:1e3) {
    1
    att_vec[i] <- attempts(25)
  }
},
"replicate" = {replicate(1e3, attempts(25))}, replications = 100)
# replicate function is faster!

# Problem 3
benchmark({
  att_vec <- numeric(length = 1e4)
  for(i in 1:1e4) {
    1
    att_vec[i] <- attempts(25)
  }
},
replicate(1e4, attempts(25)), replications = 20)
# Now the for loop ran faster!

# Problem 4
benchmark("for loop" = {
  att_vec <- numeric(length = 1e4)
  for(i in 1:1e4) {
    1
    att_vec[i] <- attempts(25)
  }
},
"replicate" = {replicate(1e4, attempts(25))}, 
"dynamic" = {
att_vec <- NULL
for(i in 1:1e4) {
  att_vec <- c(att_vec, attempts(25))
  }}
, replications = 25)
# shows how efficiency changes when we optimize memory allocation as well!
# if you know how much memory you need, then preallocate the memory. That works better
# for loop vs replicate are not very different -> no significance performance gain => we need to find a better way, and go beyond what R provides => cpp version

# Problem 5 
# To improve performance for heavy loop computations, we can use C++ within R via the Rcpp package:
install.packages("Rcpp")
library(Rcpp)
library(rbenchmark)
setwd("C:/Users/sruth/Downloads/MTH208_221089/Lab 5")
sourceCpp("attempts.cpp")

attempts <- function(age) {
  count <- 0
  remain <- age
  while(remain > 0) {
    count <- count + 1
    blown_out <- sample(1:remain, size = 1)
    remain <- remain - blown_out
  }
  return(count)
}
benchmark(
  "For Loop" = {
    att_vec <- numeric(length = 1e3)
    for(i in 1:1e3) {
      att_vec[i] <- attempts(25)
    }
  },
  "Replicate" = {
    att_vec <- replicate(1e3, attempts(25))
  },
  "C++ via Rcpp" = {
    att_vec <- attempts_cpp(25, 1e3)
  },
  replications = 100,
  columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self")
)

benchmark(
  "For Loop" = {
    att_vec <- numeric(length = 1e4)
    for(i in 1:1e4) {
      att_vec[i] <- attempts(25)
    }
  },
  "Replicate" = {
    att_vec <- replicate(1e4, attempts(25))
  },
  "C++ via Rcpp" = {
    att_vec <- attempts_cpp(25, 1e4)
  },
  replications = 20,
  columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self"),
  order = "relative"
)

# Problem 6
library(rbenchmark)
benchmark("for loop" = {
  u <- numeric(length = 1e4)
  for(i in 1:1e4) {
    u[i] <- runif(1, min = 0, max = 1)
  }},
  "all at once" = {
    u <- runif(1e4, min = 0, max = 1)
  }, replications = 250)

# Problem 7
n = 10
m = 5
M <- matrix(0, nrow = n, ncol = m)
for(i in 1:n){
  for (j in 1:m){
    M[i,j] = runif(1, min = 0, max = 1)
  }
}
?colMeans
?apply
benchmark(
  "colMeans" = {
    means <- colMeans(M)
  },
  "apply" = {
    apply(M,2,mean)
  },
  replications = 1000
)
# colMeans is faster than apply method

# Problem 8
# Theoretically assess roughly how much memory each of the objects below will take and then verify using object.size()
num1 <- numeric(length = 1e3)
num2 <- numeric(length = 1e6)
object.size(num1)
object.size(num2)

mat1 <- matrix(runif(100*1000), nrow = 100, ncol = 1000)
mat2 <- matrix(0, nrow = 100, ncol = 1000)
object.size(mat1)
object.size(mat2)

arr <- array(0, dim = c(100,100,100))
object.size(arr)

# why is there a factor of 48 or 216 or 224 more than 8(size of each byte)*no of entries?

