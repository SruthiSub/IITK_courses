# Worksheet 15

library(Rcpp)
library(rbenchmark)
# We use Rcpp to use C++ from within R
# For C++, look at structure of a function, data types, looping, and if-else branching.

# Consider the following R function
addR <- function(x, y)
{
  return(x + y)
}
# cppFunction() is a function that takes a bunch of C++ codes (all in quotes)
# and then saves and compiles it.
# In C++, we have to declare the type of every object.
# The function we are making is addC() which accepts two integers
# and returns the sum of the two integers.
cppFunction('int addC(int x, int y) {
 int sum = x + y;
 return sum;
 }')
addR(3,4)
addC(3,4)

# Calculating the euclidean distance in R and in C++
#In R
EucR <-function(x,y)
{
  rtn<-sqrt(sum((x-y)^2))
  return(rtn)
}
#In C++ usingRcpp
cppFunction('double EucC(NumericVector x,NumericVector y){
 double track=0;
 int n =x.size();
 for(int i =0;i<n;i++){
 track= track+pow((x[i]-y[i]),2);
 }
 track=sqrt(track);
 return track;
 }
 ')
x <-1:10
y <-3:12
# all.equal checks whether result is the same
all.equal(EucR(x, y),EucC(x, y))

# Problem 1
x <- runif(n = 1e4, 0, 10)
y <- runif(n = 1e4, 0, 10)
benchmark(EucR(x,y), EucC(x,y), rep = 25)

# Problem 2

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
funcR(c(1,2,8))

cppFunction('NumericVector funcC(NumericVector v){
double sum = 0;
NumericVector logv = v;
for (int i = 0; i < v.length(); i++){
logv[i] = log(v[i]);
sum = sum + logv[i];
}
NumericVector frac = logv;
for (int i = 0; i < v.length(); i++){
frac[i] = logv[i]/sum;
}
return logv;
            }')
funcC(c(1,2,8))
x <- c(1,2,8,3)
benchmark(func(x), funcR(x), funcC(x), replications = 25)

# Problem 3
cppFunction('NumericVector addMat(NumericMatrix mat1, NumericMatrix mat2){
            int r = mat1.nrow();
            int c = mat1.ncol();
            NumericMatrix mat(r,c);
            for (int i=0;i<c;i++){
            for (int j=0;j<r;j++){
            mat(j,i) = mat1(j,i)+mat2(j,i);
            }
            }
            return mat;
            }')

A <- matrix(1:5, nrow = 5, ncol = 1)
B <- matrix(3:7, nrow = 5, ncol = 1)
A+B
addMat(A,B)
all.equal(A+B, addMat(A,B))

# Problem 4
cppFunction('NumericVector columnSum(NumericMatrix M){
NumericVector C = {};
int r = M.ncol();
for (int i = 0; i < r; i++){
C.push_back(sum(M.column(i)));
}
return C;
            }')
M1 <- matrix(1:9, nrow = 3, ncol = 3)
all.equal(columnSum(M1),colSums(M1))
M2 <- matrix(runif(n =200,0,1), nrow = 20, ncol = 10)
all.equal(columnSum(M2),colSums(M2))
benchmark(columnSum(M2), colSums(M2), replications = 25)

# Problem 5
cppFunction('LogicalVector pos(NumericVector x){
int n = x.size();
LogicalVector retn = {};
for (int i = 0; i < n ; i++){
if (x[i] > 0) retn.push_back(TRUE);
else retn.push_back(FALSE);
}
return retn;
}
            ')
v <- c(1,2,-3,0,-4)
pos(v)
# alternative implementation:
cppFunction('NumericVector posneg(NumericVector vec){
            int n = vec.size();
            NumericVector ret(n);
            for (int i=0;i < n;i++){
            if(vec[i] >0){
            ret[i] = 1;
            }
            else{
            ret[i] = 0;
            }
            }
            return ret;
}
            ')

# Problem 6
rho_mat_1 <- function(n, rho)
{
  mat <- matrix(rho, nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      mat[i,j] <- rho^(abs(i-j))
    }
  }
  return(mat)
}
rho_mat_2 <- function(n, rho)
{
  mat <- matrix(rho, nrow = n, ncol = n)
  mat <- mat^(abs(col(mat) - row(mat)))
  return(mat)
}
# Add Cpp implementations and benchmark
cppFunction('NumericMatrix rhomatC(double rho, int n){
            NumericMatrix m(n);
            for (int i=0;i<n;i++){
            for (int j=0;j<n;j++){
            m(i,j) = pow(rho,abs(i-j));
            }
            }
            return m;
            }')
rho_mat_1(3,2)
rho_mat_2(3,2)
rhomatC(2,3)
benchmark(rho_mat_1(1000,2), rho_mat_2(1000,2), rhomatC(2,1000))
# clearly the implementation in C is the most efficient!