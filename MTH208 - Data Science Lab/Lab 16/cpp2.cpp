#include <Rcpp.h>
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
[[Rcpp::depends(RcppArmadillo)]]
[[Rcpp::export]]
mat matrixMultiplicationC(mat A, mat B){
  mat product = A * B;
  return product;
}

[[Rcpp::export]]
mat matrixAdditionC(mat A, mat B){
  mat sum = A + B;
  return sum;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
