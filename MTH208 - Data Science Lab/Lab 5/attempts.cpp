#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector attempts_cpp(int age,int n_reps) {
  NumericVector results(n_reps);//Preallocatevectorforn_repsresults
  for(int i = 0; i < n_reps; ++i) {
    int count = 0;
    int remain = age;
    while(remain > 0) {
      count++;
      //Generaterandomintegerbetween 1andremain(inclusive)
      int blown_out = floor(R::runif(1, remain +1));
      remain-= blown_out;
    }
    results[i] = count;
  }
  return results;
}
