#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double EucC(NumericVector x,NumericVector y){
  double track=0;
  int n =x.size();
  for(int i =0;i<n;i++){
    track= track+pow((x[i]-y[i]),2);
  }
  track=sqrt(track);
  return track;
}

// [[Rcpp::export]]
NumericVector funcC(NumericVector v){
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
}



//[[Rcpp::export]]
NumericVector columnSum(NumericMatrix M){
  NumericVector C = {};
  int r = M.ncol();
  for (int i = 0; i < r; i++){
    C.push_back(sum(M.column(i)));
  }
  return C;
}

//[[Rcpp::export]]
LogicalVector pos(NumericVector x){
  int n = x.size();
  LogicalVector retn = {};
  for (int i = 0; i < n ; i++){
    if (x[i] > 0) retn.push_back(TRUE);
    else retn.push_back(FALSE);
  }
  return retn;
}

//[[Rcpp::export]]
int exceedC(){
  int count = 0;
  double sum = 0;
  double draw = 0;
  while (sum < 1){
    draw = runif(1, 0, 1)[0];
    sum = draw + sum;
    count = 1 + count;
  }
  return count;
}
//[[Rcpp::export]]
double trialsC(int n){
  double sum = 0;
  for (int i=0; i<n; i++){
    sum = sum + exceedC();
  }
  sum =sum/1000;
  return sum;
}


