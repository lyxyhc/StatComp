#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

//' @title count 
//' @description count the number of rejected
//' @param sigma the variance of the normal distribution 
//' @param x0 the initial value
//' @param N number of trials
//' @return a vector
//' @export
//[[Rcpp::export]]
NumericVector rwMetropolis (double sigma, double x0, int N) {
  NumericVector x(N);
  x[0] = x0; 
  NumericVector u = runif(N);
  for (int i = 1; i < N;i++ ) {
    NumericVector y = rnorm(1, x[i-1], sigma);
    if (u[i] <= (exp(-abs((y[0]))) / exp(-abs((x[i-1]))))){
      x[i] = y[0];
    }
    else { 
      x[i] = x[i-1]; 
    }
  }
  return(x);
}
