#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector na_locf(NumericVector x) {
  int n = x.size() ;
  double lo = x[0];
  for( int i=1; i<n; i++){
    if( NumericVector::is_na(x[i]) ) {
      x[i] = lo ;
    } else {
      lo = x[i] ;    
    }
  }
  
  return x;
}