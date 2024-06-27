#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double quantile2(SEXP x, double q){

  if(TYPEOF(x) != REALSXP){
    stop("Unknown SEXP type: only double are supported");
  }

  R_xlen_t n = Rf_xlength( x );
  R_xlen_t n_y = n;

  SEXP y = PROTECT(Rf_allocVector(REALSXP, n));

  // copy x to y
  double* first = REAL(y);
  double* ptr = REAL(x);

  // memcpy(first, REAL(x), n);
  for(R_xlen_t ii = 0; ii < n ; ptr++, ii++ ){
    if( R_IsNA(*ptr) ){
      n_y--;
    } else {
      *first = *ptr;
      first++;
    }
  }

  double tmp = NA_REAL;

  if( n_y == 0 ){
    tmp = NA_REAL;
  } else {
    first = REAL(y);
    double* last = first + n_y;

    // calculate index
    double a = (n_y-1) * q;
    R_xlen_t i1 = floor( a );
    R_xlen_t i2 = ceil( a );
    double* ptr_i1 = first + ( i1 );

    // find quantile
    std::nth_element(first, ptr_i1, last);
    tmp = *ptr_i1;

    if( i1 != i2 ){
      ptr_i1 = first + ( i2 );
      std::nth_element(first, ptr_i1, last);
      tmp = tmp * (i2 - a) + *ptr_i1 * (a - i1);
    }
  }

  UNPROTECT(1);

  return( tmp );
}



/*** R
n <- 1e8
x <- sample(n); x[sample(n,10)] = NA
microbenchmark::microbenchmark({
  quantile2(as.double(x), 0.5)
}, {
  quantile(x, 0.5, na.rm = TRUE)
}, {
  median(x, na.rm = TRUE)
},
times = 1, unit = 'ms')


range(c(
  quantile2(as.double(x), 0.5),
  quantile(x, 0.5, na.rm = TRUE),
  median(x, na.rm = TRUE)
))


*/
