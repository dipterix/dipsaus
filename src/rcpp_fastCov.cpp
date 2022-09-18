#include <cstdlib>
#include <Rcpp.h>
#include "TinyParallel.h"

using namespace Rcpp;
// using namespace TinyParallel;

struct FastCov : public TinyParallel::Worker
{
  Rcpp::NumericVector &x1;
  Rcpp::NumericVector &x2;
  Rcpp::IntegerVector &col1;
  Rcpp::IntegerVector &col2;
  Rcpp::NumericVector &cm1;
  Rcpp::NumericVector &cm2;
  const R_xlen_t &nrow;
  const R_xlen_t y_nrow;
  const double &df;
  double* y_ptr;

  FastCov(
    Rcpp::NumericVector &x1,
    Rcpp::NumericVector &x2,
    Rcpp::IntegerVector &col1,
    Rcpp::IntegerVector &col2,
    Rcpp::NumericVector &cm1,
    Rcpp::NumericVector &cm2,
    const R_xlen_t &nrow,
    const double &df,
    // const R_xlen_t &y_nrow,
    SEXP y
  ): x1(x1), x2(x2), col1(col1), col2(col2), cm1(cm1), cm2(cm2),
  nrow(nrow), y_nrow(col1.length()), df(df), y_ptr(REAL(y)){}

  void operator()(std::size_t begin, std::size_t end) {
    // begin -> end columns
    R_xlen_t ii, jj, kk, c1, c2;
    Rcpp::NumericVector::iterator pt1, pt2;
    Rcpp::NumericVector::iterator pt_cm1, pt_cm2;
    Rcpp::IntegerVector::iterator pt_col1, pt_col2;
    double tmp;
    double* y_ptr2;

    pt_col1 = col2.begin() + begin;
    pt_cm2 = cm2.begin() + begin;
    y_ptr2 = y_ptr + begin * y_nrow;
    for(jj = begin; jj < end; jj++, pt_cm2++){
      c2 = *pt_col1++ - 1;
      // y_ptr2 = y_ptr + jj * y_nrow;
      pt_cm1 = cm1.begin();
      pt_col2 = col1.begin();
      for(ii = 0; ii < y_nrow; ii++){
        c1 = (*pt_col2++) - 1;
        pt1 = x1.begin() + c1 * nrow;
        pt2 = x2.begin() + c2 * nrow;
        // cov c1, c2 columns
        tmp = 0;
        for(kk = 0; kk < nrow; kk++, pt1++, pt2++){
          tmp += (*pt1) * (*pt2);
        }
        tmp -= (*pt_cm2) * (*pt_cm1++) * nrow;
        *y_ptr2++ = tmp / df;
      }

    }
  }

};


// [[Rcpp::export]]
SEXP fastcov(
    Rcpp::NumericVector &x1,
    Rcpp::NumericVector &x2,
    const R_xlen_t nrow,
    Rcpp::IntegerVector &col1,
    Rcpp::IntegerVector &col2,
    Rcpp::NumericVector &cm1,
    Rcpp::NumericVector &cm2,
    const double df) {

  const R_xlen_t re_nrow = col1.length();
  const R_xlen_t re_ncol = col2.length();
  SEXP re = PROTECT(Rf_allocVector(REALSXP, re_nrow * re_ncol));
  SEXP dm = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(dm)[0] = re_nrow;
  INTEGER(dm)[1] = re_ncol;
  Rf_setAttrib(re, R_DimSymbol, dm);
  // double* re_ptr = REAL(re);

  FastCov fcov(x1, x2, col1, col2, cm1, cm2, nrow, df, re);
  parallelFor(0, re_ncol, fcov);

  UNPROTECT(2);
  return(re);
}

/*** R
devtools::load_all()
TinyParallel::setThreadOptions(numThreads = 8)

x <- as.data.frame(matrix(rnorm(100000), nrow = 1000))
y <- matrix(rnorm(100000), nrow = 1000)
col1 <- sample(100)
col2 <- sample(100)

a <- cov(x[,col1], y[,col2])
b <- fastcov2(x, y, col1 = col1, col2 = col2)
range(a-b)

microbenchmark::microbenchmark(
  cpp = {
    fastcov2(x, y, col1 = col1, col2 = col2)
  },
  r = {
    cov(x[,col1], y[,col2])
  },
  unit = 'ms', times = 100
)


*/
