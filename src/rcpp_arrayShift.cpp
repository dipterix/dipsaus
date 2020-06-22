#include <cstdlib>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include "utils.h"

using namespace Rcpp;

struct ArrShift : public RcppParallel::Worker
{
  const RcppParallel::RVector<double> x;
  const RcppParallel::RVector<int> dims;
  const int64_t tidx;
  const int64_t sidx;
  const RcppParallel::RVector<int> shift;
  int64_t leap;

  RcppParallel::RVector<double> y;

  ArrShift(
    const Rcpp::NumericVector x,
    const int64_t tidx,
    const int64_t sidx,
    const Rcpp::IntegerVector shift,
    const Rcpp::IntegerVector dims,
    const int64_t leap,
    const Rcpp::NumericVector y
  ): x(x), dims(dims), tidx(tidx), sidx(sidx), shift(shift), leap(leap), y(y){}


  void do_shift(std::size_t begin, std::size_t end){
    std::vector<int64_t> idx = std::vector<int64_t>(dims.length());

    std::size_t ii;
    std::size_t jj;
    int64_t trial, new_t;

    // idx = get_index(begin, dims);
    get_index(idx.begin(), begin, dims);

    idx[0] -= 1;

    // Rcpp::Rcout << "Index is " << idx[0] << std::endl;

    for( ii = begin; ii < end; ii++ ){
      // Calculate current index
      idx[0] = idx[0] + 1;
      for( jj = 0; jj < dims.length() - 1 ; jj++ ){
        if( idx[jj] == dims[jj] ){
          idx[jj] = 0;
          idx[jj + 1] = idx[jj + 1] + 1;
        }
      }

      trial = idx[sidx];

      // shift time index
      new_t = idx[tidx] + shift[trial];

      // check if idx[tidx] is too large or too small
      if( new_t >= 0 && new_t < dims[tidx] ){
        y[ii] = x[ii + shift[trial] * leap];
      }else{
        y[ii] = NA_REAL;
      }


    }
  }


  void operator()(std::size_t begin, std::size_t end) {
    do_shift(begin, end);
  }
};


// [[Rcpp::export]]
Rcpp::NumericVector arrayShift(const Rcpp::NumericVector x,
                         const int64_t tidx,
                         const int64_t sidx,
                         const Rcpp::IntegerVector& shift,
                         const Rcpp::IntegerVector& dims) {

  int64_t len = x.length();
  int64_t leap = 1;
  int64_t jj;

  std::vector<int64_t> idx = std::vector<int64_t>(dims.length());
  idx[0] = -1;
  Rcpp::NumericVector re = Rcpp::NumericVector( len );

  for( jj = 0; jj < tidx; jj++ ){
    leap = leap * dims[jj];
  }

  // int trial, new_t;
  // for( long int ii = 0; ii < len; ii++ ){
  //   // Calculate current index
  //   // idx[0] = idx[0] + 1;
  //   // for( jj = 0; jj < dims.length() - 1 ; jj++ ){
  //   //   if( idx[jj] == dims[jj] ){
  //   //     idx[jj] = 0;
  //   //     idx[jj + 1] = idx[jj + 1] + 1;
  //   //   }
  //   // }
  //   get_index(idx.begin(), ii, dims);
  //
  //   trial = idx[sidx];
  //
  //   // shift time index
  //   new_t = idx[tidx] + shift[trial];
  //
  //   // check if idx[tidx] is too large or too small
  //   if( new_t >= 0 && new_t < dims[tidx] ){
  //     re[ii] = x[ii + shift[trial] * leap];
  //   }else{
  //     re[ii] = NA_REAL;
  //   }
  //
  //
  // }

  ArrShift arrShift(x, tidx, sidx, shift, dims, leap, re);

  // arrShift.do_shift(0, len);
  parallelFor(0, len, arrShift, len / 24);

  re.attr("dim") = dims;
  return re;
}


/*** R
dim = c(100,10,300,20)
x = array(rnorm(prod(dim)), dim)
# time is dim 2
# trial is dim 1
tidx = 3
sidx = 1
shifts = sample(dim[3], dim[1])
shifts[1] = NA
f1 = function(){
  arrayShift(x, 2L, 0L, shifts, dim(x))
}
f2 = function(){
  tm = seq_len(dim[3])
  re = sapply(seq_len(dim[1]), function(ii){
    shift = shifts[ii]
    new_idx = tm + shift
    new_idx[new_idx > dim[3]] = NA
    new_idx[new_idx <= 0] = NA
    x[ii,,new_idx,]
  })
  dim(re) = c(dim(x)[-1], dim[1])
  re = aperm(re, c(4,1,2,3))
  re
}

range(f2()-f1(), na.rm = TRUE)
# microbenchmark::microbenchmark({f1()}, {f2()}, times = 10)
*/
