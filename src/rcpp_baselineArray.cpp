#include <cstdlib>
#include <Rcpp.h>
#include "utils.h"
using namespace Rcpp;



// [[Rcpp::export]]
Rcpp::NumericVector baselineArray(
    const Rcpp::NumericVector &x,
    const Rcpp::IntegerVector &dims,
    const int tidx,
    const int tbegin,
    const int tend,
    const Rcpp::IntegerVector &per,
    const Rcpp::IntegerVector &perDims,
    const int method
  ) {
  // For example,
  // tidx = 3, dims = 287 x 16 x 301 x 2
  // per = c(1,4), perDims = c(287, 2)
  // tbegin = 0, tend = 102
  // method:
  // 1: direct baseline
  // 2. sqrt then baseline
  // 3. 10*log10 then baseline
  // 4. z-score then baseline
  // 5. median as baseline (???)

  // sequence along perDims
  // long int ii;
  // std::vector<int> iterIdx = std::vector<int>(perDims.length());
  // Rcpp::IntegerVector idx = Rcpp::IntegerVector(dims.length());
  //
  // // 1. calculate loop index range (0 - prod(perDims))
  // const long int idxLen = std::accumulate(perDims.begin(),
  //                                         perDims.end(), 1,
  //                                         std::multiplies<int>());


  // for( ii=0; ii < idxLen; ii++ ){
  //   get_index(iterIdx.begin(), ii, perDims);
  //
  //
  //
  // }


  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
ravebuiltins::dev_ravebuiltins(F,F)
rave::mount_demo_subject()
power = module_tools$get_power()

bl = rave::baseline(power, from = -1, to = 0)
*/
