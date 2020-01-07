#include <cstdlib>
#include <Rcpp.h>
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

using namespace Rcpp;
// using namespace RcppParallel;

struct Collapse : public RcppParallel::Worker
{
  const Rcpp::NumericVector x;
  const Rcpp::IntegerVector dims;
  const Rcpp::IntegerVector keep;
  const Rcpp::IntegerVector remain;
  const int total_dim;
  const int out_dim;

  RcppParallel::RVector<double> y;

  Collapse(
    const Rcpp::NumericVector x,
    const Rcpp::IntegerVector dims,
    const Rcpp::IntegerVector keep,
    const Rcpp::IntegerVector remain,
    const int total_dim,
    const int out_dim,
    const Rcpp::NumericVector y
  ): x(x), dims(dims), keep(keep), remain(remain), total_dim(total_dim), out_dim(out_dim), y(y){}

  void operator()(std::size_t begin, std::size_t end) {
    const int input_size = dims.size();
    int *input_ind = new int[input_size];
    // int input_ind[input_size];
    int rem_dim = total_dim / out_dim;

    unsigned int i;
    int j, k;
    int l,p,a,b,c;    // indices
    double re;

    for(i = begin; i < end; i++){
      /*
       * i is index for output[a,b,c] -> i = a + b * dim[1] + c * prod(dim[1,2]) -> output[i]
       * output[a,b,c] = sum(input[,a,,b,c]), if keep = c(2,4,5)
       */
      // Calculate output index
      a = i;
      re = 0;
      for(j = 0; j < keep.size(); j++){
        /*
        * i -> coord in y
        * coord_1 = i % dims[0]
        * res_1 = (i - coord_y1) / dims[keep[0]]
        * coord_y2 = res_1 % dims[keep[1]]
        * res_2 = (res_1 - coord_y2) / dims[keep[1]]
        * coord_y3 = res_2 / dims[keep[2]] % dims[keep[2]]
        * ...
        */
        input_ind[keep[j]-1] = a % dims[keep[j]-1];
        a = (int)(a / dims[keep[j]-1]);
      }

      for(k = 0; k < rem_dim; k++ ){
        b = k;
        for(l = 0; l < remain.size(); l++){
          input_ind[remain[l]] = b % dims[remain[l]];
          b = (int)(b / dims[remain[l]]);
        }

        // For X[input_ind], add to re
        c = 0;
        for(p = input_size-1; p >= 0 ; p--){
          c += input_ind[p] + c * (dims[p] - 1);
        }
        if(::R_finite(x[c])){
          re += x[c];
        }else{
          re += NA_REAL;
        }


      }

      /*
       * Replace output[i] with re;
       */
      y[i] = re;
      // std::printf("%.2f", re);
    }

    delete [] input_ind;
  }

};


// [[Rcpp::export]]
Rcpp::NumericVector collapser(
    Rcpp::NumericVector x, Rcpp::IntegerVector dims, Rcpp::IntegerVector keep)
{
  // Generate template output
  int len = 1;
  for(int i=0; i<keep.length(); i++){
    len *= dims[keep[i]-1];
  }
  Rcpp::NumericVector re(len);

  // Calculate total dim
  int total_dim = 1;
  for(int i=0; i<dims.length(); i++){
    total_dim *= dims[i];
  }

  // Calculate remaining dimensions
  int remsize = dims.size() - keep.size();
  bool is_in;
  Rcpp::IntegerVector remain(remsize);
  for(int64_t j = dims.size(); j > 0; j-- ){
    is_in = std::find(keep.begin(), keep.end(), j) != keep.end();
    if(!is_in){
      remain[--remsize] = j - 1;
    }
  }


  Collapse collapse(x, dims, keep, remain, total_dim, len, re);

  parallelFor(0, len, collapse);

  return(re);
  // return(Rcpp::as<Rcpp::NumericVector>(Rcpp::NumericVector::create(len)));
}

/*** R
RcppParallel::setThreadOptions(numThreads = 4)
dat = array(1:16, c(4,4))
dat[1,1] = NA
dat[2,1] = Inf
dat[3,1] = NaN
re = collapser(dat, dim(dat), 1); re
rowSums(dat)
# dat = array(rnorm(240), c(300,200,105,1))
# re = collapser(dat, dim(dat), 1:2); dim(re) = dim(dat)[1:2]; re
# re0 = apply(dat, c(1,2), sum); re0
# delta = re-re0; range(delta)
# # image(delta)
# # plot(as.vector(delta))
#
# dat = force(dat)
#
#
# # peformance
# microbenchmark::microbenchmark(
#   {re = collapser(dat, dim(dat), 1:2); dim(re) = dim(dat)[1:2]; re},
#   collapse(dat, c(1,2)),
#   apply(dat, c(1,2), sum),
#   times = 10L
# )
#
# # accuracy
# acc = replicate(300, {
#   dim = sample(1:40, 4, replace = T)
#   dat = array(rnorm(prod(dim)), dim)
#   keep = sample(4, 2)
#   s0 = Sys.time()
#   re = collapse(dat, keep)
#   s1 = Sys.time()
#   re0 = apply(dat, keep, sum);
#   s2 = Sys.time()
#   c(sum(abs(re-re0)), as.numeric(s2 - s0) , as.numeric(s1 - s0))
# })
# hist(acc[1,])
# summary(acc[2,] * 1000)
# summary(acc[3,] * 1000)

*/
