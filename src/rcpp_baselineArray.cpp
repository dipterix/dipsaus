#include "utils.h"
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;

struct Baseliner : public RcppParallel::Worker
{
  const RcppParallel::RVector<double> x;
  const RcppParallel::RVector<int> dims;
  RcppParallel::RVector<int> dat_vec_idx;
  const Rcpp::NumericVector bl;
  const RcppParallel::RVector<int> bldims;
  RcppParallel::RVector<int> bl_vec_idx;
  const RcppParallel::RVector<int> per;
  const RcppParallel::RVector<int> per_dim;
  const int method;
  const int64_t blloop_len;
  const int64_t innerloop_len;

  RcppParallel::RVector<double> y;

  Baseliner(
    const Rcpp::NumericVector x,
    const Rcpp::IntegerVector dims,
    const Rcpp::IntegerVector dat_vec_idx,
    const Rcpp::NumericVector bl,
    const Rcpp::IntegerVector bldims,
    const Rcpp::IntegerVector bl_vec_idx,
    const Rcpp::IntegerVector per,
    const Rcpp::IntegerVector per_dim,
    const int method,
    const int64_t blloop_len,
    const int64_t innerloop_len,
    const Rcpp::NumericVector y
  ):x(x), dims(dims), dat_vec_idx(dat_vec_idx),
    bl(bl), bldims(bldims), bl_vec_idx(bl_vec_idx),
    per(per), per_dim(per_dim), method(method),
    blloop_len(blloop_len), innerloop_len(innerloop_len), y(y){}

  void do_baseline(std::size_t begin, std::size_t end){
    std::vector<int64_t> per_idx = std::vector<int64_t>(per_dim.length());
    std::vector<int64_t> subset_idx = std::vector<int64_t>(dims.length());
    // per_idx.fill(0);
    // subset_idx.fill(0);

    int64_t dat_partial_ii, bl_partial_ii;
    double bl_mean = 0;
    double bl_sd = 0;
    int64_t bl_len = bl_vec_idx.length();
    std::vector<double> bl_container = std::vector<double>(bl_len);

    std::size_t tmp_idx, arr_idx;
    std::vector<double>::iterator ptr_std_int_1;
    Rcpp::IntegerVector::iterator ptr_cpp_int_1;

    for( std::size_t ii = begin; ii < end; ii++ ){

      get_index(per_idx.begin(), ii, per_dim);

      // Rcout << Rcpp::wrap(per_idx) << '\n';

      // subset_idx[per] = per_idx;
      for(tmp_idx = 0; tmp_idx < per.length(); tmp_idx++){
        subset_idx[per[tmp_idx]] = per_idx[tmp_idx];
      }

      // Calculate another partial of the index
      bl_partial_ii = get_ii(subset_idx, bldims);
      dat_partial_ii = get_ii(subset_idx, dims);


      // Calculate baseline mean
      //
      // bl_idx_tmp = bl_vec_idx + bl_partial_ii;
      // bl_container = bl[bl_idx_tmp];
      ptr_cpp_int_1 = bl_vec_idx.begin();
      ptr_std_int_1 = bl_container.begin();
      for(; ptr_cpp_int_1 != bl_vec_idx.end(); ptr_cpp_int_1++ ){
        *ptr_std_int_1 = bl[ *ptr_cpp_int_1 + bl_partial_ii ];
        ptr_std_int_1++;
      }

      ptr_cpp_int_1 = dat_vec_idx.begin();
      switch(method){
      case 0: // 0: direct baseline

        bl_mean = std::accumulate( bl_container.begin(), bl_container.end(), 0.0) / bl_len;
        // Element-wise (e1 / e2 - 1) * 100
        for(; ptr_cpp_int_1 != dat_vec_idx.end(); ptr_cpp_int_1++ ){
          arr_idx = *ptr_cpp_int_1 + dat_partial_ii;
          y[ arr_idx ] = (x[ arr_idx ] / bl_mean - 1.0) * 100.0;
        }
        break;

      case 1: // 1. sqrt then baseline
        bl_mean = std::accumulate( bl_container.begin(), bl_container.end(), 0.0, add_sqrt) / bl_len;
        // Element-wise (e1 / e2 - 1) * 100
        for(; ptr_cpp_int_1 != dat_vec_idx.end(); ptr_cpp_int_1++ ){
          arr_idx = *ptr_cpp_int_1 + dat_partial_ii;
          y[ arr_idx ] = (std::sqrt(x[ arr_idx ]) / bl_mean - 1.0) * 100.0;
        }
        break;

      case 2: // 2. 10*log10 then baseline
        bl_mean = std::accumulate( bl_container.begin(), bl_container.end(), 0.0, add_log10) / bl_len;
        // Element-wise (e1 - e2)
        for(; ptr_cpp_int_1 != dat_vec_idx.end(); ptr_cpp_int_1++ ){
          arr_idx = *ptr_cpp_int_1 + dat_partial_ii;
          y[ arr_idx ] = (std::log10(x[ arr_idx ]) - bl_mean) * 10.0;
        }
        break;


      case 3: // 3. z-score then baseline
        bl_mean = std::accumulate( bl_container.begin(), bl_container.end(), 0.0) / bl_len;
        bl_sd = std::accumulate( bl_container.begin(), bl_container.end(), 0.0, add_square) / bl_len;
        bl_sd = std::sqrt( (bl_sd - bl_mean*bl_mean) / ( bl_len - 1 ) * bl_len );
        // Element-wise (e1 - e2) / sd
        for(; ptr_cpp_int_1 != dat_vec_idx.end(); ptr_cpp_int_1++ ){
          arr_idx = *ptr_cpp_int_1 + dat_partial_ii;
          y[ arr_idx ] = (x[ arr_idx ] - bl_mean) / bl_sd;
        }
        break;


      case 4: // 4. sqrt,z-score and baseline
        bl_mean = std::accumulate( bl_container.begin(), bl_container.end(), 0.0, add_sqrt) / bl_len;
        bl_sd = std::accumulate( bl_container.begin(), bl_container.end(), 0.0) / bl_len;
        bl_sd = std::sqrt( (bl_sd - bl_mean*bl_mean) / ( bl_len - 1 ) * bl_len );
        // Element-wise (e1 - e2) / sd
        for(; ptr_cpp_int_1 != dat_vec_idx.end(); ptr_cpp_int_1++ ){
          arr_idx = *ptr_cpp_int_1 + dat_partial_ii;
          y[ arr_idx ] = (std::sqrt(x[ arr_idx ]) - bl_mean) / bl_sd;
        }
        break;

      }

    }
  }

  void operator()(std::size_t begin, std::size_t end) {
    do_baseline(begin, end);
  }

};


// [[Rcpp::export]]
Rcpp::NumericVector baselineArray(
    const Rcpp::NumericVector &x,
    const Rcpp::NumericVector &bl,
    const Rcpp::IntegerVector dims,
    const Rcpp::IntegerVector bldims,
    const int tidx,
    const Rcpp::IntegerVector &per,
    const Rcpp::IntegerVector &rest,
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

  // Rcpp::Rcout << "cpp debug baseline\n";

  Rcpp::IntegerVector::iterator ptr;
  Rcpp::NumericVector y = Rcpp::NumericVector(x.length());
  y.attr("dim") = x.attr("dim");
  y.attr("dimnames") = x.attr("dimnames");

  // Rcpp::Rcout << "cpp debug 1\n";

  // Generate subset index
  Rcpp::IntegerVector per_dim = dims[per];
  int64_t loop_len = length_from_dim(per_dim);

  Rcpp::IntegerVector subset_idx = Rcpp::IntegerVector(dims.length());
  Rcpp::IntegerVector per_idx = Rcpp::IntegerVector(per_dim.length());

  // Rcpp::Rcout << "cpp debug subset index generated\n";

  // Callculate baseline inner loop length
  Rcpp::IntegerVector rest_dim = dims[rest];
  int64_t innerloop_len = length_from_dim(rest_dim);
  int64_t blloop_len = length_from_dim(bldims[rest]);
  Rcpp::IntegerVector rest_idx = Rcpp::IntegerVector(rest_dim.length());

  // Rcpp::Rcout << "cpp debug calculated innerloop length\n";

  // calculate baseline indices in vector (partial index)
  Rcpp::IntegerVector bl_vec_idx = Rcpp::IntegerVector(blloop_len);
  rest_idx.fill(0);
  rest_idx[0] = -1;
  subset_idx.fill(0);
  for(ptr = bl_vec_idx.begin(); ptr != bl_vec_idx.end(); ptr++ ){
    next_array_index(rest_idx.begin(), rest_idx.end(), rest_dim.begin());
    subset_idx[rest] = rest_idx;
    *ptr = get_ii(subset_idx, bldims);
  }

  // Rcpp::Rcout << "cpp debug baseline indives calculated\n";

  // calculate data indices in vector (partial index)
  Rcpp::IntegerVector dat_vec_idx = Rcpp::IntegerVector(innerloop_len);
  rest_idx.fill(0);
  rest_idx[0] = -1;
  subset_idx.fill(0);
  for(ptr = dat_vec_idx.begin(); ptr != dat_vec_idx.end(); ptr++ ){
    next_array_index(rest_idx.begin(), rest_idx.end(), rest_dim.begin());
    subset_idx[rest] = rest_idx;
    *ptr = get_ii(subset_idx, dims);
  }

  // Rcpp::Rcout << "cpp debug partial index calculated\n";

  // subset_idx.fill(0);
  //
  // int64_t dat_partial_ii, bl_partial_ii;
  // double bl_mean = 0;
  // NumericVector bl_container = NumericVector(blloop_len);
  // NumericVector dat_container = NumericVector(innerloop_len);
  // IntegerVector dat_idx_tmp = IntegerVector(innerloop_len);
  // for( int64_t ii = 0; ii < loop_len; ii++ ){
  //   get_index(per_idx.begin(), ii, per_dim);
  //   subset_idx[per] = per_idx;
  //   // Calculate another partial of the index
  //   bl_partial_ii = get_ii(subset_idx, bldims);
  //   dat_partial_ii = get_ii(subset_idx, dims);
  //
  //
  //   // Calculate baseline mean
  //   bl_container = bl[bl_vec_idx + bl_partial_ii];
  //   // Rcout << ii <<'\n';
  //   bl_mean = Rcpp::mean(bl_container);
  //
  //   dat_idx_tmp = dat_vec_idx + dat_partial_ii;
  //   dat_container = x[dat_idx_tmp];
  //   dat_container = (dat_container / bl_mean - 1.0) * 100.0;
  //
  //   y[dat_idx_tmp] = dat_container;
  // }

  Baseliner baseliner(x, dims, dat_vec_idx,
                      bl, bldims, bl_vec_idx,
                      per, per_dim, method,
                      blloop_len, innerloop_len, y);

  // Rcpp::Rcout << "cpp debug baseliner created\n";

  // Last parameter - grain size is very important
  // When data is large, or some recursive calls,
  // C stack will goes to the limit quickly
  // Setting x.size() / 500 will allow a block size not small such that
  // there will only be 500 threads created
  parallelFor(0, loop_len, baseliner, loop_len / 24);
  // baseliner.do_baseline(0, loop_len);

  // Rcpp::Rcout << "cpp debug done\n";

  return y;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
ravebuiltins::dev_ravebuiltins(F,F)
rave::mount_demo_subject()
power = module_tools$get_power()$subset(Electrode = Electrode == 14)

power$hybrid = FALSE
junk = power$get_data()

f1 = function(){
  rave::baseline(power, from = -1, to = 0, data_only = TRUE, hybrid = FALSE, mem_optimize = FALSE)
}

f2 = function(method = 0L){
  x = power$get_data()
  bl = x[,,seq_len(101),,drop=FALSE]
  baselineArray(x, bl, dim(x), dim(bl), 2L, c(0L, 1L, 3L), c(2L), method)
}

f3 = function(){
  x = power$get_data()
  aperm(apply(x, c(1,2,4), function(y){
    m = mean(y[seq_len(101)])
    (y/m - 1) * 100
  }), c(2,3,1,4))
}
# f3 = function(){
#   x = sqrt(power$get_data())
#   re = apply(x, c(1,2,4), function(y){
#     m = mean(y[seq_len(101)])
#     (y - m) / sd(y[seq_len(101)])
#   })
#   aperm(re, c(2,3,1,4))
# }
range(f3() - f1())
range(f1() - f2())
range(f3() - f2())
microbenchmark::microbenchmark(f1(),f3(),f2(0L),f2(1L),f2(2L),f2(3L),f2(4L), times = 20)

# dif = f2() - f1()
#
#
# fields::image.plot(z=dif[,,105,1],x=1:287, y=1:16)

#
#
# dim = c(10,10,100,10)
# varnames = c("Trial","Frequency", "Time","Electrode")
# dimnames = lapply(dim, seq_len); names(dimnames) = varnames
# x = array(x, dim, dimnames)
# p = rave::ECoGTensor$new(data = x, dim(x), dimnames = dimnames, varnames = varnames)
# re1 = rave::baseline(p, from = 0, to = 10)
#
# bl = x[,,1:10,, drop=FALSE]
# re2 = baselineArray(x, bl, dim(x), dim(bl), 2L, c(0L,1L,3L), 2L, 0L)
#
# range(re1$get_data() - re2)
#
#
#
# x = array(1:100, c(2,25,2))
# x[2,1:10,] = 1
# x[1,1:10,] = 1
# bl = x[,1:10, ,drop = FALSE]
# # const int tidx,
# # const Rcpp::IntegerVector &per,
# # const Rcpp::IntegerVector &rest,
# # const int method
# baselineArray(x, bl, dim(x), dim(bl), 1L, c(0L,2L), 1L, 0L) / 100
*/
