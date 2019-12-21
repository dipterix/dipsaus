#include "signals.h"

using namespace Rcpp;



// [[Rcpp::export]]
bool guard_length(const size_int n){
  if( n < 0 ){
    stop("window length n is negative");
  }
  return n <= 1;
}


size_int extended_length(size_int n, bool* sym){
  *sym = !(*sym);
  if( *sym ){
    return n + 1;
  }
  return n;
}


template <typename VT>
VT truncate(VT& input, bool trunc){
  VT res;

  if( trunc ){
    if( input.size() == 0 ){
      res = VT(0);
    }else{
      res = VT(input.begin(), input.end() - 1);
    }
  }else{
    res = VT(input.begin(), input.end());
  }
  return(res);
}


template <typename VT>
VT seq2(double from, double to, double by, size_int len){
  if( len == 0 ){
    if( by != 0 ){
      len = (size_int)((to - from) / by) + 1;
    }
  }else{
    by = (to - from) / (len - 1);
  }
  if( len < 0 ){
    stop("seq2: length is negative");
  }
  VT res = VT(len);

  double last_value = from;

  for(size_int ii = 0; ii < res.size(); ii++){
    res[ii] = last_value;
    last_value += by;
  }
  return res;
}


// [[Rcpp::export]]
NumericVector cosine_gen(size_int n, NumericVector a, const bool sym){
  if( guard_length(n) ){
    // return np.ones(M)
    return seq2<NumericVector>(1, 1, 0, n);
  }

  // needs_truncate will change
  bool needs_truncate = sym;
  size_int len = extended_length( n, &needs_truncate );

  NumericVector x = seq2<NumericVector>(-PI, PI, 0, len);
  NumericVector res = seq2<NumericVector>(0, 0, 0, len);

  for(size_int i = 0; i < a.size(); i++){
    res += a[i] * cos(x * i);
  }


  return truncate<NumericVector>(res, needs_truncate);

}

// [[Rcpp::export]]
NumericVector hamming_gen(const size_int n, double alpha, bool sym){
  NumericVector a = NumericVector(2);
  a[0] = alpha;
  a[1] = 1.0 - alpha;
  return cosine_gen(n, a, sym);
}

// [[Rcpp::export]]
NumericVector hann(const size_int n, bool sym){
  return hamming_gen(n, 0.5, sym);
}

// [[Rcpp::export]]
NumericVector hanning(const size_int n){
  return hann(n, true);
}


// [[Rcpp::export]]
NumericVector detrend_naive(const NumericVector& y, NumericVector input){
  // res - input * a - b
  const int n = input.size();
  if(n == 0 && y.size() > 0){
    input = seq_len(y.size());

    return detrend_naive(y, as<NumericVector>(input));
  }
  if(n != y.size()){
    stop("De-trend error: res and input must have the same length.");
  }
  if( n == 0 ){
    return NumericVector(0);
  }
  const double b = (y[n - 1] - y[0]) / (input[n-1] - input[0]);
  const double a = y[0] - b * input[0];
  return y - a - b * input;
}








// [[Rcpp::export]]
void test_dipsaus_signals(){
  size_int n = 1;
  bool sym = true;
  size_int n2 = extended_length(n, &sym);

  if( n != 1 ){
    stop("[test_dipsaus_signals]: n is not 1");
  }
  if( n2 != 1 ){
    stop("[test_dipsaus_signals]: n2 is not 1");
  }
  if( sym ){
    stop("[test_dipsaus_signals]: sym should be changed to false not true is found");
  }

  NumericVector s = NumericVector(10);
  NumericVector res = truncate<NumericVector>(s, true);
  if(res.size() != 9){
    stop("[test_dipsaus_signals] truncate not doing its work");
  }
  s[0] = 100;
  Rcout << res[0] << '\n';
}





/*** R
require(testthat)
expect_true(guard_length(0))
expect_true(guard_length(1))
expect_false(guard_length(2))
expect_error(guard_length(-1))
expect_lt(max(abs(hanning(11) - (0.5 - 0.5 * cos (2 * pi * seq(0, 10) / (10))))), 1e-10)
expect_lt(sum(detrend_naive(1:100, numeric(0))^2), 1e-10)

test_dipsaus_signals()


# x <- as.vector(x)
# x_len = length(x)
#
# nfft = max(min(nfft, length(x)), window)
#
# window <- hanning(window)
#
# # window_norm = norm(window, '2')
# window_len <- length(window)
#
# # normalization <- mean(window^2)
#
# step <- max(floor(window_len - noverlap + 0.99), 1)
#
# ## Average the slices
# offset = seq(1, x_len-window_len+1, by = step)
#
# N = length(offset);
#
# sapply(seq_len(N), function(i){
#   a = detrend_naive(x[offset[i] - 1 + seq_len(window_len)])
#   a = fftwtools::fftw_r2c(postpad(a$Y * window, nfft))
#   Mod(a)^2
# }) ->
#   re
#
# NN = floor((nfft + 1) / 2)
#
# freq = seq(1, fs / 2, length.out = NN)
# spec = rowMeans(re[seq_len(NN),,drop = F]) / (window_len / 2)^2
#
#
# res = list(
#   freq = freq,
#   spec = spec,
#   method = "Welch"
# )
*/

