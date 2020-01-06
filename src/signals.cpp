
#include "signals.h"


using namespace Rcpp;



// [[Rcpp::export]]
bool guard_length(const R_xlen_t n){
  if( n < 0 ){
    stop("window length n is negative");
  }
  return n <= 1;
}


R_xlen_t extended_length(R_xlen_t n, bool* sym){
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
VT seq2(double from, double to, double by, R_xlen_t len){
  if( len == 0 ){
    if( by != 0 ){
      len = (R_xlen_t)((to - from) / by) + 1;
    }
  }else{
    by = (to - from) / (len - 1);
  }
  if( len < 0 ){
    stop("seq2: length is negative");
  }
  VT res = VT(len);

  double last_value = from;

  for(R_xlen_t ii = 0; ii < res.size(); ii++){
    res[ii] = last_value;
    last_value += by;
  }
  return res;
}


// [[Rcpp::export]]
NumericVector cosine_gen(R_xlen_t n, NumericVector a, const bool sym){
  if( guard_length(n) ){
    // return np.ones(M)
    return seq2<NumericVector>(1, 1, 0, n);
  }

  // needs_truncate will change
  bool needs_truncate = sym;
  R_xlen_t len = extended_length( n, &needs_truncate );

  NumericVector x = seq2<NumericVector>(-PI, PI, 0, len);
  NumericVector res = seq2<NumericVector>(0, 0, 0, len);

  for(R_xlen_t i = 0; i < a.size(); i++){
    res += a[i] * cos(x * i);
  }


  return truncate<NumericVector>(res, needs_truncate);

}

// [[Rcpp::export]]
NumericVector hamming_gen(const R_xlen_t n, double alpha, bool sym){
  NumericVector a = NumericVector(2);
  a[0] = alpha;
  a[1] = 1.0 - alpha;
  return cosine_gen(n, a, sym);
}

// [[Rcpp::export]]
NumericVector hann(const R_xlen_t n, bool sym){
  return hamming_gen(n, 0.5, sym);
}

// [[Rcpp::export]]
NumericVector hanning(const R_xlen_t n){
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

template <typename VT>
VT postpad(VT& x, const int n){
  VT y;
  if(x.size() >= n){
    y = VT(x.begin(), x.begin() + (n - 1));
  }else{
    y = VT(n);
    R_xlen_t ii = 0;
    for( ; ii < x.size(); ii++ ){
      y[ii] = x[ii];
    }
    for( R_xlen_t ii = x.size(); ii < n; ii++ ){
      y[ii] = 0;
    }
  }
  return y;
}


// [[Rcpp::export]]
ComplexVector fftw_r2c(const R_xlen_t N, NumericVector data, bool retConj, bool gc) {
  R_xlen_t n = N;
  R_xlen_t nc = n / 2 + 1;
  R_xlen_t i;

  fftw_plan p;

  fftw_complex *out = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * N);
  ComplexVector res = ComplexVector(N);

  p = fftw_plan_dft_r2c_1d(n, &(data[0]), out, FFTW_ESTIMATE);

  fftw_execute(p);
  fftw_destroy_plan(p);

  // Convert res to ComplexVector

  if( retConj ) {
    for(i = nc; i < n; i++ ) {
      out[i][0] = out[n - i][0];
      out[i][1] = -out[n - i][1];
    }
  }
  for(i = 0; i < n; i++ ) {
    res[i].r = out[i][0];
    res[i].i = out[i][1];
  }

  fftw_free(out);
  if( gc ){
    fftw_cleanup();
  }

  return res;
}


ComplexVector fftw_c2c(const R_xlen_t N, const ComplexVector& data, bool reverse, bool gc = true){
  R_xlen_t n = N;
  // R_xlen_t nc = n / 2 + 1;
  R_xlen_t i;

  fftw_plan p;

  fftw_complex *out = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * N);
  fftw_complex *inp = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * N);

  // Cannot take advantage like r2c, we have to copy data
  for(i = 0; (i < N) && (i < data.size()); i++ ){
    inp[i][0] = data[i].r;
    inp[i][1] = data[i].i;
  }

  ComplexVector res = ComplexVector(N);
  unsigned sign = FFTW_FORWARD;
  if( reverse ){
    sign = FFTW_BACKWARD;
  }

  p = fftw_plan_dft_1d(n, inp, out, sign, FFTW_ESTIMATE);

  fftw_execute(p);
  fftw_destroy_plan(p);

  // Convert res to ComplexVector
  for(i = 0; i < n; i++ ) {
    res[i].r = out[i][0];
    res[i].i = out[i][1];
  }

  fftw_free(out);
  if( gc ){
    fftw_cleanup();
  }

  return res;
}

void safe_fftw_r2c(const R_xlen_t N, RcppParallel::RVector<double>::iterator data,
                   bool retConj, RcppParallel::RVector<double>::iterator resReal,
                   RcppParallel::RVector<double>::iterator resImag) {
  R_xlen_t n = N;
  R_xlen_t nc = n / 2 + 1;
  R_xlen_t i;

  fftw_complex *out = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * N);

  fftw_plan p = fftw_plan_dft_r2c_1d(n, &(*data), out, FFTW_ESTIMATE);

  fftw_execute(p);

  // Convert res to ComplexVector

  if( retConj ) {
    for(i = nc; i < n; i++ ) {
      out[i][0] = out[n - i][0];
      out[i][1] = -out[n - i][1];
    }
  }
  for(i = 0; i < n; i++, resReal++, resImag++ ) {
    *resReal = *(out + i)[0];
    *resImag = *(out + i)[1];
  }

  fftw_destroy_plan(p);
  fftw_free(out);

  // To make it faster, the trick is do not free out (reuse)
  // and do not clean up until the last steps.
  // fftw_cleanup();
}

// [[Rcpp::export]]
void test_dipsaus_signals(){
  R_xlen_t n = 1;
  bool sym = true;
  R_xlen_t n2 = extended_length(n, &sym);

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

set.seed(1)
x = rnorm(50000)
microbenchmark::microbenchmark({fftw_r2c(length(x), x, TRUE, TRUE)},{fftwtools::fftw_r2c(x)}, times = 3L)

res = rave:::pwelch(x, 1000, window = 64, noverlap = 8, nfft = 256, plot = F)

res2 = pwelch(x, 1000, 64, 8, 256)

microbenchmark::microbenchmark({
  rave:::pwelch(x, 1000, window = 64, noverlap = 8, nfft = 256, plot = F)
}, {
  pwelch(x, 1000, 64, 8, 256)
}, times = 3)

*/

