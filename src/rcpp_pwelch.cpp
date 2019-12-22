#include "signals.h"

using namespace Rcpp;

struct PwelchWorker : public RcppParallel::Worker{
  const RcppParallel::RVector<double> x;
  const RcppParallel::RVector<double> window;
  const RcppParallel::RVector<int> offsets;
  R_xlen_t nfft;
  const double normalize;
  const R_xlen_t grain_size;
  const R_xlen_t nthreads;

  RcppParallel::RVector<double> y;

  fftw_complex **out;
  double **tmp;
  fftw_plan *plans;
  bool scheduled = false;

  PwelchWorker(
    const Rcpp::NumericVector x,
    const Rcpp::NumericVector window,
    const Rcpp::IntegerVector offsets,
    const R_xlen_t nfft,
    const double normalize,
    const R_xlen_t grain_size,
    const R_xlen_t nthreads,
    const R_xlen_t NN
  ) : x(x), window(window), offsets(offsets), nfft(nfft), normalize(normalize),
    grain_size(grain_size), nthreads(nthreads), y(NumericVector(NN)) {
    schedule_plan();
  }
  PwelchWorker(
    const PwelchWorker& w, RcppParallel::Split
  ) : x(w.x), window(w.window), offsets(w.offsets), nfft(w.nfft), normalize(w.normalize),
    grain_size(w.grain_size), nthreads(w.nthreads), y(NumericVector(w.y.size())),
    out(w.out), tmp(w.tmp), plans(w.plans), scheduled(w.scheduled) {
    if(!scheduled){
      stop("plans are not scheduled?");
    }
  }
  void join(const PwelchWorker& rhs) {
    for(R_xlen_t j = 0; j<y.size(); j++ ){
      y[j] += rhs.y[j];
    }
  }


  void schedule_plan(){
    if(scheduled){
      stop("FFTW plans have already been scheduled.");
    }
    scheduled = true;
    R_xlen_t n = nfft;

    // malloc array pointers
    out = (fftw_complex **) malloc(sizeof(fftw_complex*) * nthreads);
    tmp = (double **) malloc(sizeof(double*) * nthreads);
    plans = (fftw_plan *) malloc(sizeof(fftw_plan) * nthreads);

    for(unsigned int i = 0; i < nthreads; i++){
      *(out + i) = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * nfft);
      *(tmp + i) = (double*) malloc(sizeof(double) * nfft);
      *(plans + i) = fftw_plan_dft_r2c_1d(n, *(tmp + i), *(out + i), FFTW_ESTIMATE);
    }
    // out = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * nfft);
    // tmp = (double*) malloc(sizeof(double) * nfft);
  }

  void destroy_plan(){

    for(unsigned int i = 0; i < nthreads; i++){
      fftw_destroy_plan(*(plans + i));
      fftw_free(*(out + i));
      free(*(tmp + i));

    }
    free(out);
    free(tmp);
    free(plans);
    plans = NULL;
    out = NULL;
    tmp = NULL;
    fftw_cleanup();

    scheduled = false;
  }

  // begin - end are threads. no load balance
  void work(R_xlen_t begin, R_xlen_t end){
    const R_xlen_t winLen = window.size();
    R_xlen_t i, j, k, th;

    double a, b;
    double* y_partial = (double*) malloc(sizeof(double) * y.size());
    for(j = 0; j < y.size(); j++){
      y_partial[j] = 0;
    }

    // This block is ran in single thread
    // Thread th
    for(th = begin; th < end; th++ ){
      // Rcout << "Current Thread: " << th << "\n";
      // Rcout << "i start from: " << th*grain_size << " - " << (th*grain_size+grain_size-1) << "\n";


      for( i = th*grain_size; i < (th*grain_size+grain_size); i++ ){
        if( i < offsets.size() ){
          k = offsets[i];

          // detrend
          b = (x[k+winLen-1] - x[k]) / (winLen - 1);
          a = x[k];

          if(nfft > winLen){
            for(j = 0; j < winLen; j++ ){
              *(*(tmp + th)+j) = (x[j + k] - a - b * j) * window[j];
              (*(*(out + th) + j))[0] = 0;
              (*(*(out + th) + j))[1] = 0;
            }
            for(j = winLen; j < nfft; j++){
              *(*(tmp + th)+j) = 0;
              (*(*(out + th) + j))[0] = 0;
              (*(*(out + th) + j))[1] = 0;
            }
          }else{
            for(j = 0; j < nfft; j++ ){
              *(*(tmp + th)+j) = (x[j + k] - a - b * j) * window[j];
              (*(*(out + th) + j))[0] = 0;
              (*(*(out + th) + j))[1] = 0;
            }
          }

          // if(i== 99){
          //   Rcout << (*(tmp + th))[1] << ' ' << (*(tmp + th))[2] << '\n';
          // }

          // fftw_plan p = fftw_plan_dft_r2c_1d(n, tmp, out, FFTW_ESTIMATE);
          // fftw_execute_dft_r2c(*plans, *(tmp + th), *(out + th));
          fftw_execute(*(plans+th));

          // if(i== 10009){
          //   for(j = 0; j < y.size(); j++){
          //     Rcout << (*(out + th))[j][0] << " + " << (*(out + th))[j][1] << "i,\n";
          //   }
          //
          // }

          for(j = 0; j < y.size(); j++){

            // y[j] += (
            //   (*(*(out + th) + j))[0] * (*(*(out + th) + j))[0] +
            //     (*(*(out + th) + j))[1] * (*(*(out + th) + j))[1]
            // ) / normalize;
            y_partial[j] += (
                (*(*(out + th) + j))[0] * (*(*(out + th) + j))[0] +
                  (*(*(out + th) + j))[1] * (*(*(out + th) + j))[1]
              ) / normalize;


            // *ptr_y = ((*out_ptr)[0] * (*out_ptr)[0] + (*out_ptr)[1] * (*out_ptr)[1]) / normalize;
            // y[j] += (output[j][0] * output[j][0] + output[j][1] * output[j][1]) / normalize;
            // y[j] += out1[j].r * out1[j].r + out1[j].i * out1[j].i;
            // y[j] += (out1[j]) * (out1[j]) + (out2[j]) * (out2[j]);
          }

          // Rcout << "Finished y\n";
        }


      }


    }


    for(j = 0; j < y.size(); j++){
      y[j] += y_partial[j];
    }

    free(y_partial);
    y_partial = NULL;

  }


  void operator()(std::size_t begin, std::size_t end){
    work(begin, end);
  }

};



// [[Rcpp::export]]
NumericVector pwelch(NumericVector x, double fs, R_xlen_t winLen,
                     R_xlen_t noverlap, R_xlen_t nfft, const R_xlen_t nthread){
  R_xlen_t x_len = x.size();
  R_xlen_t i, j;

  nfft = std::max(std::min(nfft, x_len), winLen);
  NumericVector window = hanning(winLen);
  winLen = window.size();

  R_xlen_t step = winLen - noverlap;
  if( step < 1 ){
    step = 1;
  }

  R_xlen_t nsamples = (x_len - winLen);
  if(nsamples % step == 0){
    nsamples = nsamples / step + 1;
  }else{
    nsamples = (nsamples - (nsamples % step)) / step + 1;
  }

  IntegerVector offsets = IntegerVector(nsamples);
  j = 0;
  for( i = 0; i < nsamples; i++ ){
    offsets[i] = j;
    j += step;
  }

  R_xlen_t NN;
  if(nfft % 2){
    NN = (nfft + 1) / 2;
  }else{
    NN = nfft / 2;
  }


  // NumericVector res = NumericVector(NN);
  NumericVector freq = seq2<NumericVector>(1, fs / 2.0, 0, NN);


  double halfWindLen = winLen / 2.0;

  R_xlen_t grain_size = nsamples % nthread;
  if( grain_size == 0 ){
    grain_size = nsamples / nthread;
  }else{
    grain_size = (nsamples - grain_size) / nthread + 1;
  }

  double normal = halfWindLen * halfWindLen * nsamples;
  // Rcout << normal << "\n";
  PwelchWorker worker(x, window, offsets, nfft,
                      normal,
                      grain_size, nthread, NN);


  // Rcout << "Thread: " << nthread << ", grain size: " << grain_size <<
    // ", sample size: " << nsamples <<'\n';

  // worker.schedule_plan();

  parallelReduce(0, nthread, worker);
  // worker.work(0, nthread);

  // Rcout<<"Finished all\n";
  // res = res / (halfWindLen * halfWindLen * nsamples) ;

  worker.destroy_plan();

  // Rcout<<"Destroyed all\n";
  NumericVector res = NumericVector(worker.y.begin(), worker.y.end());

  res.attr("frequency") = freq;


  return res;
}



/*** R
set.seed(1)
x = rnorm(2000 *1800)

res = rave:::pwelch(x, 2000, window = 64, noverlap = 8, nfft = 256, plot = F)

res2 = pwelch(x, 1000, 64L, 8L, 256L, 8L)
range(res2 - res$spec)

microbenchmark::microbenchmark({
  res = rave:::pwelch(x, 2000, window = 64, noverlap = 8, nfft = 256, plot = F)
},{
  pwelch(x, 2000, 64L, 8L, 256L, 8L)
}, {
  pwelch(x, 2000, 64L, 8L, 256L, 1L)
}, times = 1)

# rave::rave_prepare('demo/YAB', 14, 'YABaOutlier', c(1,2), data_types = NULL)
# volt = module_tools$get_voltage2()
# res = rave:::pwelch(volt$`008`[[1]], 2000, window = 16, noverlap = 8, nfft = 512, plot = F)
# plot(res$spec, type = 'l')
#
# res2 = pwelch(volt$`008`[[1]], 2000, 16L, 8L, 512L, 8L)
# plot(res2, type = 'l')
#
# plot((res$spec - res2) / res$spec, type='l')

*/