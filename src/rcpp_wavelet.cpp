#include "signals.h"
using namespace Rcpp;



struct WaveletWorker : public RcppParallel::Worker
{
  const RcppParallel::RVector<double> freqs;
  const RcppParallel::RVector<double> wave_cycles;
  const double srate;
  const R_xlen_t trunc;
  std::vector<std::vector<std::complex<double>>> res;
  unsigned int nthread;
  int state;
  fftw_complex **inputs;
  fftw_complex **outputs;
  fftw_plan *plans;
  fftw_complex *data;
  bool has_data;
  R_xlen_t cutoff;

  WaveletWorker(
    const NumericVector& freqs,
    const NumericVector& wave_cycles,
    const double srate,
    const R_xlen_t trunc,
    const unsigned int nthread
  ): freqs(freqs), wave_cycles(wave_cycles), srate(srate),
  trunc(trunc), nthread(nthread){
    state = 0;
    cutoff = 0;
    has_data = false;
    res = std::vector<std::vector<std::complex<double>>>(freqs.size());
  };

  void destroy(){

    if(state == 1 || state == 3){
      // Rcout << "gc plans, setting state to 0\n";
      for(unsigned int i = 0; i < nthread; i++){
        fftw_destroy_plan(*(plans + i));
        fftw_free(*(inputs + i));
        fftw_free(*(outputs + i));

      }
      free(inputs);
      free(outputs);
      free(plans);
      plans = NULL;
      inputs = NULL;
      outputs = NULL;
      fftw_cleanup();
      state = 0;
    }


  }

  void remove_data(){
    if( has_data ){
      free(data);
      data = NULL;
      has_data = false;
    }
  }


  void set_cutoff(R_xlen_t cut){
    if(cut < 0 || cut > trunc){
      stop("wavelet reorder index is wrong (internal error)");
    }
    cutoff = cut;
  }


  bool set_state(int new_state){
    /**
     * State:
     * 0 - generate kernels
     * 1 - FFTW kernels  (alloc)
     * 2 - dot-product data with fft kernels
     * 3 - FFTW again (reverse convolution)  (alloc)
     * 4 - re-order
     */

    if(new_state == state){ return(false); }

    // state is changed
    destroy();

    // new_state not 1 nor 3, no need to malloc
    if(new_state == 0 || new_state == 2 || new_state == 4){
      // Rcout << "Setting state to " << new_state << "\n";
      state = new_state;
      return(true);
    }
    if(trunc == 0){ stop("Cannot set state with data length missing"); }

    state = new_state;
    // Rcout << "Setting state to " << new_state << "\n";
    // wavelet - FFTW_c2c
    inputs = (fftw_complex **) malloc(sizeof(fftw_complex*) * nthread);
    outputs = (fftw_complex **) malloc(sizeof(fftw_complex*) * nthread);
    plans = (fftw_plan *) malloc(sizeof(fftw_plan) * nthread);

    unsigned int sign;
    if(new_state == 1){
      sign = FFTW_FORWARD;
    }else{ // new_state = 3
      // reverse fftw
      sign = FFTW_BACKWARD;
    }

    for(unsigned int i = 0; i < nthread; i++){
      *(inputs + i) = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * trunc);
      *(outputs + i) = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * trunc);
      *(plans + i) = fftw_plan_dft_1d(trunc, *(inputs + i), *(outputs + i), sign, FFTW_ESTIMATE);
    }
    return true;
  }

  void set_data(const NumericVector& x, bool demean = true){
    if(x.size() != trunc){
      stop("data length does not matches with kernel lengths");
    }
    NumericVector xcopy = NumericVector(x);
    if(demean){
      xcopy = xcopy - mean(x);
    }
    if(!has_data){
      has_data = true;
      data = (fftw_complex *) fftw_malloc(sizeof(fftw_complex) * trunc);
    }

    // fft r2c
    fftw_plan p = fftw_plan_dft_r2c_1d(trunc, &(xcopy[0]), data, FFTW_ESTIMATE);
    fftw_execute(p);
    fftw_destroy_plan(p);
    fftw_cleanup();
    // Finish up:
    R_xlen_t i, nc;
    nc = (trunc - (trunc % 2)) / 2 + 1;
    for(i = nc; i < trunc; i++ ) {
      data[i][0] = data[trunc - i][0];
      data[i][1] = -data[trunc - i][1];
    }
  }


  void generate_kernels(std::size_t begin, std::size_t end) {
    double fq, cycles, st, A, tmp_gaus_win, wavelet_win, w_l_half;
    R_xlen_t w_l;
    R_xlen_t trunc_l = trunc;
    std::vector<std::complex<double>> tmp_wavelet;
    std::complex<double> tmp;

    R_xlen_t ii, jj, ks, ke;
    for(ii=begin; ii< end; ii++ ){

      fq = freqs[ii];
      cycles = wave_cycles[ii];

      st = cycles / (2.0 * PI * fq);

      // half of window length
      w_l = (R_xlen_t)(3.0 * st * srate * 2) + 1;
      w_l_half = (double)(w_l - 1) / 2.0;
      if(trunc > 0){

        tmp_wavelet = std::vector<std::complex<double>>(trunc);
        ks = (trunc + (trunc % 2)) / 2 - (w_l - (w_l%2)) / 2;
        ke = ks + w_l;
      }else{
        tmp_wavelet = std::vector<std::complex<double>>(w_l);
        ks = 0;
        ke = w_l;
        trunc_l = w_l;
      }

      A = 1.0/sqrt(st* std::sqrt(PI));
      for( jj = 0; jj < trunc_l; jj++ ){
        if(jj < ks || jj >= ke){
          // either truncate or pad the kernels
          tmp_wavelet[jj] = std::complex<double>(0, 0);
        }else{

          // 0: window length
          wavelet_win = -3.0 * st + (1.0 / srate) * (jj - ks);

          // wavelet 1: calc sinus in complex domain
          tmp = std::exp(std::complex<double>(0, (-w_l_half+(jj - ks)) * 2.0 * PI * fq / srate));

          // wavelet 2: calc gaussian wrappers
          tmp_gaus_win = A * std::exp(
            - std::pow(wavelet_win, 2.0) /(
                std::pow(cycles / (2.0 * PI *fq), 2.0) * 2.0
            )
          );

          // Generate kernel
          tmp_wavelet[jj] = tmp * tmp_gaus_win;
        }


      }


      res[ii] = tmp_wavelet;
    }
  }

  void fftw_c2c_kernels(std::size_t startThread, std::size_t endThread,
                        bool conjugate, double normalize = 1.0){

    R_xlen_t ii, jj;
    double sign = 1 - (conjugate * 2);
    std::complex<double>* ptr;
    for(R_xlen_t cth = startThread; cth < endThread; cth++ ){

      for(jj = cth; jj < freqs.size() ; jj+=nthread){


        // copy kernel to input
        for(ii = 0 ; ii < trunc; ii++){
          (*(*(inputs+cth) + ii))[0] = res[jj][ii].real();
          (*(*(inputs+cth) + ii))[1] = res[jj][ii].imag();
        }
        // run fftw
        fftw_execute(*(plans + cth));

        // Copy back to output
        ptr = &(res[jj][0]);
        for(ii = 0 ; ii < trunc; ii++, ptr++){
          *ptr = std::complex<double>(
            (*(*(outputs+cth) + ii))[0] / normalize,
            sign * (*(*(outputs+cth) + ii))[1] / normalize
          );
        }

      }



    }
    ptr = NULL;
  }

  std::complex<double> dot(std::complex<double> a, fftw_complex b){
    return std::complex<double>(
      a.real() * b[0] - a.imag() * b[1],
                                    a.imag() * b[0] + a.real() * b[1]
    );
  }

  void dot_product(std::size_t begin, std::size_t end){
    R_xlen_t jj;
    for(R_xlen_t ii = begin; ii < end; ii++ ){
      for(jj = 0; jj < trunc; jj++){
        // res[ii] = res[ii] * data;
        res[ii][jj] = dot(res[ii][jj], data[jj]);
      }
    }
  }

  void reorder(std::size_t begin, std::size_t end){
    std::vector<std::complex<double>> tmp = std::vector<std::complex<double>>(cutoff);
    if(cutoff == 0 || cutoff == trunc){
      return ;
    }
    for(R_xlen_t ii = 0; ii < res.size(); ii++ ){
      // step 1: copy
      std::copy(res[ii].begin(), res[ii].begin() + cutoff, tmp.begin());

      std::copy(res[ii].begin() + cutoff, res[ii].end(), res[ii].begin());

      std::copy(tmp.begin(), tmp.end(), res[ii].end() - cutoff);
    }
  }

  void operator()(std::size_t begin, std::size_t end){
    switch(state){
    case 0:
  {
    // generate kernels
    // ii from 0 to freq.size()
    generate_kernels(begin, end);
    break;
  }
    case 1:
  {
    // kernels are generated, need calculate
    // begin and end should < nthread
    // fft_wave = Conj(fftwtools::fftw_c2c(wvi))
    // ii from 0 to nthread
    fftw_c2c_kernels(begin, end, true, 1.0);
    break;
  }
    case 2:
  {
    // dot-product data with fft kernels and save to res
    // no checks, but has_data must be true, otherwise segfault
    // ii from 0 to freq.size()
    dot_product(begin, end);
    break;
  }

    case 3:
  {
    // dreverse fftw
    // ii from 0 to nthread
    double norm = double(trunc) * std::sqrt(srate / 2.0);
    fftw_c2c_kernels(begin, end, false, norm);
    break;
  }

    case 4:
  {
    // reorder
    reorder(begin, end);
    break;
  }

    }

  }

  List as_list(){
    List re = List(freqs.size());
    ComplexVector tmp;
    for(R_xlen_t ii = 0; ii<freqs.size(); ii++ ){
      tmp = ComplexVector(res[ii].size());
      for(R_xlen_t jj = 0; jj < res[ii].size(); jj++){
        tmp[jj].r = res[ii][jj].real();
        tmp[jj].i = res[ii][jj].imag();
      }
      re[ii] = tmp;
    }
    return re;
  }

  ComplexMatrix as_matrix(bool transpose = false){
    ComplexMatrix re;
    if(transpose){
      re = ComplexMatrix(freqs.size(), trunc);
      for(R_xlen_t ii = 0; ii<freqs.size(); ii++ ){
        for(R_xlen_t jj = 0; jj < trunc; jj++){
          re(ii, jj).r = res[ii][jj].real();
          re(ii, jj).i = res[ii][jj].imag();
        }
      }
    }else{
      re = ComplexMatrix(trunc, freqs.size());
      for(R_xlen_t ii = 0; ii<freqs.size(); ii++ ){
        for(R_xlen_t jj = 0; jj < trunc; jj++){
          re(jj, ii).r = res[ii][jj].real();
          re(jj, ii).i = res[ii][jj].imag();
        }
      }
    }
    return re;
  }
};


// [[Rcpp::export]]
List wavelet_kernels(
    NumericVector freqs, const double srate, NumericVector wave_cycles,
    const int nthread
) {
  // Don't round srate as some labs use float swrate

  R_xlen_t f_l = freqs.size();
  if(f_l != wave_cycles.size()){
    stop("wave_cycles MUST have the same length as frequencies");
  }
  // TODO: Make sure srate is positive (R)
  // TODO: Make sure there is no NA (R)

  WaveletWorker worker(freqs, wave_cycles, srate, 0, nthread);

  parallelFor(0, f_l, worker);
  // worker.generate_kernels(0, f_l);

  List res = worker.as_list();


  return res;

}


// [[Rcpp::export]]
List wavelet(
    NumericVector data, NumericVector freqs, const double srate,
    NumericVector wave_cycles, const int nthread, bool demean = true,
    bool power = false, bool phase = false,  bool ampl = false,
    bool cplx = true, bool knel = false, bool transpose = true
){
  List re = List();
  R_xlen_t f_l = freqs.size();
  if(f_l != wave_cycles.size()){
    stop("wave_cycles MUST have the same length as frequencies");
  }
  // TODO: Make sure srate is positive (R)
  // TODO: Make sure there is no NA (R)

  R_xlen_t d_l = data.size();

  WaveletWorker worker(freqs, wave_cycles, srate, d_l, nthread);

  // FFT data from r2c
  worker.set_data(data, demean);

  // generate kernels
  // Rcout << worker.state << " - generate kernels" << "\n";
  // worker.generate_kernels(0, f_l);
  parallelFor(0, f_l, worker);

  if(knel){
    re["kernel"] = worker.as_matrix(transpose);
  }

  // wavelet kernels
  worker.set_state(1);
  // Rcout << worker.state << " - FFTW kernels" << "\n";
  // worker.fftw_c2c_kernels(0, nthread, true);
  parallelFor(0, nthread, worker);

  // dot-product
  worker.set_state(2);
  // Rcout << worker.state << " - fftw(kernels) * fftw(data)" << "\n";
  // worker.dot_product(0, f_l);
  parallelFor(0, f_l, worker);

  // also remove data as they are not used anymore
  worker.remove_data();

  // reverse fftw to finish convolution
  worker.set_state(3);
  // Rcout << worker.state << " - reverse fftw" << "\n";
  // worker.fftw_c2c_kernels(0, nthread, false, double(d_l) * std::sqrt(srate / 2.0));
  parallelFor(0, nthread, worker);

  // Reorder. First set state to destroy the fftw plans
  worker.set_state(4);
  R_xlen_t cutoff = (d_l + (d_l % 2)) / 2;
  worker.set_cutoff(cutoff);
  worker.reorder(0, f_l);
  // parallelFor(0, f_l, worker);

  ComplexMatrix mat = worker.as_matrix(transpose);
  if( cplx ){
    re["result"] = mat;
  }

  if( phase ){
    re["phase"] = Rcpp::Arg(mat);
  }

  if( ampl || power ){

    NumericMatrix po = NumericMatrix(mat.nrow(), mat.ncol());
    ComplexMatrix::iterator ptr_mat = mat.begin();
    NumericMatrix::iterator ptr_pow = po.begin();

    for(; ptr_mat != mat.end(); ptr_mat++, ptr_pow++){
      (*ptr_pow) = (*ptr_mat).r * (*ptr_mat).r + (*ptr_mat).i * (*ptr_mat).i;
    }
    if(power){
      re["power"] = po;
    }


    if(ampl){
      NumericMatrix amp = NumericMatrix(mat.nrow(), mat.ncol());
      NumericMatrix::iterator ptr_amp = amp.begin();
      for(ptr_amp = amp.begin(), ptr_pow = po.begin(); ptr_amp != amp.end(); ptr_amp++, ptr_pow++){
        (*ptr_amp) = std::sqrt(*ptr_pow);
      }
      re["amplitude"] = amp;
    }


  }



  return re;
  // return worker.as_matrix();
}



/*** R
wave_rave <- rave:::wavelet_kernels;
body(wave_rave) = quote({
  srate = round(srate);
  # calculate wavelet cycles for each frequencies
  if(length(wave_num) != length(freqs)){
    # calculate wavelet cycles for each frequencies
    ratio = (log(max(wave_num)) - log(min(wave_num))) / (log(max(freqs)) - log(min(freqs)))
    wavelet_cycles = exp((log(freqs) - log(min(freqs))) * ratio + log(min(wave_num)))
  }else{
    wavelet_cycles = wave_num
  }
  f_l = length(freqs)


  # wavelet window calc - each columns of final wave is a wavelet kernel (after fft)
  # sts = wavelet_cycles / (2 * pi * freqs)
  # wavelet_wins = cbind(-3 * sts, 3 * sts)

  lapply(1:f_l, function(ii){
    fq = freqs[ii]
    cycles = wavelet_cycles[ii]
    # standard error
    st = cycles / (2 * pi * fq)

    # calculate window size
    wavelet_win = seq(-3 * st, 3 * st, by = 1/srate)

    # half of window length
    w_l_half = (length(wavelet_win) - 1) / 2

    # wavelet 1: calc sinus in complex domain
    tmp_sine = exp((0+1i) * 2 * pi * fq / srate * (-w_l_half:w_l_half))

    # Gaussian normalization part
    A = 1/sqrt(st*sqrt(pi))

    # wavelet 2: calc gaussian wrappers
    tmp_gaus_win = A * exp(-wavelet_win^2/(2 * (cycles/(2 * pi * fq))^2))

    # wave kernel
    tmp_wavelet = tmp_sine * tmp_gaus_win

    tmp_wavelet
  })
})

# re1 = wavelet_kernels(1:200, 2000, seq(3, 20, length.out = 200), 7)
# re2 = wave_rave(1:200, 2000, seq(3, 20, length.out = 200))
# length(re1[[1]])
# length(re2[[1]])
# par(mfrow=c(3,3))
# lapply(sample(200, 9), function(ii){
#   plot(re1[[ii]] - re2[[ii]])
# })

# range(Mod(unlist(re1) - unlist(re2)))
#
# microbenchmark::microbenchmark(
#   {
#     re1 = wavelet_kernels(1:200, 2000, seq(3, 20, length.out = 200), 7)
#   },{
#     re2 = wave_rave(1:200, 2000, seq(3, 20, length.out = 200))
#   }, {
#     re3 = wavelet(1:10000, 1:200, 2000, seq(3, 20, length.out = 200), 7, TRUE)
#   },
# times = 3)



# par(mfrow=c(3,2))
# lapply(100+(0:2), function(ii){
#   plot(Re(re3[[ii]]), type = 'l')
#   plot(Im(re3[[ii]]), type = 'l')
# })


# range(Re(fft_waves - re3))
# range(Im(fft_waves - re3))
#
# plot(fft_waves[,6])
# plot(re3[,6])
# plot(re1[[6]])

set.seed(1)
data = rnorm(10000)
freqs = 1:200
srate = 2000
wave_num = seq(3,20, length.out = 200)
demean = TRUE

re1 = rave:::wavelet(data, freqs, srate, wave_num, demean = TRUE)
re3 = wavelet(data, freqs, srate, wave_num, 7, TRUE,
              power = TRUE, phase = TRUE, cplx = TRUE, ampl = TRUE)

range(re3$power - re1$power)
range(re3$power - (re3$amplitude)^2)
range(re1$coef - re3$amplitude)
cplx = re1$coef * exp(re1$phase * 1i)
range(Re(re3$result - cplx))

# s = rave::load_h5('~/rave_data/data_dir/demo/YAB/rave/preprocess/voltage/electrode_14.h5', '/raw/008')[]
data = rnorm(646130)

tm = lapply(1:8, function(ii){
  gc()
  s = Sys.time()
  wavelet(data = data, freqs = freqs, srate = srate, wave_cycles = wave_num,
          nthread = ii, demean = TRUE, knel = FALSE,
          power = FALSE, phase = FALSE, cplx = FALSE, ampl = FALSE)
  s = Sys.time() - s
  print(s)
  s
})


microbenchmark::microbenchmark(
  {
    re1 = rave:::wavelet(data, freqs, srate, wave_num, demean = TRUE)
  },
  {
    re3 = wavelet(data, freqs, srate, wave_num, 8L, TRUE,
                  power = TRUE, phase = TRUE, cplx = TRUE, ampl = TRUE)
  },
  times = 1L
)

# plot(re3$result, type='l')

# wavelet_cycles = wave_num
#
# f_l = length(freqs)
# d_l = length(data)
#
# # normalize data, and fft
# if(demean){
#   fft_data = fftwtools::fftw_r2c(data - mean(data))
# }else{
#   fft_data = fftwtools::fftw_r2c(data)
# }
# message("fftw_data")
# range(Re(re3$fftw_data - fft_data) / Re(fft_data))
# range(Im(re3$fftw_data - fft_data) / Im(fft_data), na.rm = T)
#
#
# # wavelet window calc - each columns of final wave is a wavelet kernel (after fft)
# # sts = wavelet_cycles / (2 * pi * freqs)
# # wavelet_wins = cbind(-3 * sts, 3 * sts)
#
# sapply(1:f_l, function(ii){
#   fq = freqs[ii]
#   cycles = wavelet_cycles[ii]
#   # standard error
#   st = cycles / (2 * pi * fq)
#
#   # calculate window size
#   wavelet_win = seq(-3 * st, 3 * st, by = 1/srate)
#
#   # half of window length
#   w_l_half = (length(wavelet_win) - 1) / 2
#
#   # wavelet 1: calc sinus in complex domain
#   tmp_sine = exp((0+1i) * 2 * pi * fq / srate * (-w_l_half:w_l_half))
#
#   # Gaussian normalization part
#   A = 1/sqrt(st*sqrt(pi))
#
#   # wavelet 2: calc gaussian wrappers
#   tmp_gaus_win = A * exp(-wavelet_win^2/(2 * (cycles/(2 * pi * fq))^2))
#
#   # wave kernel
#   tmp_wavelet = tmp_sine * tmp_gaus_win
#
#   # padding
#   w_l = length(tmp_wavelet)
#   n_pre  = ceiling(d_l / 2) - floor(w_l/2)
#   n_post = d_l - n_pre - w_l
#   wvi = c(rep(0, n_pre), tmp_wavelet, rep(0, n_post))
#   fft_wave = Conj(fftwtools::fftw_c2c(wvi))
#
#   fft_wave
# }) ->
#   fft_waves
# wave_len = nrow(fft_waves)
#
# message("kernel")
# range(Re(re3$fftw_kernel - fft_waves))
#
# dots = fft_waves * fft_data
# message("dots")
# range(Re(re3$dots - dots))
#
#
# message("fftw_reverse")
# wave_rev = fftwtools::mvfftw_c2c(dots, inverse = 1) / wave_len / sqrt(srate / 2)
# range(Re(re3$fftw_rev - wave_rev)/Re(wave_rev), na.rm = TRUE)
# range(Im(re3$fftw_rev - wave_rev)/Re(wave_rev), na.rm = TRUE)
#
# message("wave_spectrum")
# ind = (1:(ceiling(wave_len / 2)))
# # Normalizing and re-order
# wave_spectrum = rbind(wave_rev[-ind, ], wave_rev[ind, ])
#
# range(Re(re3$wave_spectrum - wave_spectrum))
# range(Im(re3$wave_spectrum - wave_spectrum))



*/
