#ifndef DIPSAUS_SIGNALS_H
#define DIPSAUS_SIGNALS_H

#include <stdlib.h>
#include <fftw3.h>
#include <math.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>


// whether window length is small
bool guard_length(const R_xlen_t n);


template <typename T>
std::vector<T> truncate(std::vector<T>& input, bool trunc);

template <typename VT>
VT seq2(double from, double to, double by = 1.0, R_xlen_t len = 0);

/**
 * @param n number of time points
 * @param a weighting coefficients
 * @param sym generate symmetric window?
 */
Rcpp::NumericVector cosine_gen(R_xlen_t n, Rcpp::NumericVector a, bool sym = true);

Rcpp::NumericVector hamming_gen(const R_xlen_t n, double alpha, bool sym = true);
Rcpp::NumericVector hann(const R_xlen_t n, bool sym = true);

Rcpp::NumericVector detrend_naive(const Rcpp::NumericVector& y, Rcpp::NumericVector input);
Rcpp::NumericVector hanning(const R_xlen_t n);

Rcpp::ComplexVector fftw_r2c(const R_xlen_t N, Rcpp::NumericVector data, bool retConj, bool gc = true);
Rcpp::ComplexVector fftw_c2c(const R_xlen_t N, Rcpp::ComplexVector data, bool reverse, bool gc = true);

void safe_fftw_r2c(const R_xlen_t N, RcppParallel::RVector<double>::iterator data,
                   bool retConj, RcppParallel::RVector<double>::iterator resReal,
                   RcppParallel::RVector<double>::iterator resImag);

template <typename VT>
VT postpad(VT& x, const int n);


// void cfft_r2c(int* n, double* data, double complex* res, int* retHermConj)

// void detrend_linear(Rcpp::NumericVector res, Rcpp::NumericVector input, Rcpp::IntegerVector breaks);



#endif


