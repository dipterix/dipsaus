#ifndef DIPSAUS_SIGNALS_H
#define DIPSAUS_SIGNALS_H

#include <stdlib.h>
#include <cmath>
#include <Rcpp.h>


typedef R_xlen_t size_int;

// whether window length is small
bool guard_length(const size_int n);


template <typename T>
std::vector<T> truncate(std::vector<T>& input, bool trunc);

template <typename VT>
VT seq2(double from, double to, double by = 1.0, size_int len = 0);

/**
 * @param n number of time points
 * @param a weighting coefficients
 * @param sym generate symmetric window?
 */
Rcpp::NumericVector cosine_gen(size_int n, Rcpp::NumericVector a, bool sym = true);

Rcpp::NumericVector hamming_gen(const size_int n, double alpha, bool sym = true);
Rcpp::NumericVector hann(const size_int n, bool sym = true);

Rcpp::NumericVector detrend_naive(const Rcpp::NumericVector& y, Rcpp::NumericVector input);
Rcpp::NumericVector hanning(const size_int n);


// void cfft_r2c(int* n, double* data, double complex* res, int* retHermConj)

// void detrend_linear(Rcpp::NumericVector res, Rcpp::NumericVector input, Rcpp::IntegerVector breaks);



#endif


