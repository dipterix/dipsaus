#ifndef DIPSAUS_UTILS_H
#define DIPSAUS_UTILS_H

#include <stdlib.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>

/*
 * Calculate vector indices in array
 * @param ptr iterator where to save the calculated array indices to
 * @param ii index in vector must be greater equal than 0 but less than vector length
 * @param dims array dimensions
 * @return void
 */
void get_index(Rcpp::IntegerVector::iterator ptr, int64_t ii, const Rcpp::IntegerVector& dims);
void get_index(std::vector<int64_t>::iterator ptr, int64_t ii, const Rcpp::IntegerVector& dims);
void get_index(std::vector<int64_t>::iterator ptr, int64_t ii, const Rcpp::IntegerVector& dims);
void get_index(std::vector<int64_t>::iterator ptr, int64_t ii, const RcppParallel::RVector<int>& dims);



/*
 * Get position in array given vector index
 * @param idx vector index in array (vector)
 * @param dim dimension of the array
 * @return index in vectorized array (scalar)
 */
int64_t get_ii(Rcpp::IntegerVector idx, Rcpp::IntegerVector dim);
int64_t get_ii(std::vector<int64_t> idx, Rcpp::IntegerVector dim);
int64_t get_ii(std::vector<int64_t> idx, RcppParallel::RVector<int> dim);
int64_t get_ii(RcppParallel::RVector<int64_t> idx, Rcpp::IntegerVector dim);



int64_t length_from_dim(Rcpp::IntegerVector dim);
void next_array_index( Rcpp::IntegerVector::iterator begin, Rcpp::IntegerVector::iterator end, Rcpp::IntegerVector::iterator dim);



/**
 * Operator for std::accumulate calculate sum of square root
 */
double add_sqrt(const double e1, const double e2);
double add_log10(const double e1, const double e2);
double add_square(const double e1, const double e2);

std::string object_address(SEXP x);

#endif
