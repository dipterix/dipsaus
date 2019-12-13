#ifndef DIPSAUS_UTILS_H
#define DIPSAUS_UTILS_H

#include <stdlib.h>
#include <Rcpp.h>


void get_index(std::vector<int>::iterator ptr, long int ii, const Rcpp::IntegerVector& dims);

#endif
