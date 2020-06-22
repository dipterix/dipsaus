#include <cstdlib>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
#include "utils.h"

// [[Rcpp::export]]
std::string object_address(SEXP x) {
  std::ostringstream addr;
  addr << x;
  return addr.str();
}

void get_index(std::vector<int64_t>::iterator ptr, int64_t ii, const RcppParallel::RVector<int>& dims){
  int64_t rem = 0;
  int64_t leap = 1;
  std::size_t jj;

  if(ii == NA_INTEGER){
    for( jj = 0; jj < dims.length(); jj++ ){
      *ptr = NA_INTEGER;
      ptr++;
    }
    return;
  }

  for( jj = 0; jj < dims.length(); jj++ ){
    *ptr = ((ii - rem) / leap) % dims[jj];
    rem = *ptr * leap + rem;
    leap = leap * dims[jj];
    ptr++;
  }
}

void get_index(Rcpp::IntegerVector::iterator ptr, int64_t ii,
               const Rcpp::IntegerVector& dims){
  int64_t rem = 0;
  int64_t leap = 1;
  R_xlen_t jj;

  if(ii == NA_INTEGER){
    for( jj = 0; jj < dims.length(); jj++ ){
      *ptr = NA_INTEGER;
      ptr++;
    }
    return;
  }

  for( jj = 0; jj < dims.length(); jj++ ){
    *ptr = ((ii - rem) / leap) % dims[jj];
    rem = *ptr * leap + rem;
    leap = leap * dims[jj];
    ptr++;
  }
}

void get_index(std::vector<int64_t>::iterator ptr, int64_t ii,
               const Rcpp::IntegerVector& dims){
  int64_t rem = 0;
  int64_t leap = 1;
  R_xlen_t jj;

  if(ii == NA_INTEGER){
    for( jj = 0; jj < dims.length(); jj++ ){
      *ptr = NA_INTEGER;
      ptr++;
    }
    return;
  }

  for( jj = 0; jj < dims.length(); jj++ ){
    *ptr = ((ii - rem) / leap) % dims[jj];
    rem = *ptr * leap + rem;
    leap = leap * dims[jj];
    ptr++;
  }
}




int64_t get_ii(Rcpp::IntegerVector idx, Rcpp::IntegerVector dim){

  int64_t ii = 0;
  int64_t leap = 1;

  for(R_xlen_t j = 0; j < idx.size(); j++ ){
    if(idx[j] == NA_INTEGER){
      return NA_INTEGER;
    }
    ii += (idx[j]) * leap;
    leap *= dim[j];
  }
  return ii;
}

int64_t get_ii(std::vector<int64_t> idx, Rcpp::IntegerVector dim){

  int64_t ii = 0;
  int64_t leap = 1;

  for(std::size_t j = 0; j < idx.size(); j++ ){
    if(idx[j] == NA_INTEGER){
      return NA_INTEGER;
    }
    ii += (idx[j]) * leap;
    leap *= dim[j];
  }
  return ii;
}
int64_t get_ii(RcppParallel::RVector<int64_t> idx, Rcpp::IntegerVector dim){

  int64_t ii = 0;
  int64_t leap = 1;

  for(std::size_t j = 0; j < idx.size(); j++ ){
    if(idx[j] == NA_INTEGER){
      return NA_INTEGER;
    }
    ii += (idx[j]) * leap;
    leap *= dim[j];
  }
  return ii;
}

int64_t get_ii(std::vector<int64_t> idx, RcppParallel::RVector<int> dim){
  int64_t ii = 0;
  int64_t leap = 1;

  for(std::size_t j = 0; j < idx.size(); j++ ){
    if(idx[j] == NA_INTEGER){
      return NA_INTEGER;
    }
    ii += (idx[j]) * leap;
    leap *= dim[j];
  }
  return ii;
}




int64_t length_from_dim(Rcpp::IntegerVector dim){

  if(!dim.length()){
    return 0;
  }

  int64_t len = Rcpp::algorithm::prod_nona(dim.begin(), dim.end());

  // for(auto& el : dim){
  //   len *= el;
  // }
  return len;
}

void next_array_index(
    Rcpp::IntegerVector::iterator begin,
    Rcpp::IntegerVector::iterator end,
    Rcpp::IntegerVector::iterator dim){

  *begin += 1;
  for(; begin != end; begin++ ){
    if( *begin >= *dim ){
      *begin = 0;
      *(begin+1) += 1;
    }else{
      break;
    }
    dim++;
  }
}





double add_sqrt(const double e1, const double e2){
  return e1 + std::sqrt(e2);
}

double add_log10(const double e1, const double e2){
  return e1 + std::log10(e2);
}

double add_square(const double e1, const double e2){
  return e1 + e2 * e2;
}
