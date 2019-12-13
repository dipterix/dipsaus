#include <cstdlib>
#include <Rcpp.h>
#include "utils.h"

void get_index(std::vector<int>::iterator ptr, long int ii,
               const Rcpp::IntegerVector& dims){
  long int rem = 0;
  long int leap = 1;
  int jj;

  for( jj = 0; jj < dims.length(); jj++ ){
    *ptr = ((ii - rem) / leap) % dims[jj];
    rem = *ptr * leap + rem;
    leap = leap * dims[jj];
    ptr++;
  }
}
