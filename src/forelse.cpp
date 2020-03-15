#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP cpp_forelse(Rcpp::List data, Rcpp::Function fun, Rcpp::Function alt){
  SEXP ans;
  bool flag = true;
  for(int i = 0; i < data.size(); i++) {
    ans = fun( data[i] );
    if( ans != R_NilValue ){
      flag = false;
      break;
    }
  }
  if( flag ){
    ans = alt();
  }
  return( ans );
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
forelse(seq_along(100), function(x) {
  if (x > 90) {
    return(x)
  }
  return(NULL)
}, function(){
  'yay'
})
*/
