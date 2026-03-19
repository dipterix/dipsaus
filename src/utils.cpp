#include <cstdlib>
#include <Rcpp.h>
#include "utils.h"

// ---------------------------------------------------------------------------
// Version-conditional lookup of the `...` binding.
//
// Rf_findVarInFrame returns the raw SEXP binding for a symbol, including
// R_MissingArg and unforced promises, without side-effects.  This is exactly
// what check_missing_dots needs: it must not force promises (to avoid
// evaluating user arguments) and it must see R_MissingArg (to detect a
// call-site with no dots, e.g. f() where f <- function(...) ...).
//
// Rf_findVarInFrame is still declared in the public Rinternals.h header in
// R 4.5.x and is NOT on CRAN's checked list of non-API entry points (only its
// sibling Rf_findVarInFrame3 is).  However, the R C-API compliance project
// may remove it from the public headers in a future release.
//
// R_getVarEx (public C API, R >= 4.5.0) is the intended successor, but it:
//   1. forces PROMSXP bindings (bad: we must not evaluate user arguments), and
//   2. signals a missingArgError when the binding IS R_MissingArg (the case
//      where f() is called with no dots), instead of returning R_MissingArg.
//
// We work around (2) with R_tryCatchError: if R_getVarEx signals a
// missingArgError we return R_MissingArg, exactly replicating the raw-binding
// behaviour.  (1) is not a problem in practice for the `...` symbol because
// the DOTSXP entry itself is never a PROMSXP; only the dot *elements* inside
// the DOTSXP are promises, and those are only reached later via CAR() calls
// that we deliberately do not force.
//
// The #if guard lets older R builds continue to use Rf_findVarInFrame, which
// is safe and correct there; the new path is compiled only where R_getVarEx
// and R_tryCatchError are available.
// ---------------------------------------------------------------------------

#if R_VERSION >= R_Version(4, 5, 0)

// Body passed to R_tryCatchError: looks up `...` in env's direct frame.
static SEXP dots_lookup(void *data) {
  SEXP env = *static_cast<SEXP *>(data);
  return R_getVarEx(R_DotsSymbol, env, (Rboolean)FALSE, R_UnboundValue);
}

// Handler: missingArgError means `...` IS bound but to R_MissingArg.
// Return R_MissingArg so the loop in check_missing_dots behaves identically
// to the <4.5 Rf_findVarInFrame path.
static SEXP dots_missing_handler(SEXP /*cond*/, void * /*data*/) {
  return R_MissingArg;
}

#endif  /* R_VERSION >= R_Version(4, 5, 0) */

// [[Rcpp::export]]
SEXP check_missing_dots(const SEXP env){
  if( TYPEOF(env) != ENVSXP ){
    Rcpp::stop("`check_missing_dots` is asking for an environment");
  }

#if R_VERSION >= R_Version(4, 5, 0)
  // Use the public-API path (see detailed comment above).
  SEXP env_ref = env;
  SEXP dots = R_tryCatchError(dots_lookup, &env_ref,
                              dots_missing_handler, nullptr);
#else
  // Rf_findVarInFrame: still public API on R < 4.5, returns raw binding.
  SEXP dots = Rf_findVarInFrame(env, R_DotsSymbol);
#endif

  std::vector<bool> is_missing(0);

  int count = 0;

  if( dots != R_NilValue ){
    SEXP el = R_NilValue;

    for(; (dots != R_NilValue) && (dots != R_MissingArg); dots = CDR(dots) ){
      el = CAR(dots);
      if( el == R_MissingArg ){
        is_missing.push_back(true);
      } else {
        is_missing.push_back(false);
      }
      count++;
      if(count > 1000){
        Rcpp::stop("Cannot iterate through all dots. Are you calling `missing_dots` with correct environment?");
      }
    }
  }

  return(Rcpp::wrap(is_missing));
}

// [[Rcpp::export]]
std::string object_address(SEXP x) {
  std::ostringstream addr;
  addr << x;
  return addr.str();
}

// [[Rcpp::export]]
SEXP sumsquared(SEXP &x){
  // only support vector, matrix (numerical)
  const SEXPTYPE stype = get_sexp_type(x);

  R_xlen_t xlen = Rf_xlength(x);
  SEXP res = R_NilValue;
  int protects = 0;

  switch(stype) {
  case LGLSXP: {
    R_xlen_t re = 0;
    int* ptr = LOGICAL(x);
    for(R_xlen_t ii = 0; ii < xlen; ii++, ptr++){
      if(*ptr && *ptr != NA_LOGICAL){
        re++;
      }
    }
    res = PROTECT(Rcpp::wrap(re));
    protects++;
    break;
  }
  case INTSXP: {
    R_xlen_t re = 0;
    int* ptr = INTEGER(x);
    for(R_xlen_t ii = 0; ii < xlen; ii++, ptr++){
      if(R_finite(*ptr)){
        re += (*ptr) * (*ptr);
      }
    }
    res = PROTECT(Rcpp::wrap(re));
    protects++;
    break;
  }
  case REALSXP: {
    double re = 0;
    double* ptr = REAL(x);
    for(R_xlen_t ii = 0; ii < xlen; ii++, ptr++){
      if(R_finite(*ptr)){
        re += (*ptr) * (*ptr);
      }
    }
    res = PROTECT(Rcpp::wrap(re));
    protects++;
    break;
  }
  default: {
    Rcpp::stop("Unsupported data type: numerical (integer, logical, double) vector or matrix allowed.");
  }
  }
  if(protects > 0){
    UNPROTECT(protects);
  }
  return res;
}

// [[Rcpp::export]]
SEXPTYPE get_sexp_type(const SEXP &x){
  return TYPEOF(x);
}

// [[Rcpp::export]]
SEXP set_dim(SEXP &x, SEXP &dim){
  Rf_setAttrib(x, R_DimSymbol, dim);
  Rf_setAttrib(x, R_NamesSymbol, R_NilValue);
  return R_NilValue;
}

void get_index(std::vector<int64_t>::iterator ptr, int64_t ii, const TinyParallel::RVector<int>& dims){
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
int64_t get_ii(TinyParallel::RVector<int64_t> idx, Rcpp::IntegerVector dim){

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

int64_t get_ii(std::vector<int64_t> idx, TinyParallel::RVector<int> dim){
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

// [[Rcpp::export]]
bool is_namespace(SEXP &rho) {
  if (rho == R_BaseNamespace)
    return true;
  else if (TYPEOF(rho) == ENVSXP) {
    // R_NamespaceEnvSpec(rho) is the public C API that mirrors the manual
    // .__NAMESPACE__. → spec lookup the original code performed with
    // Rf_findVarInFrame.  It returns the namespace spec (a non-empty STRSXP)
    // when rho is a namespace environment, and R_NilValue otherwise.
    // Available since R 2.x, declared in the public Rinternals.h header.
    SEXP spec = R_NamespaceEnvSpec(rho);
    return (spec != R_NilValue && spec != R_UnboundValue &&
            TYPEOF(spec) == STRSXP && LENGTH(spec) > 0);
  }
  else return false;
}

// [[Rcpp::export]]
bool is_env_from_package(SEXP &x, const bool& recursive) {

  Rcpp::Environment env;

  switch (TYPEOF(x))
  {
    case NILSXP:
    case PROMSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case EXTPTRSXP:
    // 25	S4SXP	S4 classes not of simple type
    // case S4SXP:
      return true;
    case ENVSXP:
      env = Rcpp::Environment(x);
      break;
    case CLOSXP: {
      // Use the public C API to retrieve the closure's enclosing environment
      // without going through R's evaluator (which can fail in restricted
      // execution contexts such as testthat 3.x evaluation frames).
      // R_ClosureEnv is the documented public replacement for CLOENV added in
      // R 4.5.0; fall back to CLOENV for older R.
#if R_VERSION >= R_Version(4, 5, 0)
      env = Rcpp::Environment(R_ClosureEnv(x));
#else
      env = Rcpp::Environment(CLOENV(x));
#endif
      break;
    }
    default: {
      // env = Rf_getAttrib(x, Rf_install(".Environment"));
      SEXP attr = Rf_getAttrib(x, Rf_install(".Environment"));
      if (TYPEOF(attr) != ENVSXP) return false;
      env = Rcpp::Environment(attr);
    }
  }

  if( TYPEOF(env) - ENVSXP != 0 ) { return false; }

  if( env == Rcpp::Environment::global_env() ) { return false; }
  if( env == Rcpp::Environment::empty_env() ) { return false; }
  if( env == Rcpp::Environment::base_env() ) { return true; }

  SEXP env_impl = Rcpp::wrap(env);
  if( is_namespace(env_impl) ) { return true; }

  // recursively check
  if( recursive ) {
    // Use the public C API to get the parent environment without going through
    // R's evaluator.  R_ParentEnv is the documented replacement for ENCLOS
    // added in R 4.5.0; fall back to ENCLOS for older R.
#if R_VERSION >= R_Version(4, 5, 0)
    SEXP res = R_ParentEnv(env_impl);
#else
    SEXP res = ENCLOS(env_impl);
#endif
    if (res == R_EmptyEnv) return false;
    return is_env_from_package(res, recursive);
  }

  return false;

}

/*** R
(function(...){ check_missing_dots(environment()) })()
b <- local({function(){2}})
get_sexpinfo_obj(b)
PackFlags(3, 0, 0, 0, 1)
b()
get_sexpinfo_obj(b)
# 263171
PackFlags(3, 64, 0, 0, 1)
a <- get_enclosing_env(b)
*/