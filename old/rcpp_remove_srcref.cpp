#include <Rcpp.h>
#include "utils.h"

using namespace Rcpp;

#include <memory>
#include <stdexcept>


/*** R
devtools::load_all()
# DIPSAUS DEBUG START
{
  options(keep.source = TRUE)
  n %?<-% 1000


  b <- list(a = matrix(1:9,3))
  a <- new.env()
  a$a <- function(){a$c + b}
  a$b <- quote({print({b + 123})})
  a$c <- 2
  a$d <- pairlist(a = 1, b = quote({print(b + b + 123)}))
  a$e <- quote({})
  a$e[[2]] <- a$b
  f <- tempfile()
  writeLines("expression(print({a + b}))", con = f)
  a$f <- parse(f, keep.source = TRUE)
  rm(f)


  if(n < 1000) {
    gc(); gctorture(on = TRUE)
    gctorture2(n, wait = 0L, inhibit_release = FALSE)
  } else {
    gc(); gctorture(on = FALSE)
  }
  re <- remove_srcref(a, verbose = TRUE)
  gctorture(on = FALSE)

  # re <- digest_deep(environment(), dipsaus::digest, verbose = TRUE)
  NULL
}; print(list(
  attributes(a$a),
  attributes(a$b),
  attributes(a$c),
  attributes(a$d),
  attributes(a$d$b),
  attributes(a$e),
  attributes(a$e[[2]])
))

*/

// [[Rcpp::export]]
SEXP remove_srcref(SEXP &obj, const bool &verbose = false) {


  // only work with environment, function, list, pairlist, language
  std::map<std::string, bool> registry;

  Rcpp::Environment env_utils = Rcpp::Environment::namespace_env("utils");
  Rcpp::Function remove_source = env_utils["removeSource"];

  SEXP R_SrcRefSymbol = Rf_install("srcref");
  SEXP R_SrcFileSymbol = Rf_install("srcfile");
  SEXP R_WholeSrcRefSymbol = Rf_install("wholeSrcref");

  auto rm_src_basic = [&R_SrcRefSymbol, &R_SrcFileSymbol, &R_WholeSrcRefSymbol](SEXP &x) {

    switch( TYPEOF(x) )
    {
      // 0	NILSXP	NULL
      case NILSXP:
      // 1	SYMSXP	symbols
      case SYMSXP:
      // 2	LISTSXP	pairlists
      case LISTSXP:
      // 4	ENVSXP	environments
      case ENVSXP:
      // 5	PROMSXP	promises
      case PROMSXP:
      // 7	SPECIALSXP	special functions
      case SPECIALSXP:
      // 8	BUILTINSXP	builtin functions
      case BUILTINSXP:
      // 9	CHARSXP	internal character strings
      case CHARSXP:
      // 10	LGLSXP	logical vectors
      case LGLSXP:
      // 13	INTSXP	integer vectors
      case INTSXP:
      // 14	REALSXP	numeric vectors
      case REALSXP:
      // 15	CPLXSXP	complex vectors
      case CPLXSXP:
      // 16	STRSXP	character vectors
      case STRSXP:
      // 17	DOTSXP	dot-dot-dot object
      case DOTSXP:
      // 18	ANYSXP	make “any” args work
      case ANYSXP:
      // 19	VECSXP	list (generic vector)
      case VECSXP:
      // 21	BCODESXP	byte code
      case BCODESXP:
      // 22	EXTPTRSXP	external pointer
      case EXTPTRSXP:
      // 23	WEAKREFSXP	weak reference
      case WEAKREFSXP:
      // 24	RAWSXP	raw vector
      case RAWSXP:
      // 25	S4SXP	S4 classes not of simple type
      case S4SXP:
      {
        break;
      }
      // 3	CLOSXP	closures
      case CLOSXP:
      // 6	LANGSXP	language objects
      case LANGSXP:
      // 20	EXPRSXP	expression vector
      case EXPRSXP:
      {
        Rf_setAttrib(x, R_SrcRefSymbol, R_NilValue);
        Rf_setAttrib(x, R_SrcFileSymbol, R_NilValue);
        Rf_setAttrib(x, R_WholeSrcRefSymbol, R_NilValue);
      }
    }

  };

  auto rm_src_lang = [&rm_src_basic](SEXP &x, auto && rm_src) {
    if( is_env_from_package(x) ) { return; }
    // assuming TYPEOF(x) == LANGSXP or LISTSXP

    rm_src_basic(x);

    SEXP el = PROTECT(CAR(x));
    rm_src( el, rm_src );
    UNPROTECT(1);

    el = PROTECT(CDR(x));
    rm_src( el, rm_src );
    UNPROTECT(1);
  };

  auto rm_src_closure = [&rm_src_basic] (SEXP &x, auto && rm_src) {
    // if( is_env_from_package(x) ) { return; }
    // assuming TYPEOF(x) == CLOSXP
    rm_src_basic( x );

    SEXP body = BODY_EXPR(x);
    rm_src( body, rm_src );
  };

  auto rm_src_list = [&rm_src_basic] (SEXP &x, auto && rm_src) {

    rm_src_basic( x );

    R_xlen_t len = XLENGTH(x);
    SEXP el;
    for( R_xlen_t i = 0; i < len; i++ ) {
      el = PROTECT(VECTOR_ELT(x, i));
      rm_src(el, rm_src);
      UNPROTECT(1);
    }
  };

  auto rm_src_env = [ ] (SEXP &x, auto && rm_src) {
    Rcpp::Environment x_(x);
    SEXP env_names = PROTECT( x_.ls(true) );
    if( env_names != R_NilValue ) {

      R_xlen_t len = XLENGTH(env_names);
      SEXP el, nm, nm_sxp;
      for( R_xlen_t i = 0; i < len; i++ ) {
        nm = PROTECT( STRING_ELT(env_names, i) );
        nm_sxp = PROTECT( Rf_installChar( nm ) );
        el = PROTECT( Rf_findVar( nm_sxp , x ) );
        rm_src(el, rm_src);
        UNPROTECT(3);
      }
    }
    UNPROTECT(1);
  };


  int n_prefix = 0;

  auto rm_src = [&](SEXP &x, auto && rm_src) {

    std::string mem_addr = object_address(x);
    std::string prefix(n_prefix, '.');
    n_prefix++;

    auto verbatim = [&](const std::string &msg) {
      if( verbose ) {
        Rprintf("%s%s\n", prefix.c_str(), msg.c_str());
      }
    };

    if( registry.count( mem_addr ) > 0 ) {
      mem_addr += " (duplicated)";
      verbatim( mem_addr );
      n_prefix--;
      return;
    }
    registry[ mem_addr ] = true;

    std::string special = "";
    switch( TYPEOF(x) )
    {
      // 0	NILSXP	NULL
      case NILSXP:
        special = " [NILSXP]"; break;

        // 1	SYMSXP	symbols
      case SYMSXP:
        special = " [SYMSXP]"; break;

        // 5	PROMSXP	promises
      case PROMSXP:
        special = " [PROMSXP]"; break;

        // 7	SPECIALSXP	special functions
      case SPECIALSXP:
        special = " [SPECIALSXP]"; break;

        // 8	BUILTINSXP	builtin functions
      case BUILTINSXP:
        special = " [BUILTINSXP]"; break;

        // 9	CHARSXP	internal character strings
      case CHARSXP:
        special = " [CHARSXP]"; break;

        // 10	LGLSXP	logical vectors
      case LGLSXP:
        special = " [LGLSXP]"; break;

        // 13	INTSXP	integer vectors
      case INTSXP:
        special = " [INTSXP]"; break;

        // 14	REALSXP	numeric vectors
      case REALSXP:
        special = " [REALSXP]"; break;

        // 15	CPLXSXP	complex vectors
      case CPLXSXP:
        special = " [CPLXSXP]"; break;

        // 16	STRSXP	character vectors
      case STRSXP:
        special = " [STRSXP]"; break;

        // 17	DOTSXP	dot-dot-dot object
      case DOTSXP:
        special = " [DOTSXP]"; break;

        // 18	ANYSXP	make “any” args work
      case ANYSXP:
        special = " [ANYSXP]"; break;

        // 21	BCODESXP	byte code
      case BCODESXP:
        special = " [BCODESXP]"; break;

        // 22	EXTPTRSXP	external pointer
      case EXTPTRSXP:
        special = " [EXTPTRSXP]"; break;

        // 23	WEAKREFSXP	weak reference
      case WEAKREFSXP:
        special = " [WEAKREFSXP]"; break;

        // 24	RAWSXP	raw vector
      case RAWSXP:
        special = " [RAWSXP]"; break;

        // 25	S4SXP	S4 classes not of simple type
      case S4SXP:
        special = " [S4SXP]"; break;

      default:
        if( is_env_from_package(x) ) {
          special = " [PackageObject]";
        }
    }

    if( special.compare("") != 0 ) {
      mem_addr += special;
      mem_addr += " (ignored)";
      verbatim( mem_addr );
      n_prefix--;
      return ;
    }

    switch (TYPEOF(x))
    {
      case LISTSXP:   // 2	LISTSXP	pairlists
      case LANGSXP:   // 6	LANGSXP	language objects
      {
        mem_addr += " [LANGSXP or LISTSXP]";
        verbatim( mem_addr );
        rm_src_lang(x, rm_src);
        break;
      }
      case CLOSXP:    // 3	CLOSXP	closures
      {
        mem_addr += " [CLOSXP]";
        verbatim( mem_addr );
        rm_src_closure(x, rm_src);
        break;
      }
      case VECSXP:    // 4	ENVSXP	environments
      case EXPRSXP:   // 20	EXPRSXP	expression vector
      {
        mem_addr += " [VECSXP or EXPRSXP]";
        verbatim( mem_addr );
        rm_src_list(x, rm_src);
        break;
      }
      case ENVSXP:    // 19	VECSXP	list (generic vector)
      {
        mem_addr += " [ENVSXP]";
        verbatim( mem_addr );
        rm_src_env(x, rm_src);
        break;
      }
      default:
      {
        mem_addr += " [Unknown SEXPTYPE] (ignored)";
        verbatim( mem_addr );
      }
    }
    n_prefix--;
  };


  rm_src(obj, rm_src);

  return obj;
}

