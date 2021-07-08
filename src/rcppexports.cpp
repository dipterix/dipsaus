// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cpp_forelse
SEXP cpp_forelse(Rcpp::List data, Rcpp::Function fun, Rcpp::Function alt);
RcppExport SEXP _dipsaus_cpp_forelse(SEXP dataSEXP, SEXP funSEXP, SEXP altSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::Function >::type fun(funSEXP);
    Rcpp::traits::input_parameter< Rcpp::Function >::type alt(altSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_forelse(data, fun, alt));
    return rcpp_result_gen;
END_RCPP
}
// arrayShift
Rcpp::NumericVector arrayShift(const Rcpp::NumericVector x, const int64_t tidx, const int64_t sidx, const Rcpp::IntegerVector& shift, const Rcpp::IntegerVector& dims);
RcppExport SEXP _dipsaus_arrayShift(SEXP xSEXP, SEXP tidxSEXP, SEXP sidxSEXP, SEXP shiftSEXP, SEXP dimsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< const int64_t >::type tidx(tidxSEXP);
    Rcpp::traits::input_parameter< const int64_t >::type sidx(sidxSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type shift(shiftSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type dims(dimsSEXP);
    rcpp_result_gen = Rcpp::wrap(arrayShift(x, tidx, sidx, shift, dims));
    return rcpp_result_gen;
END_RCPP
}
// baselineArray
Rcpp::NumericVector baselineArray(const Rcpp::NumericVector& x, const Rcpp::NumericVector& bl, const Rcpp::IntegerVector dims, const Rcpp::IntegerVector bldims, const int tidx, const Rcpp::IntegerVector& per, const Rcpp::IntegerVector& rest, const int method);
RcppExport SEXP _dipsaus_baselineArray(SEXP xSEXP, SEXP blSEXP, SEXP dimsSEXP, SEXP bldimsSEXP, SEXP tidxSEXP, SEXP perSEXP, SEXP restSEXP, SEXP methodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type bl(blSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type dims(dimsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type bldims(bldimsSEXP);
    Rcpp::traits::input_parameter< const int >::type tidx(tidxSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type per(perSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type rest(restSEXP);
    Rcpp::traits::input_parameter< const int >::type method(methodSEXP);
    rcpp_result_gen = Rcpp::wrap(baselineArray(x, bl, dims, bldims, tidx, per, rest, method));
    return rcpp_result_gen;
END_RCPP
}
// collapser
Rcpp::NumericVector collapser(Rcpp::NumericVector x, Rcpp::IntegerVector dims, Rcpp::IntegerVector keep);
RcppExport SEXP _dipsaus_collapser(SEXP xSEXP, SEXP dimsSEXP, SEXP keepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type dims(dimsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type keep(keepSEXP);
    rcpp_result_gen = Rcpp::wrap(collapser(x, dims, keep));
    return rcpp_result_gen;
END_RCPP
}
// fastcov
SEXP fastcov(Rcpp::NumericVector& x1, Rcpp::NumericVector& x2, const R_xlen_t nrow, Rcpp::IntegerVector& col1, Rcpp::IntegerVector& col2, Rcpp::NumericVector& cm1, Rcpp::NumericVector& cm2, const double df);
RcppExport SEXP _dipsaus_fastcov(SEXP x1SEXP, SEXP x2SEXP, SEXP nrowSEXP, SEXP col1SEXP, SEXP col2SEXP, SEXP cm1SEXP, SEXP cm2SEXP, SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type x2(x2SEXP);
    Rcpp::traits::input_parameter< const R_xlen_t >::type nrow(nrowSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type col1(col1SEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type col2(col2SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type cm1(cm1SEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type cm2(cm2SEXP);
    Rcpp::traits::input_parameter< const double >::type df(dfSEXP);
    rcpp_result_gen = Rcpp::wrap(fastcov(x1, x2, nrow, col1, col2, cm1, cm2, df));
    return rcpp_result_gen;
END_RCPP
}
// object_address
std::string object_address(SEXP x);
RcppExport SEXP _dipsaus_object_address(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(object_address(x));
    return rcpp_result_gen;
END_RCPP
}
// sumsquared
SEXP sumsquared(SEXP& x);
RcppExport SEXP _dipsaus_sumsquared(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sumsquared(x));
    return rcpp_result_gen;
END_RCPP
}
// get_sexp_type
SEXPTYPE get_sexp_type(const SEXP& x);
RcppExport SEXP _dipsaus_get_sexp_type(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const SEXP& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(get_sexp_type(x));
    return rcpp_result_gen;
END_RCPP
}
// set_dim
SEXP set_dim(SEXP& x, SEXP& dim);
RcppExport SEXP _dipsaus_set_dim(SEXP xSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP& >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(set_dim(x, dim));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dipsaus_cpp_forelse", (DL_FUNC) &_dipsaus_cpp_forelse, 3},
    {"_dipsaus_arrayShift", (DL_FUNC) &_dipsaus_arrayShift, 5},
    {"_dipsaus_baselineArray", (DL_FUNC) &_dipsaus_baselineArray, 8},
    {"_dipsaus_collapser", (DL_FUNC) &_dipsaus_collapser, 3},
    {"_dipsaus_fastcov", (DL_FUNC) &_dipsaus_fastcov, 8},
    {"_dipsaus_object_address", (DL_FUNC) &_dipsaus_object_address, 1},
    {"_dipsaus_sumsquared", (DL_FUNC) &_dipsaus_sumsquared, 1},
    {"_dipsaus_get_sexp_type", (DL_FUNC) &_dipsaus_get_sexp_type, 1},
    {"_dipsaus_set_dim", (DL_FUNC) &_dipsaus_set_dim, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_dipsaus(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
