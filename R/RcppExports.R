# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

cpp_forelse <- function(data, fun, alt) {
    .Call(`_dipsaus_cpp_forelse`, data, fun, alt)
}

arrayShift <- function(x, tidx, sidx, shift, dims) {
    .Call(`_dipsaus_arrayShift`, x, tidx, sidx, shift, dims)
}

baselineArray <- function(x, bl, dims, bldims, tidx, per, rest, method) {
    .Call(`_dipsaus_baselineArray`, x, bl, dims, bldims, tidx, per, rest, method)
}

collapser <- function(x, dims, keep) {
    .Call(`_dipsaus_collapser`, x, dims, keep)
}

fastcov <- function(x1, x2, nrow, col1, col2, cm1, cm2, df) {
    .Call(`_dipsaus_fastcov`, x1, x2, nrow, col1, col2, cm1, cm2, df)
}

quantile2 <- function(x, q) {
    .Call(`_dipsaus_quantile2`, x, q)
}

check_missing_dots <- function(env) {
    .Call(`_dipsaus_check_missing_dots`, env)
}

object_address <- function(x) {
    .Call(`_dipsaus_object_address`, x)
}

sumsquared <- function(x) {
    .Call(`_dipsaus_sumsquared`, x)
}

get_sexp_type <- function(x) {
    .Call(`_dipsaus_get_sexp_type`, x)
}

set_dim <- function(x, dim) {
    .Call(`_dipsaus_set_dim`, x, dim)
}

is_namespace <- function(rho) {
    .Call(`_dipsaus_is_namespace`, rho)
}

is_env_from_package <- function(x, recursive) {
    .Call(`_dipsaus_is_env_from_package`, x, recursive)
}

