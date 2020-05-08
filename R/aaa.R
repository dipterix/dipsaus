#' @import shiny
#' @importFrom Rcpp sourceCpp
#' @import RcppParallel
#' @importFrom R6 R6Class
#' @importFrom base64url base64_urldecode
#' @importFrom base64url base64_urlencode
#' @importFrom fastmap fastmap
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel stopCluster
#' @importFrom utils flush.console
#' @useDynLib dipsaus, .registration = TRUE
NULL

# Same as rlang::sym('')
.missing_arg <- alist(x = )

# Compatibility issue

str2lang_alt <- function(s){
  s <- sprintf('quote(%s)', stringr::str_trim(s))
  eval(parse(text = s))
}

str2lang <- function(s){
  get0('str2lang', envir = baseenv(), ifnotfound = str2lang_alt)(s)
}


