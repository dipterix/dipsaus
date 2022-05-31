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
#' @importFrom graphics par
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


R_user_dir <- function (package, which = c("data", "config", "cache")) {
  stopifnot(is.character(package), length(package) == 1L)
  which <- match.arg(which)
  home <- normalizePath("~")
  path <- switch(
    which, data = {
      p <- Sys.getenv("R_USER_DATA_DIR")
      if (!nzchar(p)) {
        p <- Sys.getenv("XDG_DATA_HOME")
        if( !nzchar(p) ){
          if( .Platform$OS.type == "windows" ){
            p <- file.path(Sys.getenv("APPDATA"), "R", "data")
          } else if (Sys.info()["sysname"] == "Darwin") {
            p <- file.path(home, "Library", "Application Support", "org.R-project.R")
          } else {
            p <- file.path(home, ".local", "share" )
          }
        }
      }
      p
    }, config = {
      p <- Sys.getenv("R_USER_CONFIG_DIR")
      if (!nzchar(p)) {
        p <- Sys.getenv("R_USER_CONFIG_DIR")
        if (!nzchar(p)) {
          p <- Sys.getenv("XDG_CONFIG_HOME")
          if (!nzchar(p)) {
            if( .Platform$OS.type == "windows" ){
              p <- file.path(Sys.getenv("APPDATA"), "R", "config")
            } else if (Sys.info()["sysname"] == "Darwin") {
              p <- file.path(home, "Library", "Preferences", "org.R-project.R")
            } else {
              p <- file.path(home, ".config")
            }
          }
        }
      }
      p
    }, cache = {
      p <- Sys.getenv("R_USER_CACHE_DIR")
      if (!nzchar(p)) {
        p <- Sys.getenv("XDG_CACHE_HOME")
        if (!nzchar(p)) {
          if( .Platform$OS.type == "windows" ){
            p <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
          } else if (Sys.info()["sysname"] == "Darwin") {
            p <- file.path(home, "Library", "Caches", "org.R-project.R")
          } else {
            p <- file.path(home, ".cache")
          }
        }
      }
      p
    })
  file.path(path, "R", package)
}

deparse1 <- function (expr, collapse = " ") {
  paste(deparse(expr), collapse = collapse)
}

str_sub <- function(x, start = 1L, end = -1L) {
  if(end < 0) {
    end <- nchar(x) + 1 + end
  }
  substr(x, start = start, stop = end)
}

str_detect <- function(x, pattern) {
  grepl(pattern, x)
}

str_replace <- function(x, pattern, replacement) {
  sub(pattern = pattern, replacement = replacement, x = x)
}

str_replace_all <- function(x, pattern, replacement) {
  gsub(pattern = pattern, replacement = replacement, x = x)
}



