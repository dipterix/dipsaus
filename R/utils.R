safe_urlencode <- function(x){
  re <- base64_urlencode(as.character(x))
  paste0('==', re)
}
safe_urldecode <- function(x){
  x <- str_replace(as.character(x), pattern = '^==', '')
  base64_urldecode(x)
}


stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    cat2(msg, level = 'FATAL')
  }
}

# Unclassified utility functions

#' Calculate time difference and return a number
#' @param t1 time start
#' @param t2 time end
#' @param units character, choices are \code{'secs'}, \code{'mins'}, \code{'hours'}, and \code{'days'}
#' @return numeric difference of time in units specified
#' @examples
#' a = Sys.time()
#' Sys.sleep(0.3)
#' b = Sys.time()
#'
#' time_delta(a, b) # In seconds, around 0.3
#' time_delta(a, b, 'mins') # in minutes, around 0.005
#'
#' @export
time_delta <- function(t1, t2, units = 'secs'){
  as.numeric(t2 - t1, units=units)
}

is_invalid <- function(x, any = FALSE, .invalids = list('is.null', 'is.na'), ...){
  .invalids <- c(.invalids, list(...))
  if('is.null' %in% .invalids){
    if(is.null(x) || !length(x)){
      return(TRUE)
    }
  }
  for( func in .invalids ){
    res <- do.call(func, args = list(x))
    if(length(res) > 1){
      if(any){
        res <- any(res)
      }else{
        res <- all(res)
      }
    }
    if(res){
      return(TRUE)
    }
  }

  return(FALSE)
}


#' Drop \code{NULL} values from list or vectors
#'
#' @param x list to check
#' @param .invalids a list of functions, or function name. Default is 'is.null'.
#'
#' @return list or vector containing no invalid values
#' @examples
#'
#' x <- list(NULL,NULL,1,2)
#' drop_nulls(x)  # length of 2
#'
#' @export
drop_nulls <- function (x, .invalids = list('is.null')) {
  x[!vapply(x, is_invalid, FUN.VALUE = logical(1), .invalids = .invalids)]
}


#' Function to clear all elements within environment
#'
#' @param env environment to clean, can be an R environment, or a
#' \code{\link{fastmap2}} instance
#' @param ... ignored
#'
#' @examples
#'
#' env = new.env()
#' env$a = 1
#' print(as.list(env))
#'
#' clear_env(env)
#' print(as.list(env))
#'
#' @export
clear_env <- function(env, ...){
  if(is.environment(env)){
    if(environmentIsLocked(env)){
      warning('Environment is locked, cannot clear environment')
      return(invisible(env))
    }
    nms <- names(env)
    nms <- nms[!str_detect(nms, '^\\.__rave')]
    if(isNamespace(env)){
      nms <- nms[!nms %in% c(".__NAMESPACE__.", ".__S3MethodsTable__.")]
    }
    rm(list = nms, envir = env)
  } else if(inherits(env, 'fastmap2')){
    .subset2(env, 'remove')(names(env))
  } else if(inherits(env, 'fastmap')){
    .subset2(env, 'remove')(names(env))
  } else {
    stop('env can only be environment or fastmap2 instances')
  }
  return(invisible(env))
}

#' A dummy function that literally does nothing
#' @param ... ignored
#' @return Nothing
#' @export
do_nothing <- function(...){

}





SEXP_TYPES <- list(
  '0' =  c('NILSXP',     '0 NILSXP      NULL'),
  '1' =  c('SYMSXP',     '1 SYMSXP      symbols'),
  '2' =  c('LISTSXP',    '2 LISTSXP     pairlists'),
  '3' =  c('CLOSXP',     '3 CLOSXP      closures'),
  '4' =  c('ENVSXP',     '4 ENVSXP      environments'),
  '5' =  c('PROMSXP',    '5 PROMSXP     promises'),
  '6' =  c('LANGSXP',    '6 LANGSXP     language objects'),
  '7' =  c('SPECIALSXP', '7 SPECIALSXP  special functions'),
  '8' =  c('BUILTINSXP', '8 BUILTINSXP  builtin functions'),
  '9' =  c('CHARSXP',    '9 CHARSXP     internal character strings'),
  '10' = c('LGLSXP',     '10 LGLSXP     logical vectors'),
  '13' = c('INTSXP',     '13 INTSXP     integer vectors'),
  '14' = c('REALSXP',    '14 REALSXP    numeric vectors'),
  '15' = c('CPLXSXP',    '15 CPLXSXP    complex vectors'),
  '16' = c('STRSXP',     '16 STRSXP     character vectors'),
  '17' = c('DOTSXP',     '17 DOTSXP     dot-dot-dot object'),
  '18' = c('ANYSXP',     '18 ANYSXP     make "any" args work'),
  '19' = c('VECSXP',     '19 VECSXP     list (generic vector)'),
  '20' = c('EXPRSXP',    '20 EXPRSXP    expression vector'),
  '21' = c('BCODESXP',   '21 BCODESXP   byte code'),
  '22' = c('EXTPTRSXP',  '22 EXTPTRSXP  external pointer'),
  '23' = c('WEAKREFSXP', '23 WEAKREFSXP weak reference'),
  '24' = c('RAWSXP',     '24 RAWSXP     raw vector'),
  '25' = c('S4SXP',      '25 S4SXP      S4 classes not of simple type')
)

#' @title Get Internal Storage Type
#' @description Get internal (\code{C}) data types; See
#' \url{https://cran.r-project.org/doc/manuals/r-release/R-ints.pdf} Page 1
#' for more different \code{SEXPTYPE}s.
#' @param x any \pkg{R} object
#' @param ... ignored
#' @return An integer of class \code{"sexp_type2"}
#' @seealso \code{\link{storage.mode}}
#'
#' @examples
#'
#' # 1 vs 1L
#'
#' # Integer case
#' sexp_type2(1L)
#'
#' # double
#' sexp_type2(1)
#'
#' # Built-in function
#' sexp_type2(`+`)
#'
#' # normal functions
#' sexp_type2(sexp_type2)
#'
#' # symbols (quoted names)
#' sexp_type2(quote(`+`))
#'
#' # Calls (quoted expressions)
#' sexp_type2(quote({`+`}))
#'
#'
#' @export
sexp_type2 <- function(x){
  re <- get_sexp_type(x)
  class(re) <- "sexp_type2"
  re
}

#' @rdname sexp_type2
#' @export
as.character.sexp_type2 <- function(x, ...){
  x <- unclass(x)
  x <- SEXP_TYPES[[as.character(x)]]
  if(is.null(x)){
    return("Unknown")
  }
  x[[1]]
}

#' @rdname sexp_type2
#' @export
print.sexp_type2 <- function(x, ...){
  y <- unclass(x)
  y <- SEXP_TYPES[[as.character(y)]]
  if(is.null(y)){
    return(sprintf("%d Unknown type", x))
  }
  cat(y[[2]], '\n')
  invisible(x)
}

