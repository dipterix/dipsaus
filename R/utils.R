safe_urlencode <- function(x){
  re <- base64url::base64_urlencode(as.character(x))
  paste0('==', re)
}
safe_urldecode <- function(x){
  x <- stringr::str_replace(as.character(x), pattern = '^==', '')
  base64url::base64_urldecode(x)
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
    nms <- nms[!stringr::str_detect(nms, '^\\.__rave')]
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

