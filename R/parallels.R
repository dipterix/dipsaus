#' Apply R expressions in a parallel way
#' @param .X a vector or a list to apply evaluation on
#' @param .expr R expression, unquoted
#' @param .varname variable name representing element of each \code{.X}
#' @param .pre_run expressions to be evaluated before looping.
#' @param envir environment to evaluate expressions
#' @param .ncore number of CPU cores
#' @param ... passed to \code{future::future}
#' @examples
#'
#' \donttest{
#' \dontrun{
#' # Takes about 15 seconds to run
#' library(dipsaus)
#' async_expr(.X = 1:10, {
#' # each element of .X is assigned as 'x'
#'
#' # This part is not async
#' print(x)
#'
#' # The magic starts here, async is provided
#' async({
#'   Sys.sleep(3)
#'   return(x*2)
#' })
#' }, .pre_run = {
#'   make_forked_clusters()
#' })
#' }
#' }
#'
#' @export
async_expr <- function(.X, .expr, .varname = 'x', envir = parent.frame(),
                       .pre_run = NULL,
                       .ncore = future::availableCores(), ...){
  .envir = new.env(parent = envir)
  .expr = substitute(.expr)
  .pre_run = substitute(.pre_run)
  .envir$._args = list(...)
  .envir$._futures = list()
  ._length = length(.X)
  ._values = list()
  ._ii = 1
  .envir$async = function(expr){
    expr = substitute(expr)
    call = as.call(c(list(
      quote(future::future),
      expr = expr,
      substitute = TRUE,
      envir = .envir
    ), .envir$._args))
    .envir$._futures[[length(.envir$._futures) + 1]] = eval(call)
  }

  .__check__ = function( force_all = FALSE ){
    if( force_all ){
      future::resolve(.envir$._futures)
      ._values[length(._values) + seq_along(.envir$._futures)] <<- future::values(.envir$._futures)
      .envir$._futures = NULL
    }else{
      if(length(.envir$._futures) >= .ncore){
        ._values[[._ii]] <<- future::values(.envir$._futures[[1]])
        .envir$._futures[[1]] = NULL
        ._ii <<- ._ii + 1
      }
    }
  }

  eval(.pre_run, envir = .envir)

  lapply(.X, function(x){
    .__check__()
    if(any( future::resolved(.envir$._futures)) ){
      .envir$._values
    }
    assign(.varname, x, envir = .envir)
    eval(.expr, envir = .envir)
  })
  .__check__(TRUE)
  .envir$._futures = NULL
  if( length(._values) != ._length ){
    length(._values) = ._length
  }
  ._values
}


#' Apply to each element in parallel with callbacks
#' @param x vectors or lists to apply
#' @param fun function to apply on \code{x}
#' @param ... further arguments passing to \code{fun}
#' @param .ncores number of cores to use
#' @param .call_back \code{NULL} or function that will execute at the beginning
#' of each loop
#' @param .packages packages to use
#' @param .envir environment to evaluate
#' @param .globals,.gc see \code{future::future}
#' @param .as_datatable return a data frame instead of list? Default is \code{FALSE}
#' @param .nrows if \code{.as_datatable} is true, what's the number of rows of the result
#' @examples
#'
#' \donttest{
#' \dontrun{
#' # Takes about 15 seconds to run
#' library(dipsaus)
#'
#' # Make a forked clusters
#' make_forked_clusters()
#'
#' async_lapply(11:20, function(x){
#'   Sys.sleep(3)
#'   return(x)
#' }, .call_back = function(i){
#'   # Callback is not async
#'   print(sprintf('Iteration %d', i))
#' })
#' }
#' }
#' @export
async_lapply <- function(
  x, fun, ..., .ncores = future::availableCores(), .call_back = NULL, .packages = NULL,
  .envir = environment(), .globals = TRUE, .gc = TRUE, .as_datatable = FALSE,
  .nrows = 0
){
  if(!length(x)){
    return(list())
  }
  .ncores = as.integer(.ncores)
  if(.ncores <= 0){
    .ncores = future::availableCores()
  }
  # compatible with windows
  args = list(...)
  if(stringr::str_detect(Sys.info()['sysname'], '^[wW]in') || .ncores == 1){
    return(lapply(seq_along(x), function(ii){
      if(is.function(.call_back)){
        try({
          .call_back(ii)
        })
        do.call(fun, c( list(quote(x[ii])), args), envir = environment())
      }
    }))
  }


  if(is.null(.packages)){
    .packages = stringr::str_match(search(), 'package:(.*)')
    .packages = .packages[,2]
    .packages = rev(.packages[!is.na(.packages)])
  }
  .niter = length(x)


  .future_list = list()

  if(.as_datatable){
    .future_values = data.table::data.table(
      V1 = rep(NA, .nrows),
      keep.rownames = F, stringsAsFactors = F
    )
  }else{
    .future_values = list()
  }




  if(.niter == 0){
    return(list())
  }

  .this_env = environment()
  ..started = FALSE


  lapply(seq_along(x), function(.i){
    if(is.function(.call_back)){
      try({
        .call_back(.i)
      })
    }

    expr = rlang::quo_squash(rlang::quo({ do.call(fun, c(list(quote(!!x[[.i]])), args)) }))

    .this_env$.future_list[[length(.future_list) + 1]] = future::future(expr, envir = .envir, substitute = FALSE, lazy = FALSE, globals = .globals, .packages = .packages, gc = .gc)


    if(length(.future_list) >= .ncores){
      # wait for one of futures resolved
      if(!..started && .as_datatable){
        .this_env$..started = TRUE
        .this_env$.future_values[[1]] = future::value(.future_list[[1]])
      }else{
        .this_env$.future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
      }

      .this_env$.future_list[[1]] = NULL
    }
  })

  if(length(.future_list)){
    future::resolve(.future_list)
    while(length(.future_list)){
      .future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
      .future_list[[1]] = NULL
    }
  }

  return(.future_values)
}

#' Wrapper for \code{future.apply::future_lapply}
#' @param X,FUN,... passing to \code{future.apply::future_lapply}
#' @export
async_flapply <- function(X, FUN, ...){
  future.apply::future_lapply(X = X, FUN = FUN, ...)
}

#' Create forked clusters
#' @param ... passing to \code{future::plan}
#' @details This is a wrapper for \code{future::plan(future::multicore, ...)}.
#' However, since version 1.14.0, forked clusters are disabled in `RStudio` by
#' default, and you usually need to enable it manually. This function provides
#' a simple way of enable it and plan the future at the same time.
#' @export
make_forked_clusters <- function(...){
  options(future.fork.enable = TRUE)
  future::plan(future::multicore, ...)
}
