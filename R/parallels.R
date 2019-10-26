#' Apply R expressions in a parallel way
#' @param .X a vector or a list to apply evaluation on
#' @param .expr R expression, unquoted
#' @param .varname variable name representing element of each \code{.X}
#' @param .pre_run expressions to be evaluated before looping.
#' @param envir environment to evaluate expressions
#' @param .ncore number of CPU cores
#' @param ... passed to \code{future::future}
#'
#' @details \code{async_expr} uses \code{lapply} and \code{future::future} internally.
#' Within each loop, an item in \code{".X"} will be assigned to variable \code{"x"}
#' (defined by \code{".varname"}) and enter the evaluation. During the evaluation,
#' function \code{async} is provided. Expressions within \code{async} will be
#' evaluated in another session, otherwise will be evaluated in current session.
#' Below is the workflow:
#' \itemize{
#'   \item Run \code{.pre_run}
#'   \item For \code{i} in \code{seq_along(.X)}:
#'   \itemize{
#'     \item 1. Assign \code{x} with \code{.X[[i]]}, variable name \code{x} is
#'     defined by \code{.varname}
#'     \item 2. Evaluate \code{expr} in current session.
#'     \itemize{
#'       \item a. If \code{async} is not called, return evaluated \code{expr}
#'       \item b. If \code{async(aync_expr)} is called, evaluate \code{aync_expr}
#'       in another session, and return the evaluation results if \code{aync_expr}
#'     }
#'   }
#' }
#'
#' @return a list whose length equals to \code{.X}. The value of each item
#' returned depends on whether \code{async} is called. See details for workflow.
#'
#' @examples
#'
#' \donttest{
#'
#' library(dipsaus)
#'
#' # Enable parallel computing
#' make_forked_clusters()
#'
#' # Case 1: async is called
#' async_expr(.X = 1:10, {
#'   # This part is not async
#'   print(sprintf('x=%s in session pid: %s', x, Sys.getpid()))
#'
#'   # The magic starts here, async is provided
#'   async({
#'     Sys.getpid()
#'   })
#'
#'   x  # this is not returned as async is called
#' })
#'
#' # Case 2: async is not called, all expressions are evaluated in
#' # a single session
#' async_expr(.X = 1:10, {
#'   # This part is not async
#'   print(sprintf('x=%s in session pid: %s', x, Sys.getpid()))
#'   x  # x is returned as async is not called
#' })
#'
#' }
#' @export
async_expr <- function(.X, .expr, .varname = 'x', envir = parent.frame(),
                       .pre_run = NULL,
                       .ncore = future::availableCores(), ...){
  .envir <- new.env(parent = envir)
  .expr <- substitute(.expr)
  .pre_run <- substitute(.pre_run)
  .envir$._args <- list(...)
  .envir$._futures <- list()
  ._length <- length(.X)
  ._values <- list()
  ._ii <- 1
  .envir$async <- function(expr){
    expr <- substitute(expr)
    call <- as.call(c(list(
      quote(future::future),
      expr = expr,
      substitute = TRUE,
      envir = .envir
    ), .envir$._args))
    .envir$._futures[[length(.envir$._futures) + 1]] <- eval(call)
  }

  .__check__ <- function( force_all = FALSE ){
    if( force_all ){
      future::resolve(.envir$._futures)
      re <- future::values(.envir$._futures)
      if( !is.null(re) ){
        ._values[._ii - 1 + seq_along(.envir$._futures)] <<- re
      }
      .envir$._futures <- NULL
    }else{
      if(length(.envir$._futures) >= .ncore){
        re <- future::values(.envir$._futures[[1]])
        if( !is.null(re) ){
          ._values[[._ii]] <<- re
        }
        .envir$._futures[[1]] <- NULL
        ._ii <<- ._ii + 1
      }
    }
  }

  eval(.pre_run, envir = .envir)

  lapply(seq_along(.X), function(.jj){
    .__check__()
    if(any( future::resolved(.envir$._futures)) ){
      .envir$._values
    }
    assign(.varname, .X[[.jj]], envir = .envir)
    .re <- eval(.expr, envir = .envir)
    if( ._ii <= .jj && !inherits(.re, 'Future') ){
      ._values[[.jj]] <<- .re
    }

  })
  .__check__(TRUE)
  .envir$._futures <- NULL
  if( length(._values) != ._length ){
    length(._values) <- ._length
  }
  ._values
}

#' Evaluate expression in \code{async_expr}
#' @param expr R expression
#' @seealso \code{\link[dipsaus]{async_expr}}
#' @export
async <- function(expr){
  stop('async must be evaluated in function async_expr')
}


#' Wrapper for \code{future.apply::future_lapply}
#' @param X,FUN,... passing to \code{future.apply::future_lapply}
#' @seealso \code{\link[future.apply]{future_lapply}}
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
