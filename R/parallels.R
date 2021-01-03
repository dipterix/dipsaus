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
  cat2('async must be evaluated in function async_expr', level = 'FATAL')
}


#' Wrapper for \code{future.apply::future_lapply}
#' @param X,FUN,... passing to \code{future.apply::future_lapply}
#' @seealso \code{\link[future.apply]{future_lapply}}
#' @export
async_flapply <- function(X, FUN, ...){
  future.apply::future_lapply(X = X, FUN = FUN, ...)
}

#' Create forked clusters, but more than that
#' @description Creates forked clusters. If fails, then switch to alternative
#' plan (default is \code{"multisession"}).
#' @param workers positive integer, number of cores to use
#' @param on_failure alternative plan to use if failed. This is useful when
#' forked process is not supported (like 'windows'); default is
#' \code{options("dipsaus.cluster.backup")} or 'sequential'
#' @param clean whether to reverse the plan on exit. This is useful when use
#' \code{make_forked_clusters} inside of a function. See details and examples.
#' @param ... passing to \code{future::plan}
#' @details This was original designed as a wrapper for
#' \code{future::plan(future::multicore, ...)}. Forked
#' clusters are discouraged when running in 'RStudio' because some pointers
#' in 'RStudio' might be incorrectly handled, causing fork-bombs. However,
#' forked process also has big advantages over other parallel methods: there
#' is no data transfer needed, hence its speed is very fast. Many external
#' pointers can also be shared using forked process. Since version 1.14.0,
#' unfortunately, forked 'multicore' is banned by \code{future} package by
#' default, and you usually need to enable it manually. This function provides
#' a simple way of enable it and plan the future at the same time.
#'
#' On windows, forked process is not supported, under this situation, the plan
#' fall back to sequential, which might not be what you want. In such case,
#' this function provides an alternative strategy that allows you to plan.
#' You could also always enable the alternative strategy by setting
#' \code{dipsaus.no.fork} option to true.
#'
#' The parameter \code{clean} allows you to automatically clean the plan. This
#' function allows you to reverse back to previous plan automatically once your
#' function exits. For example, users might have already set up their own plans,
#' \code{clean=TRUE} allows you to set the plan back to those original plans
#' once function exit. To use this feature, please make sure this function is
#' called within another function, and you must collect results before exiting
#' the outer function.
#' @return Current future plan
#' @seealso \code{\link{lapply_async2}}
#'
#' @examples
#'
#'
#'
#' if(interactive()){
#'
#'   # ------ Basic example
#'   library(future)
#'   library(dipsaus)
#'
#'   # sequential
#'   plan("sequential")
#'
#'   make_forked_clusters()
#'   plan()  # multicore, or multisession (on windows)
#'
#'   Sys.getpid()  # current main session PID
#'   value(future({Sys.getpid()}))  # sub-process PID, evaluated as multicore
#'
#'   # ------ When fork is not supported
#'
#'   # reset to default single core strategy
#'   plan("sequential")
#'
#'   # Disable forked process
#'   options("dipsaus.no.fork" = TRUE)
#'   options("dipsaus.cluster.backup" = "multisession")
#'
#'   # Not fall back to multisession
#'   make_forked_clusters()
#'   plan()
#'
#'   # ------ Auto-clean
#'
#'   # reset plan
#'   plan("sequential")
#'   options("dipsaus.no.fork" = FALSE)
#'   options("dipsaus.cluster.backup" = "multisession")
#'
#'   # simple case:
#'   my_func <- function(){
#'     make_forked_clusters(clean = TRUE)
#'
#'     fs <- lapply(1:4, function(i){
#'       future({Sys.getpid()})
#'     })
#'
#'     unlist(value(fs))
#'   }
#'
#'   my_func()    # The PIDs are different, meaning they ran in other sessions
#'   plan()       # The plan is sequential, auto reversed strategy
#'
#'   # ------ Auto-clean with lapply_async2
#'   my_plan <- plan()
#'
#'   # lapply_async2 version of the previous task
#'   lapply_async2(1:4, function(i){
#'     Sys.getpid()
#'   })
#'
#'   identical(plan(), my_plan)
#'
#' }
#'
#'
#'
#' @export
make_forked_clusters <- function(
  workers = future::availableCores(),
  on_failure = getOption("dipsaus.cluster.backup", "sequential"),
  clean = FALSE,
  ...){

  if( clean ){
    oplan <- future::plan("list")
    # check whether current plan is multicore
    parent_frame <- parent.frame()
    expr <- rlang::quo_squash({
      rlang::quo({
        future::plan(!!oplan, substitute = FALSE, .call = NULL, .cleanup = FALSE, .init = FALSE)
      })
    })
    do.call(
      on.exit, list(expr, add = TRUE, after = TRUE),
      envir = parent_frame
    )
  }
  os <- get_os()
  if(os == 'windows' || getOption("dipsaus.debug", FALSE) || getOption("dipsaus.no.fork", FALSE)){
    suc <- tryCatch({
      future::plan(on_failure, .call = NULL, .cleanup = !clean, .init = FALSE, workers = workers, ...)
      TRUE
    },
    error = function(e){ FALSE },
    warning = function(e){ FALSE })
    if(!suc){
      future::plan(on_failure, .call = NULL, .cleanup = !clean, .init = FALSE, ...)
    }
  } else {
    options(future.fork.enable = TRUE)
    future::plan(future::multicore, workers = workers, .cleanup = !clean, .init = FALSE, .call = NULL, ...)
  }
  invisible(future::plan())
}


