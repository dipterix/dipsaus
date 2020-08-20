#' Run jobs in other R sessions without waiting
#' @param X vector or list to be applied
#' @param FUN function with the first argument to be each element of \code{X}
#' @param ... further arguments to be passed to \code{FUN}
#' @param .globals global variables to be evaluated in \code{FUN}
#' @param .name job names, used if backed by \code{rstudioapi} jobs
#' @param .rs whether to use \code{rstudioapi} jobs
#' @param .wait whether to wait for the results
#' @param .chunk_size used only when \code{.wait=FALSE}, chunk size for each
#' workers at a time. Only useful for printing progress messages, but might
#' slow down the process when \code{.chunk_size} is too small
#' @param .nworkers number of workers at a time
#' @param .simplify whether to simplify the results, i.e. merge list of results
#' to vectors or arrays
#' @param .quiet whether to suppress the printing messages
#' @param .log internally used
#' @return If \code{.wait=TRUE}, returns the applied results of \code{FUN} on
#' each of \code{X}. The result types depend on \code{.simplify} (compare
#' the difference between \code{\link{lapply}} and \code{\link{sapply}}). If
#' \code{.wait=FALSE}, then returns a function that can check the result. The
#' function takes \code{timeout} argument that blocks the session at
#' most \code{timeout} seconds waiting for the results. See examples.
#'
#' @details Unlike \code{future} package, where the global variables can be
#' automatically detected, \code{async_works} require users to specify global
#' variables explicitly via \code{.globals}
#'
#' \code{async_works} is almost surely slower than \code{future.apply} packages.
#' However, it provides a functionality that \code{future.apply} can hardly
#' achieve: being non-block. When setting \code{.wait=FALSE}, the process will
#' run in the background, and one may run as many of these tasks as they want.
#' This is especially useful when large data generating process occurs (
#' such as read in from a file, process, generate summarizing reports).
#'
#' @examples
#' \dontrun{
#' # requires a sub-process to run the code
#'
#' # Basic usage
#' a <- 1
#' async_works(1:10, function(ii){
#'   ii + a # sub-process don't know a, hence must pass a as globals
#' }, .globals = list(a = a))
#'
#' # non-blocking case
#' system.time({
#'   check <- async_works(1:10, function(ii){
#'     # simulating process, run run run
#'     Sys.sleep(ii)
#'     Sys.getpid()
#'   }, .wait = FALSE)
#' })
#'
#' # check the results
#' res <- check(timeout = 0.1)
#' attr(res, 'resolved') # whether it's resolved
#'
#' # block the session waiting for the results
#' res <- check(timeout = Inf)
#' attr(res, 'resolved')
#'
#'
#' }
#'
#' @export
async_works <- function(X, FUN, ..., .globals = NULL, .name = 'Untitled',
         .rs = FALSE, .wait = TRUE, .chunk_size = Inf,
         .nworkers = future::availableCores(), .simplify = FALSE,
         .quiet = FALSE, .log){
  stopifnot2(!length(.globals) || (
    is.list(.globals) && length(names(.globals)) == length(.globals) &&
      (!'' %in% names(.globals))
  ) ,
  msg = '.globals must be a named list'
  )

  if(.chunk_size < Inf && !.wait){
    stop('async_work .want=FALSE, then .chunk_size must be `Inf`')
  }

  n <- length(X)

  if(!n){
    return(list())
  }

  nworkers <- min(.nworkers, n)
  # partition X
  .chunk_size <- max(min(.chunk_size, ceiling(n/nworkers)), 1)
  njobs <- ceiling(n / .chunk_size)
  mat <- c(seq_along(X), rep(NA, njobs * .chunk_size - n))
  dim(mat) <- c(njobs, .chunk_size)

  if(missing(.log)){
    log <- cat2
  } else {
    log <- .log
  }

  progress <-
    progress2(
      .name,
      max = njobs * .wait + 2,
      shiny_auto_close = TRUE,
      quiet = !.wait || .quiet,
      log = log
    )

  progress$inc('Process data...')

  tempdir(check = TRUE)
  rds <- tempfile()
  saveRDS(list(
    globals = .globals,
    args = list(...),
    fun = FUN
  ), file = rds)

  progress$inc(sprintf('Finished [0/%s]', njobs))

  res <- fastmap2()
  fs <- fastmap2()
  finished <- rep(0, njobs)
  collect_res <- function(timeout = Inf, nw = 0){
    not_finished <- finished == 1
    start <- Sys.time()
    while(sum(not_finished) && sum(not_finished) >= nw){

      if(time_delta(start,Sys.time(), 'secs') > timeout){
        return(structure(res, resolved = FALSE))
      }

      Sys.sleep(0.1)
      for(ii in which(not_finished)){
        code <- fs[[ii]]()
        if(code < 0){
          finished[[ii]] <<- 2
          code <- paste(attr(code, 'rs_exec_error'), collapse = '\n')
          stop(code)
        }
        if(code == 0){
          finished[[ii]] <<- 2
          if(.wait){
            progress$inc(sprintf('Finished [%s/%s]', sum(finished == 2), njobs))
          }
          code <- attr(code, 'rs_exec_result')
          idx <- mat[ii,]
          idx <- idx[!is.na(idx)]
          if(is.list(code) && length(code) == length(idx)){
            for(jj in seq_along(idx)){
              res[[idx[[jj]]]] <- code[[jj]]
            }
          }
        }
      }
      not_finished <- finished == 1
    }

    return(structure(sapply(seq_along(X), function(ii){
      res[[ii]]
    }, simplify = .simplify, USE.NAMES = FALSE), resolved = TRUE))
  }

  attached_pkgs <- attached_packages(include_base = FALSE)

  lapply(seq_len(njobs), function(ii){

    if(sum(finished == 1) >= nworkers){
      collect_res(nworkers)
    }
    name <- sprintf('%s - part %d', .name, ii)
    idx <- mat[ii,]
    idx <- idx[!is.na(idx)]
    el <- X[idx]
    quo <- rlang::quo({
      with(readRDS(!!rds), {
        if(!length(globals)){
          globals <- list()
        }
        return(do.call('lapply', c(
          list(
            !!el,
            do.call(!!mask_function2, list(f = fun, .list = globals))
          ),
          args
        )))
      })
    })
    expr <- rlang::quo_squash(quo)
    fs[[ii]] <- rs_exec(expr, name = name, quoted = TRUE, wait = FALSE,
                        rs = .rs, packages = attached_pkgs)
    finished[[ii]] <<- 1
  })


  if(.wait){
    return(collect_res(nw = 0))
  } else {
    return(invisible(function(timeout = Inf){
      collect_res(nw = 0, timeout = timeout)
    }))
  }
}
