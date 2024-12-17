wrap_callback <- function(.callback){
  if(!is.function(.callback)){
    return()
  }
  fmcb <- formals(.callback)
  if(length(fmcb) >= 2 || "..." %in% names(fmcb)){
    callback <- function(el, ii){
      .callback(el, ii)
    }
  } else if (length(fmcb) == 1){
    callback <- function(el, ii){
      .callback(el)
    }
  } else {
    callback <- function(el, ii){
      .callback()
    }
  }
  function(el, ii){
    re <- tryCatch({
      callback(el, ii)
    }, error = function(e){
      ii
    })
    if(!length(re)){
      return(ii)
    } else {
      return(re[[1]])
    }
  }
}

#' @title Apply function with \code{rs_exec}
#' @seealso \code{\link{rs_exec}}
#' @param x vector or list
#' @param fun function
#' @param ... passed to function, see \code{\link{lapply}}
#' @param .callback a function takes zero, one, or two arguments
#' and should return a string to show in the progress
#' @param .globals a named list that \code{fun} relies on
#' @param .ncores number of cores to use; only used when \code{.wait=TRUE}
#' @param .packages packages to load
#' @param .focus_on_console whether to focus on console once finished;
#' is only used when \code{.rs} is true
#' @param .rs whether to create 'RStudio' jobs; default is false
#' @param .quiet whether to suppress progress message
#' @param .name the name of progress and jobs
#' @param .wait whether to wait for the results; default is true, which blocks
#' the main session waiting for results.
#' @returns When \code{.wait=TRUE}, returns a list that should be, in most of
#' the cases, identical to \code{\link{lapply}}; when \code{.wait=FALSE},
#' returns a function that collects results.
#' @examples
#'
#' if(interactive()){
#'
#'   lapply_callr(1:3, function(x, a){
#'     c(Sys.getpid(), a, x)
#'   }, a = 1)
#'
#'   lapply_callr(1:30, function(x)
#'     {
#'       Sys.sleep(0.1)
#'       sprintf("a + x = %d", a + x)
#'     }, .globals = list(a = 1),
#'     .callback = I, .name = "Test")
#'
#' }
#'
#' @export
lapply_callr <- function(
  x, fun, ..., .callback = NULL,
  .globals = list(), .ncores = future::availableCores(),
  .packages = attached_packages(),
  .focus_on_console = TRUE, .rs = FALSE, .quiet = FALSE,
  .name = "", .wait = TRUE){

  stopifnot(.ncores >= 1)

  nm <- names(formals(fun))[[1]]
  tempdir(check = TRUE)
  f <- tempfile(fileext = '.rds')
  old.handlers <- progressr::handlers(handler_dipsaus_progress())
  on.exit({
    try({
      progressr::handlers(old.handlers)
    }, silent = TRUE)
    if(.wait){
      unlink(f)
    }
  }, add = TRUE)
  globals <- list(
    formal = formals(fun),
    body = body(fun),
    globals = .globals,
    args = c(structure(list(""), names = nm), list(...))
  )
  saveRDS(globals, file = f)

  res <- fastmap2()

  queue <- fastqueue2()

  if( .quiet ){
    callback <- NULL
  } else {
    callback <- wrap_callback(.callback)
  }
  .packages <- unique('dipsaus', .packages)


  p <- NULL

  error <- FALSE
  error_msg <- ""

  progressr::with_progress(expr = {
    if(is.function(callback)){
      p <- progressr::progressor(along = x)
    }


    lapply(seq_along(x), function(ii){
      if(error){ return() }
      while( .wait && queue$size() >= .ncores ){
        item <- queue$remove()
        if( (code <- item$check()) > 0 ){
          queue$add(item)
          Sys.sleep(0.2)
        } else {
          if( code < 0 ){
            # stop(attr(code, "rs_exec_error"), call. = FALSE)
            error <<- TRUE
            error_msg <<- attr(code, "rs_exec_error")
            queue$reset()
          }
          res[[item$index]] <- attr(code, "rs_exec_result")

          if(is.function(callback)){
            p(message = callback(x[[item$index]], item$index))
          }
        }
      }


      h <- rs_exec(bquote({
        if( dir.exists(.(getwd()))){
          do.call("setwd", list(.(getwd())))
        }

        .env <- new.env(parent = globalenv())
        if(!file.exists(.(f))){
          stop("Job stopped")
        }
        .globals <- readRDS(.(f))
        .genv <- parent.env(.env)

        list2env(.globals$globals, envir = .genv)
        .globals$args[[.(nm)]] <- .(x[[ii]])

        do.call(
          new_function2(
            args = .globals$formal,
            body = .globals$body,
            env = .env,
            quote_type = 'quote'
          ),
          .globals$args
        )
      }), rs = .rs, quoted = TRUE,
      name = sprintf("%s... - [%d]", .name, ii),
      wait = FALSE, packages = .packages,
      focus_on_console = FALSE, ignore.stdout = TRUE)
      queue$add(list(
        check = h,
        index = ii
      ))
    })

    if( .rs && .focus_on_console ){
      rs_focus_console(wait = 0.5)
    }
    if( error ){
      queue$reset()
    }
    while( .wait && queue$size() > 0 ){
      item <- queue$remove()

      if( (code <- item$check()) > 0 ){
        queue$add(item)
        Sys.sleep(0.2)
      } else {
        if( code < 0 ){
          # stop(attr(code, "rs_exec_error"), call. = FALSE)
          error <- TRUE
          error_msg <- attr(code, "rs_exec_error")
          queue$reset()
        }
        res[[item$index]] <- attr(code, "rs_exec_result")

        if(is.function(callback)){
          p(message = callback(x[[item$index]], item$index))
        }
      }
    }
  }, enable = interactive() || shiny_is_running())

  if( error ){
    stop(error_msg, call. = FALSE)
  }

  if( .wait ){
    keys_idxs <- as.integer(names(res))
    structure(res[sort.int(keys_idxs)], names = names(x))
  } else {
    return(function(){
      while( queue$size() > 0 ){
        item <- queue$remove()
        if( (code <- item$check()) > 0 ){
          queue$add(item)
          Sys.sleep(0.2)
        } else {
          if( code < 0 ){
            # stop(attr(code, "rs_exec_error"), call. = FALSE)
            stop(attr(code, "rs_exec_error"))
          }
          res[[item$index]] <- attr(code, "rs_exec_result")
        }
      }
      keys_idxs <- as.integer(names(res))
      structure(res[sort.int(keys_idxs)], names = names(x))
    })
  }

}

