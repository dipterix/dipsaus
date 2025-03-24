get_globals_size <- function(fun, args = list()) {
  # function_globals <- future::getGlobalsAndPackages(list(fun), envir = environment(fun), maxSize = 1e12)$globals

  # parse_env <- new.env(parent = baseenv())
  # parse_env$function_globals <- function_globals
  # fun_internal <- new_function2(body = quote({
  #   force(function_globals)
  # }), env = parse_env, quote_type = "quote")

  call <- as_call(quote(fun), quote(el), .list = args)
  fun_internal <- new_function2(alist(el = ), body = call, quote_type = 'quote', env = environment())
  globals_and_packages <- future::getGlobalsAndPackages(list(fun_internal), maxSize = 1e12)
  re <- attr(globals_and_packages$globals, "total_size")
  return(re)
}

#' Apply, but in parallel
#' @param x vector, list
#' @param FUN function to apply on each element of \code{x}
#' @param FUN.args more arguments to feed into \code{FUN}
#' @param plan logical, or character or \code{future} plan; see Details.
#' @param callback function to run after each iteration
#' @param ... passed to \code{\link[future]{plan}}
#' @param future.chunk.size,future.seed see also \code{future_lapply}.
#' If you want the callbacks
#' to be called immediately after each loop, then set it to \code{1},
#' which is not optimal but the only way right now.
#' @details
#' When \code{plan} is logical, \code{FALSE} means use current plan.
#' If \code{plan=TRUE}, then it equals to \code{plan='multicore'}. For
#' characters, \code{plan} can be \code{'multicore'}, \code{'callr'},
#' \code{'sequential'}, \code{'multisession'}, \code{'multiprocess'},
#' etc. Alternatively, you could pass future \code{\link[future]{plan}}
#' objects.
#' @return same as
#' \code{with(FUN.args, lapply(x, function(el){eval(body(FUN))}))}
#' @examples
#'
#' library(future)
#' plan(sequential)
#'
#' # Use sequential plan
#' # 1. Change `plan` to 'multicore', 'multisession', or TRUE to enable
#' # multi-core, but still with progress information
#' # 2. Change plan=FALSE will use current future plan
#' res <- lapply_async2(100:200, function(x){
#'   return(x+1)
#' }, callback = function(e){
#'   sprintf('Input=%d', e)
#' }, plan = 'sequential')
#'
#' # Disable callback message, then the function reduce to
#' # normal `future.apply::future_lapply`
#' res <- lapply_async2(100:200, function(x){
#'   return(x+1)
#' }, callback = NULL, plan = FALSE)
#'
#' if(interactive()) {
#'
#'   # PID are different, meaning executing in different sessions
#'   lapply_async2(1:4, function(x){
#'     Sys.getpid()
#'   })
#' }
#'
#' @seealso \code{\link{make_forked_clusters}}
#' @export
lapply_async2 <- function(x, FUN, FUN.args = list(),
                          callback = NULL, plan = TRUE,
                          future.chunk.size = NULL, future.seed = sample.int(1, n = 1e5 - 1), ...){

  if(inherits(callback, "formula")) {
    penv <- parent.frame()
    callback <- rlang::as_function(callback, env = penv)
  }

  if( is.function(callback) ){

    callback_formals <- formals(callback)
    if (length(callback_formals)){
      callback_call <- quote(callback(el))
    }else{
      callback_call <- quote(callback())
    }
  }else{
    callback_formals <- list()
    callback_call <- NULL
  }

  # fun_body <- body(FUN)
  # fun_param <- str2lang(names(formals(FUN)[1]))


  call <- as_call(quote(FUN), quote(el), .list = FUN.args)
  if(is.null(callback_call)){
    f <- new_function2(alist(el = ), body = call, quote_type = 'quote', env = environment())
    globals_and_packages <- future::getGlobalsAndPackages(list(f), maxSize = 1e12)

    parallel <- TRUE
    globals_size <- NA
    tryCatch(
      {
        max_globals_size <- getOption("future.globals.maxSize", default = 524288000) # 524288000 = 500MB
        globals_size <- attr(globals_and_packages$globals, "total_size")
        if( isTRUE(globals_size >= max_globals_size) ) {
          future::plan('sequential')
          parallel <- FALSE
        }
      },
      error = function(e) {
        # Nothing
      }
    )

    if( parallel ) {

      if(length(plan) && !isFALSE(plan)){
        if(isTRUE(plan)){
          make_forked_clusters(..., clean = TRUE)
        } else {
          current_plan <- future::plan("list")
          on.exit({
            # Restore plan might encounter errors, use try-catch
            tryCatch({
              future::plan(
                current_plan,
                substitute = FALSE,
                .call = NULL,
                .cleanup = FALSE,
                .init = FALSE
              )
            }, error = function(e){
              future::plan('sequential')
            })
          }, add = TRUE, after = TRUE)
          if(is.character(plan) && plan == 'callr'){
            if (!requireNamespace("future.callr", quietly = TRUE)) {
              stop("Package \"future.callr\" is needed to set plan as 'callr'. Please install it.", call. = FALSE)
            }
            future::plan(future.callr::callr, ..., .call = NULL, .cleanup = TRUE, .init = FALSE)
          }else{
            future::plan(plan, ..., .call = NULL, .cleanup = TRUE, .init = FALSE)
          }
        }
      }

      fs <- future.apply::future_lapply(
        x, f,
        future.scheduling = TRUE,
        future.chunk.size = future.chunk.size,
        future.seed = future.seed,
        future.globals = globals_and_packages$globals,
        future.packages = globals_and_packages$packages
      )
    } else {
      message(sprintf("Using single thread due to large global objects (%s)", to_ram_size(globals_size)))
      fs <- lapply(x, f)
    }
  } else {

    old.handlers <- progressr::handlers(handler_dipsaus_progress())
    on.exit({
      try({
        progressr::handlers(old.handlers)
      }, silent = TRUE)
    }, add = TRUE)

    # if(is.null(shiny::getDefaultReactiveDomain())){

    progressr::with_progress({

      # manually parse globals
      ...p <- progressr::progressor(steps = 2 * length(x) + 2)
      ...FUN2 <- new_function2(args = formals(FUN), env = baseenv())
      ...callback2 <- new_function2(args = formals(callback), env = baseenv())
      ...FUN.args <- FUN.args
      ...FUN_globals <- future::getGlobalsAndPackages(FUN, envir = environment(FUN), maxSize = 1e12)$globals
      ...callback_globals <- future::getGlobalsAndPackages(callback, envir = environment(callback), maxSize = 1e12)$globals
      store_env <- new.env(parent = globalenv())
      store_env$...p <- ...p
      store_env$...FUN2 <- ...FUN2
      store_env$...callback2 <- ...callback2
      store_env$...FUN.args <- ...FUN.args
      store_env$...FUN_globals <- ...FUN_globals
      store_env$...callback_globals <- ...callback_globals

      # print(...FUN_globals)
      ff <- new_function2(body = quote({
        ...p
        ...FUN.args
        ...FUN2
        ...FUN_globals
        ...callback_globals
        ...callback2
      }), env = store_env, quote_type = "quote")

      globals_and_packages <- future::getGlobalsAndPackages(list(FUN, callback, ff), maxSize = 1e12)

      f <- new_function2(alist(el = ), body = bquote({

        # callback
        runtime <- new.env(parent = globalenv())
        list2env(...callback_globals, envir = runtime)
        environment(...callback2) <- runtime
        body(...callback2) <- quote(.(body(callback)))
        if (length(formals(...callback2))){
          msg <- ...callback2(el)
        }else{
          msg <- ...callback2()
        }
        ...p(message = sprintf("%s (started)", msg))
        on.exit({
          ...p(message = sprintf("%s (end)", msg))
        }, add = TRUE, after = TRUE)

        # FUN
        runtime <- new.env(parent = globalenv())
        list2env(...FUN_globals, envir = runtime)

        environment(...FUN2) <- runtime
        body(...FUN2) <- quote(.(body(FUN)))

        re <- do.call(...FUN2, c(list(el), ...FUN.args))
        return(re)

      }), quote_type = "quote", env = new.env(parent = store_env))

      parallel <- TRUE
      globals_size <- NA
      tryCatch(
        {
          max_globals_size <- getOption("future.globals.maxSize", default = 524288000) # 524288000 = 500MB
          globals_size <- attr(globals_and_packages$globals, "total_size")
          if( isTRUE(globals_size >= max_globals_size) ) {
            future::plan('sequential')
            parallel <- FALSE
          }
        },
        error = function(e) {
          # Nothing
        }
      )

      if( parallel ) {

        ...p(message = "Preparing for parallel workers...")

        if(length(plan) && !isFALSE(plan)){
          if(isTRUE(plan)){
            make_forked_clusters(..., clean = TRUE)
          } else {
            current_plan <- future::plan("list")
            on.exit({
              # Restore plan might encounter errors, use try-catch
              tryCatch({
                future::plan(
                  current_plan,
                  substitute = FALSE,
                  .call = NULL,
                  .cleanup = FALSE,
                  .init = FALSE
                )
              }, error = function(e){
                future::plan('sequential')
              })
            }, add = TRUE, after = TRUE)
            if(is.character(plan) && plan == 'callr'){
              if (!requireNamespace("future.callr", quietly = TRUE)) {
                stop("Package \"future.callr\" is needed to set plan as 'callr'. Please install it.", call. = FALSE)
              }
              future::plan(future.callr::callr, ..., .call = NULL, .cleanup = TRUE, .init = FALSE)
            }else{
              future::plan(plan, ..., .call = NULL, .cleanup = TRUE, .init = FALSE)
            }
          }
        }

        fs <- future.apply::future_lapply(
          x, f,
          future.scheduling = TRUE,
          future.chunk.size = future.chunk.size,
          future.seed = future.seed,
          future.globals = globals_and_packages$globals,
          future.packages = globals_and_packages$packages
        )
      } else {
        ...p(message = sprintf("Using single thread due to large global objects (%s)\n", to_ram_size(globals_size)))
        fs <- lapply(x, f)
      }

      ...p("Results collected\n")

        # fs <- future.apply::future_lapply(x, function(el) {
        #   p(message = eval(callback_call))
        #   eval(call)
        # },
        # future.scheduling = TRUE,
        # future.chunk.size = future.chunk.size,
        # future.seed = future.seed)
      }, enable = interactive() || shiny_is_running())
    # }else{
    #   progressr::withProgressShiny({
    #     p <- progressr::progressor(along = x)
    #     fs <- future.apply::future_lapply(x, function(el){
    #       p(message = eval(callback_call))
    #       eval(call)
    #     })
    #   }, handlers = handler_dipsaus_progress)
    # }


  }



  # fs <- lapply(seq_along(x), function(ii){
  #
  #   on.exit(eval(callback_call))
  #
  #   call <- as_call(
  #     quote(future::future),
  #     as_call(
  #       quote(`{`),
  #       as_call(quote(`<-`), fun_param, x[[ii]]),
  #       fun_body
  #     ),
  #     substitute = TRUE,
  #     globals = TRUE
  #   )
  #
  #   eval_dirty(call, env = env, data = FUN.args)
  # })
  # fs = future::value(fs)
  fs
}
