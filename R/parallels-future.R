

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
        future::plan(future.callr::callr, ..., .call = NULL, .cleanup = FALSE, .init = FALSE)
      }else{
        future::plan(plan, ..., .call = NULL, .cleanup = FALSE, .init = FALSE)
      }
    }
  }

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
    f <- dipsaus::new_function2(alist(el = ), body = call, quote_type = 'quote', env = environment())
    fs <- future.apply::future_lapply(
      x, f, future.seed = future.seed,
      future.scheduling = TRUE,
      future.chunk.size = future.chunk.size)
  } else {

    old.handlers <- progressr::handlers(handler_dipsaus_progress())
    on.exit({
      try({
        progressr::handlers(old.handlers)
      }, silent = TRUE)
    }, add = TRUE)

    # if(is.null(shiny::getDefaultReactiveDomain())){

    progressr::with_progress({
      ...p <- progressr::progressor(steps = 2 * length(x) + 1)

      # f <- function(el, ...) {
      #   msg <- ""
      #   if( is.function(callback) ){
      #     callback_formals <- formals(callback)
      #     if (length(callback_formals)){
      #       msg <- callback(el)
      #     }else{
      #       msg <- callback()
      #     }
      #   }
      #   if(is.character(msg)) {
      #     msg <- paste(msg, collapse = "")
      #   } else {
      #     msg <- deparse(el, width.cutoff = 30)
      #     if(length(msg) > 1){
      #       msg <- msg[[1]]
      #     }
      #     if(nchar(msg) >= 10){
      #       msg <- sprintf("%s...", substr(msg, stop = 7, start = 1))
      #     }
      #   }
      #
      #   p(message = sprintf("%s (started)", msg))
      #   on.exit({
      #     p(message = sprintf("%s (end)", msg))
      #   }, add = TRUE, after = TRUE)
      #
      #   return(FUN(el, ...))
      # }

      # f <- dipsaus::new_function2(alist(el = ), body = bquote({
      #   ...msg... <- .(callback_call)
      #   if(is.character(...msg...)) {
      #     ...msg... <- paste(...msg..., collapse = "")
      #   } else {
      #     ...msg... <- deparse(el, width.cutoff = 30)
      #     if(length(...msg...) > 1){
      #       ...msg... <- ...msg...[[1]]
      #     }
      #     if(nchar(...msg...) >= 10){
      #       ...msg... <- sprintf("%s...", substr(...msg..., stop = 7, start = 1))
      #     }
      #   }
      #   p(message = sprintf("%s (started)", ...msg...), )
      #   on.exit({
      #     p(message = sprintf("%s (end)", ...msg...))
      #   }, add = TRUE, after = TRUE)
      #
      #   .(call)
      #
      # }), quote_type = "quote", env = environment())

      # manually parse globals
      ...FUN2 <- dipsaus::new_function2(args = formals(FUN), env = baseenv())
      ...callback2 <- dipsaus::new_function2(args = formals(callback), env = baseenv())
      ...FUN.args <- FUN.args

      ff <- function() {
        ...p
        ...FUN.args
        ...FUN2
        ...callback2
      }
      globals_and_packages <- future::getGlobalsAndPackages(c(FUN, callback, ff))

      f <- dipsaus::new_function2(alist(el = ), body = bquote({

        ...dipsaus_msg <- local({
          callback_formals <- formals(...callback2)
          environment(...callback2) <- new.env(parent = globalenv())
          body(...callback2) <- quote(.(body(callback)))
          if (length(callback_formals)){
            msg <- ...callback2(el)
          }else{
            msg <- ...callback2()
          }
          if(is.character(msg)) {
            msg <- paste(msg, collapse = "")
          } else {
            msg <- deparse(el, width.cutoff = 30)
            if(length(msg) > 1){
              msg <- msg[[1]]
            }
            if(nchar(msg) >= 10){
              msg <- sprintf("%s...", substr(msg, stop = 7, start = 1))
            }
          }
          msg
        })

        ...p(message = sprintf("%s (started)", ...dipsaus_msg))

        on.exit({
          ...p(message = sprintf("%s (end)", ...dipsaus_msg))
        }, add = TRUE, after = TRUE)

        environment(...FUN2) <- new.env(parent = globalenv())
        body(...FUN2) <- quote(.(body(FUN)))
        return(do.call(...FUN2, c(list(el), ...FUN.args)))

      }), quote_type = "quote", env = new.env(parent = globalenv()))

      fs <- future.apply::future_lapply(
        x, f,
        future.scheduling = TRUE,
        future.chunk.size = future.chunk.size,
        future.seed = future.seed,
        future.globals = globals_and_packages$globals,
        future.packages = globals_and_packages$packages
      )
      # fs <- future.apply::future_lapply(x, f,
      #                                   future.scheduling = TRUE,
      #                                   future.chunk.size = future.chunk.size,
      #                                   future.seed = future.seed)

      p("Results collected\n")

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
