

#' Apply, but in parallel
#' @param x vector, list
#' @param FUN function to apply on each element of \code{x}
#' @param FUN.args more arguments to feed into \code{FUN}
#' @param env environment to run, default to \code{FUN}'s environment
#' @param plan logical, or character or \code{future} plan; see Details.
#' @param callback function to run after each iteration
#' @param ... passed to \code{\link[future]{plan}}
#' @details
#' When \code{plan} is logical, \code{FALSE} means use current plan.
#' If \code{plan=TRUE}, then it equals to \code{plan='multicore'}. For
#' characters, \code{plan} can be \code{'multicore'}, \code{'callr'},
#' \code{'sequential'}, \code{'multisession'}, \code{'multiprocess'},
#' etc. Alternatively, you could pass future \code{\link[future]{plan}}
#' objects.
#' @return same as
#' \code{with(FUN.args, lapply(x, function(el){eval(body(FUN))}))}
#' @export
lapply_async2 <- function(x, FUN, FUN.args = list(), env = environment(FUN),
                          callback = NULL, plan = TRUE, ...){
  if(length(plan) && !isFALSE(plan)){
    if(isTRUE(plan)){
      make_forked_clusters(...)
    } else if(is.character(plan) && plan == 'callr'){
      if (!requireNamespace("future.callr", quietly = TRUE)) {
        stop("Package \"future.callr\" is needed to set plan as 'callr'. Please install it.", call. = FALSE)
      }
      future::plan(future.callr::callr, ...)
    }else{
      future::plan(plan, ...)
    }
  }

  if( is.function(callback) ){
    callback_formals <- formals(callback)
    if(length(callback_formals) >= 2 || '...' %in% names(callback_formals)){
      callback_call <- quote(callback(x[[ii]], ii))
    }else if (length(callback_formals)){
      callback_call <- quote(callback(ii))
    }else{
      callback_call <- quote(callback())
    }
  }else{
    callback_call <- NULL
  }

  fun_body <- body(FUN)
  fun_param <- str2lang(names(formals(FUN)[1]))

  fs <- lapply(seq_along(x), function(ii){

    on.exit(eval(callback_call))

    call <- as_call(
      quote(future::future),
      as_call(
        quote(`{`),
        as_call(quote(`<-`), fun_param, x[[ii]]),
        fun_body
      ),
      substitute = TRUE,
      globals = TRUE
    )

    eval_dirty(call, env = env, data = FUN.args)
  })
  fs = future::values(fs)
  fs
}
