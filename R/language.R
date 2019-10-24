# language.R defines functional operations on R languages

#' Recursively match calls and modify arguments
#' @param call an \code{R} expression
#' @param recursive logical, recursively match calls, default is true
#' @param replace_args named list of functions, see examples
#' @param quoted logical, is \code{call} quoted
#' @param envir which environment should call be evaluated
#' @param ... other parameters passing to \code{match.call}
#'
#' @return A nested call with all arguments matched
#'
#' @examples
#' library(dipsaus); library(shiny)
#'
#' # In shiny modules, we might want to add ns() to inputIds
#' # In this example, textInput(id) will become textInput(ns(id))
#' match_calls(lapply(1:20, function(i){
#'   textInput(paste('id_', i), paste('Label ', i))
#' }), replace_args = list(
#'   inputId = function(arg, call){ as.call(list(quote(ns), arg)) }
#' ))
#'
#' @export
match_calls <- function(call, recursive = TRUE, replace_args = list(),
                       quoted = FALSE, envir = parent.frame(), ...){
  if(!quoted){ call <- substitute(call) }
  args <- as.list(match.call())[-1]
  args$recursive <- quote(recursive)
  args$replace_args <- quote(replace_args)
  args$envir <- envir
  args$quoted <- TRUE
  if(is.call(call)){
    if( recursive ){
      call <- as.call(lapply(call, function(cp){
        args$call <- quote(cp)
        do.call(match_calls, args)
      }))
    }
    if( !is.primitive(eval(call[[1]], envir = envir)) ){
      call <- eval(
        as.call(list(quote(match.call), call[[1]], enquote(call))),
        envir = envir)
    }

    repl_name <- names(replace_args)
    repl_name <- repl_name[repl_name %in% names(call)]
    if(length(repl_name)){
      for(nm in repl_name){
        call[[nm]] <- replace_args[[nm]](call[[nm]], call)
      }
    }
    return(call)
  }
  call
}



#' Evaluate expressions
#'
#' @param expr R expression or 'rlang' quo
#' @param env environment to evaluate
#' @param data dataframe or list
#' @param quoted Is the expression quoted? By default, this is \code{TRUE}.
#' This is useful when you don't want to use an expression that is stored in a
#' variable; see examples
#'
#' @details \code{eval_dirty} uses \code{base::eval()} function to evaluate
#' expressions. Compare to \code{rlang::eval_tidy}, which won't affect original
#' environment, \code{eval_dirty} causes changes to the environment. Therefore
#' if \code{expr} contains assignment, environment will be changed in this case.
#'
#' @return the executed results of \code{expr} evaluated with side effects.
#'
#' @examples
#'
#' env = new.env(); env$a = 1
#' rlang::eval_tidy(quote({a <- 111}), env = env)
#' print(env$a)  # Will be 1. This is because eval_tidy has no side effect
#'
#' eval_dirty(quote({a <- 111}), env)
#' print(env$a)  # 111, a is changed
#'
#' # Unquoted case
#' eval_dirty({a <- 222}, env, quoted = FALSE)
#' print(env$a)
#'
#' @export
eval_dirty <- function(expr, env = parent.frame(), data = NULL, quoted = TRUE){

  if( !quoted ){
    expr <- substitute( expr )
  }
  if(rlang::is_quosure(expr)){
    expr <- rlang::quo_squash(expr)
  }

  if(!is.null(data)){
    return(eval(expr, enclos = env, envir = data))
  }else{
    return(eval(expr, envir = env))
  }
}

#' Assign if not exists, or NULL
#' Provides a way to assign default values to variables. If the statement
#' `\code{lhs}` is invalid or \code{NULL}, this function will try to assign
#' \code{value}, otherwise nothing happens.
#' @param lhs an object to check or assign
#' @param value value to be assigned if lhs is NULL
#'
#' @examples
#' # Prepare, remove aaa if exists
#' if(exists('aaa', envir = globalenv(), inherits = FALSE)){
#'   rm(aaa, envir = globalenv())
#' }
#'
#' # Assign
#' aaa %?<-% 1; print(aaa)
#'
#' # However, if assigned, nothing happens
#' aaa = 1;
#' aaa %?<-% 2;
#' print(aaa)
#'
#' # in a list
#' a = list()
#' a$e %?<-% 1; print(a$e)
#' a$e %?<-% 2; print(a$e)
#'
#' @export
`%?<-%` <- function(lhs, value){
  env <- parent.frame()
  lhs <- substitute(lhs)

  tryCatch({
    is.null(eval(lhs, envir = env))
  }, error = function(e){
    return(TRUE)
  }) ->
    isnull

  if(isnull){
    eval(as.call(list( str2lang('`<-`'), lhs, value )), envir = env)
  }
}


