# language.R defines functional operations on R languages

#' Recursively match calls and modify arguments
#' @param call an \code{R} expression
#' @param recursive logical, recursively match calls, default is true
#' @param replace_args named list of functions, see examples
#' @param quoted logical, is \code{call} quoted
#' @param envir which environment should call be evaluated
#' @param ... other parameters passing to \code{match.call}
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
match_calls = function(call, recursive = TRUE, replace_args = list(),
                       quoted = FALSE, envir = parent.frame(), ...){
  if(!quoted){ call = substitute(call) }
  args = as.list(match.call())[-1]
  args$recursive = quote(recursive)
  args$replace_args = quote(replace_args)
  args$envir = envir
  args$quoted = TRUE
  if(is.call(call)){
    if( recursive ){
      call = as.call(lapply(call, function(cp){
        args$call = quote(cp)
        do.call(match_calls, args)
      }))
    }
    if( !is.primitive(eval(call[[1]], envir = envir)) ){
      call = eval(as.call(list(quote(match.call), call[[1]], enquote(call))), envir = envir)
    }

    repl_name = names(replace_args); repl_name = repl_name[repl_name %in% names(call)]
    if(length(repl_name)){
      for(nm in repl_name){
        call[[nm]] = replace_args[[nm]](call[[nm]], call)
      }
    }
    return(call)
  }
  call
}



