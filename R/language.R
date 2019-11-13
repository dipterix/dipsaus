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
#' @return Assign value on the right-hand side to the left-hand side if
#' \code{lhs} does not exist or is \code{NULL}
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



#' A JavaScript style of creating functions
#' @param args function arguments: see \code{\link[base]{formals}}
#' @param expr R expression that forms the body of functions: see \code{\link[base]{body}}
#' @return A function that takes \code{args} as parameters and \code{expr} as
#' the function body
#'
#' @examples
#' # Formal arguments
#' c(a) %=>% {
#'   print(a)
#' }
#'
#' # Informal arguments
#' list(a=) %=>% {
#'   print(a)
#' }
#'
#' # Multiple inputs
#' c(a, b = 2, ...) %=>% {
#'   print(c(a, b, ...))
#' }
#'
#' # ----- JavaScript style of forEach -----
#' # ### Equivalent JavaScript Code:
#' # LETTERS.forEach((el, ii) => {
#' #   console.log('The index of letter ' + el + ' in "x" is: ' + ii);
#' # });
#'
#' iapply(LETTERS, c(el, ii) %=>% {
#'   cat2('The index of letter ', el, ' in ', sQuote('x'), ' is: ', ii)
#' }) -> results
#' @export
`%=>%` <- function(args, expr){
  expr = substitute(expr)
  parent_env = parent.frame()
  args = as.list(substitute(args))[-1]

  nms = sapply(seq_along(args), function(ii){
    nm = names(args[ii])
    if(is.null(nm) || stringr::str_trim(nm) == ''){
      nm = args[[ii]]
      args[[ii]] <<- .missing_arg[[1]]
    }
    nm
  })
  names(args) <- nms
  rlang::new_function(args, expr, parent_env)
}


#' Pipe-friendly no-operation function
#' @description returns the first input with side effects
#' @param .x any R object
#' @param .expr R expression that produces side effects
#' @param ...,.check_fun see `details`
#' @return The value of \code{.x}
#' @details
#' \code{no_op} is a pipe-friendly function that takes any values in,
#' evaluate expressions but still returns input. This is very useful when
#' you have the same input across multiple functions and you want to use pipes.
#'
#' \code{.expr} is evaluated with a special object \code{'.'}, you can use
#' \code{'.'} to represent \code{.x} in \code{.expr}. For example, if
#' \code{.x=1:100}, then \code{plot(x=seq(0,1,length.out = 100), y=.)} is
#' equivalent to \code{plot(x=seq(0,1,length.out = 100), y=1:100)}.
#'
#' \code{.check_fun} checks whether \code{.expr} returns a function, if yes,
#' then the function is called with argument \code{.x} and \code{...}
#'
#' @examples
#'
#' library(magrittr)
#'
#' ## 1. Basic usage
#'
#' # Will print('a') and return 'a'
#' no_op('a', print)
#'
#' # Will do nothing and return 'a' because .check_fun is false
#' no_op('a', print, .check_fun = FALSE)
#'
#' # Will print('a') and return 'a'
#' no_op('a', print(.), .check_fun = FALSE)
#'
#' ## 2. Toy example
#' library(graphics)
#'
#' par(mfrow = c(2,2))
#' x <- rnorm(100)
#'
#' # hist and plot share the same input `rnorm(100)`
#'
#' x %>%
#'   # .expr is a function, all ... are passed as other arguments
#'   no_op( hist, nclass = 10 ) %>%
#'   no_op( plot, x = seq(0,1,length.out = 100) ) %>%
#'
#'   # Repeat the previous two plots, but with different syntax
#'   no_op({ hist(., nclass = 10) }) %>%
#'   no_op({ plot(x = seq(0,1,length.out = 100), y = .) }) %>%
#'
#'   # The return statement is ignored
#'
#'   no_op({ return(x + 1)}) ->
#'   y
#'
#' # x is returned at the end
#'
#' identical(x, y)   # TRUE
#'
#' @export
no_op <- function(.x, .expr, ..., .check_fun = TRUE){
  .expr <- substitute(.expr)
  ...parent_env <- parent.frame()
  ...res <- eval(.expr, envir = list('.' = .x), enclos = ...parent_env)
  if( .check_fun && is.function(...res) ){
    ...res( .x, ... )
  }
  invisible(.x)
}

#' Plus-minus operator
#' @param a,b numeric vectors, matrices or arrays
#' @return \code{a +/- b}, the dimension depends on \code{a+b}. If \code{a+b} is
#' a scalar, returns a vector of two; in the case of vector, returns a matrix;
#' all other cases will return an array with the last dimension equal to 2.
#' @examples
#'
#' # scalar
#' 1 %+-% 2   # -1, 3
#'
#' # vector input
#' c(1,2,3) %+-% 2   # matrix
#'
#' # matrix input
#' matrix(1:9, 3) %+-% 2   # 3x3x2 array
#'
#' @export
`%+-%` <- function(a, b){
  re1 <- a + b
  la <- length( re1 )

  if( la <= 1 ){
    return( c( re1, a - b) )
  }else{
    da <- dim( re1 )
    if( !length(da) ){
      da = la
    }
    return(array(c( re1, a - b), dim = c(da, 2)))
  }
}
