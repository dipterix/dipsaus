# language.R defines functional operations on R languages

#' @title Create new function that supports 'quasi-quosure' syntax
#' @param args named list of function formals
#' @param body function body expression, supports 'quasi-quosure' syntax
#' @param env declare environment of the function
#' @param quote_type character, whether \code{body} is unquoted, quoted,
#' or a 'quo' object (from 'rlang' package)
#' @param quasi_env where the 'quasi-quosure' should be evaluated,
#' default is parent environment
#' @details An unquoted body expression will be quoted, all the
#' expressions with 'quasi-quosure' like \code{!!var} will be evaluated
#' and substituted with the value of \code{var}. For a 'quosure',
#' \code{\link[rlang]{quo_squash}} will be applied. A quoted
#' expression will not be substitute, but will be expanded if any
#' 'quasi-quosure' detected
#'
#' \code{args} must be a \code{list} object, see \code{\link{formals}}.
#' For arguments with no default values, or quoted defaults, use
#' \code{\link{alist}}. An \code{arg=alist(a=)} will result in a
#' function like \code{function(a){...}}. See examples for more details.
#' @seealso \code{\link[rlang]{new_function}}
#' @return a function
#' @examples
#'
#' # ------------ standard usage ------------
#' x <- 1:10
#' f1 <- new_function2(alist(a=), { print(a + x) }, env = environment())
#' f1(0)
#'
#' x <- 20:23
#' f1(0)  # result changed as x changed
#'
#' # ------------ 'quasi-quosure' syntax ------------
#' x <- 1:10
#' f2 <- new_function2(alist(a=), { print(a + !!x) })
#' print(f2)
#'
#' f2(0)
#'
#' x <- 20:23
#' f2(0)  # result doesn't change as f2 doesn't depend on x anymore
#'
#' # ------------ argument settings ------------
#'
#' default <- 123
#'
#' # default with values pre-specified
#' new_function2(list(a = default))   # function (a = 123){}
#'
#' # default with values unevaluated
#' new_function2(list(a = quote(default)))   # function (a = default){}
#' new_function2(alist(a = default))
#'
#' # missing default
#' new_function2(alist(a = ))    # function (a){}
#'
#'
#' @export
new_function2 <- function(args = alist(), body = {},
                          env = parent.frame(),
                          quote_type = c('unquoted', 'quote', 'quo'),
                          quasi_env = parent.frame()){
  quote_type <- match.arg(quote_type)
  switch (
    quote_type,
    'unquoted' = {
      quo <- eval(as.call(list(quote(rlang::quo), substitute(body))), envir = quasi_env)
      body <- rlang::quo_squash(quo)
    },
    'quote' = {
      quo <- eval(as.call(list(quote(rlang::quo), body)), envir = quasi_env)
      body <- rlang::quo_squash(quo)
    },
    'quo' = {
      body <- rlang::quo_squash(quo)
    }
  )
  f <- local({function(){}}, envir = env)
  formals(f) <- args
  body(f) <- body
  f
}



#' @title Mask a function with given variables
#' @description Modifies the default behavior of
#' the function by adding one environment layer on top of input
#' function. The masked variables are assigned directly to the
#' environment.
#' @param f any function
#' @param ...,.list name-value pairs to mask the function
#' @return a masked function
#' @examples
#'
#' a <- 123
#' f1 <- function(){
#'   a + 1
#' }
#' f1()   # 124
#'
#' f2 <- mask_function2(f1, a = 1)
#' f2()   # a is masked with value 1, return 2
#'
#' environment(f1)  # global env
#' environment(f2)  # masked env
#'
#' env <- environment(f2)
#' identical(parent.env(env), environment(f1))  # true
#' env$a  # masked variables: a=1
#'
#' @export
mask_function2 <- function(f, ..., .list = list()){
  f_env <- environment(f)
  if(!isTRUE(f_env$`@...masked`)){
    f_env <- new.env(parent = environment(f))
    environment(f) <- f_env
    f_env$`@...masked` <- TRUE
  }
  list2env(list(...), envir = f_env)
  list2env(.list, envir = f_env)
  f
}



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

#' Left-hand side checked assignment
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

  isnull <- tryCatch({
    is.null(eval(lhs, envir = env))
  }, error = function(e){
    return(TRUE)
  })

  if(isnull){
    eval(as.call(list( quote(`<-`), lhs, value )), envir = env)
  }
}


#' Right-hand side checked assignment
#' Provides a way to avoid assignment to the left-hand side. If the statement
#' `\code{value}` is invalid or \code{NULL}, this function will not assign values and nothing happens.
#' @param lhs an object to be assigned to
#' @param value value to be checked
#'
#' @return Assign value on the right-hand side to the left-hand side if
#' \code{value} does exists and is not \code{NULL}
#'
#' @examples
#' # Prepare, remove aaa if exists
#' if(exists('aaa', envir = globalenv(), inherits = FALSE)){
#'   rm(aaa, envir = globalenv())
#' }
#'
#' # aaa will not be assigned. run `print(aaa)` will raise error
#' aaa %<-?% NULL
#'
#' # Assign
#' aaa %<-?% 1
#' print(aaa)
#'
#' # in a list
#' a = list()
#' a$e %<-?% bbb; print(a$e)
#' a$e %<-?% 2; print(a$e)
#'
#' @export
`%<-?%` <- function(lhs, value){
  env <- parent.frame()
  lhs <- substitute(lhs)
  rhs <- substitute(value)

  tryCatch({
    eval(as.call(list( quote(`<-`), lhs, bquote({
      if(is.null(.(rhs))){ stop() }
      .(rhs)
    }))), envir = env)
  }, error = function(e){
  })
  invisible()
}


#' Get an element with condition that it must be from a list or vector
#' @param lhs the element of candidate
#' @param rhs the constraint
#' @return Returns an element of length one that will be from \code{rhs}
#' @examples
#'
#' # C is from LETTERS, therefore returns `C`
#' "C" %OF% LETTERS
#'
#'
#' # `lhs` is not from `rhs`, hence return the first element of LETTERS
#' '9' %OF% LETTERS
#' NULL %OF% LETTERS
#'
#' # When there are multiple elements from `lhs`, select the first that
#' # matches the constraint
#' c('9', "D", "V") %OF% LETTERS
#'
#' @export
`%OF%` <- function(lhs, rhs){
  if(length(rhs)){ de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if(!length(lhs)){ return(de) }
  sel <- lhs %in% rhs
  if(any(sel)){ return(lhs[sel][[1]]) }
  return(de)
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
  expr <- substitute(expr)
  parent_env <- parent.frame()
  args <- as.list(substitute(args))[-1]

  nms <- sapply(seq_along(args), function(ii){
    nm <- names(args[ii])
    if(is.null(nm) || trimws(nm) == ''){
      nm <- args[[ii]]
      args[[ii]] <<- .missing_arg[[1]]
    }
    nm
  })
  names(args) <- nms
  new_function2(
    args = args, body = expr,
    env = parent_env,
    quote_type = 'quote',
    quasi_env = parent_env)
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
#' y <- x |>
#'   # .expr is a function, all ... are passed as other arguments
#'   no_op( hist, nclass = 10 ) |>
#'   no_op( plot, x = seq(0,1,length.out = 100) ) |>
#'
#'   # Repeat the previous two plots, but with different syntax
#'   no_op({ hist(., nclass = 10) }) |>
#'   no_op({ plot(x = seq(0,1,length.out = 100), y = .) }) |>
#'
#'   # The return statement is ignored
#'
#'   no_op({ return(x + 1)})
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


#' Convert functions to pipe-friendly functions
#' @param x R object as input
#' @param ... default arguments explicitly display in the returned function
#' @param call a function call, or the function itself
#' @param arg_name argument name to be varied. This argument will be the first
#' argument in the new function so it's pipe-friendly.
#' @param .name new argument name; default is the same as \code{arg_name}
#' @param .env executing environment
#' @param .quoted whether \code{call} has been quoted
#' @return If \code{x} is missing, returns a function that takes one argument,
#' otherwise run the function with given \code{x}
#' @examples
#'
#' # modify a function call
#' vary_title <- as_pipe(call = plot(1:10, 1:10),
#'                       pch = 16,
#'                       arg_name = 'main',
#'                       .name = 'title')
#' vary_title
#'
#' # vary_title is pipe-friendly with `pch` default 16
#' vary_title(title = 'My Title')
#'
#' # `pch` is explicit
#' vary_title(title = 'My Title', pch = 1)
#'
#' # other variables are implicit
#' vary_title(title = 'My Title', type = 'l')
#'
#'
#' # modify a function
#'
#' f <- function(b = 1, x){ b + x }
#' f_pipable <- as_pipe(call = f, arg_name = 'x')
#' f_pipable
#'
#' f_pipable(2)
#'
#' # Advanced use
#'
#' # Set option dipsaus.debug.as_pipe=TRUE to debug
#' options("dipsaus.debug.as_pipe" = TRUE)
#'
#' # Both `.(z)` and `z` work
#'
#' image2 <- as_pipe(call = image(
#'   x = seq(0, 1, length.out = nrow(z)),
#'   y = 1:ncol(z),
#'   z = matrix(1:16, 4),
#'   xlab = "Time", ylab = "Freq",
#'   main = "Debug"
#' ), arg_name = 'z')
#'
#' # main can be overwritten
#' image2(matrix(1:50, 5), main = "Production")
#'
#'
#' # reset debug option
#' options("dipsaus.debug.as_pipe" = FALSE)
#'
#'
#' @export
as_pipe <- function(x, ..., call, arg_name,
                    .name = arg_name,
                    .env = parent.frame(),
                    .quoted = FALSE){


  stopifnot(is.character(arg_name))
  if(!.quoted){
    call <- substitute(call)
  }
  call <- as.list(call)

  if( call[[1]] == 'function'){
    # call is a function
    call <- list(as.call(list(quote(`(`), as.call(call))))
  }
  call <- match_calls(call, quoted = TRUE, envir = .env, recursive = FALSE)

  if(!'...' %in% call){
    call[[length(call) + 1]] <- quote(...)
  }
  dot_args <- list(...)
  dot_args <- dot_args[!names(dot_args) %in% '']
  if(length(dot_args)){
    for(nm in names(dot_args)){
      call[[nm]] <- str2lang(nm)
    }
  }
  call <- as.call(call)

  f <- new_function2(c(
    structure(alist(x=, y=), names = c(.name, "...")),
    dot_args
  ), bquote({

    # remove duplicated ...
    dots <- as.list(match.call(expand.dots = FALSE))[["..."]]
    dots <- dots[!names(dots) %in% '']
    call <- as.list(bquote(.( call )))
    if(length(dots)){
      for(nm in names(dots)){
        call[[nm]] <- NULL
      }
    }
    call[[.(arg_name)]] <- quote(.(str2lang(.name)))
    eval(as.call(call))
  }), quote_type = 'quote', env = .env)

  call_ <- call
  call_[[arg_name]] <- sprintf("[input:%s]", .name)
  attr(f, "call") <- call_
  class(f) <- c("f_pipe", "function")

  if( !missing(x) ){
    print(x)
    return(f(x))
  } else {
    if(getOption("dipsaus.debug.as_pipe", FALSE)){
      e <- local({
        tryCatch({
          call <- as.list(call)
          call <- call[call != "..."]
          call <- as.call(call)
          .input <- eval(call[[arg_name]], envir = list(), enclos = .env)
          f(.input)
          NULL
        }, error = function(e){
          e
        })
      })
      if(is.null(e)){
        e <- 'options("dipsaus.debug.as_pipe") is set to TRUE. This should be used only in debug mode. Do not use for production!'
        message(e)
      } else {
        warning(e, immediate. = TRUE, call. = FALSE)
      }

    }


    return(f)

  }

}

#' @export
print.f_pipe <- function(x, ...){
  cat("<Pipe-compatible function>\n")
  print(dipsaus::new_function2(formals(x), bquote({
    .(attr(x, "call"))
  }), quote_type = "quote"))
  invisible(x)
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
      da <- la
    }
    return(array(c( re1, a - b), dim = c(da, 2)))
  }
}


as_call <- function(..., .list=list(), .drop_nulls = FALSE){
  call <- c(list(...), .list)
  if('...' %in% names(call)){
    call[['...']] <- NULL
    call[[length(call) + 1]] <- quote(...)
  }
  if (.drop_nulls) {
    call <- call[!vapply(call, is.null, FUN.VALUE = FALSE)]
  }
  as.call(call)
}


#' @name decorate_function
#' @title Python-style decorator
#' @param orig,lhs any function
#' @param decor,rhs decorator function that takes \code{orig} as its first
#' argument
#' @param ... passed to \code{decor}
#' @examples
#'
#'
#' # Example 1: basic usage
#' # Decorator that prints summary of results and return results itself
#' verbose_summary <- function(...){
#'   summary_args <- list(...)
#'   function(f){
#'     function(...){
#'       results <- f(...)
#'
#'
#'       print(do.call(
#'         summary,
#'         c(list(results), summary_args)
#'       ))
#'       results
#'
#'     }
#'   }
#'
#' }
#'
#' # runs as.list, but through verbose_summary
#' as_list2 <- decorate_function(as.list, verbose_summary)
#'
#' # run test
#' res <- as_list2(1:3)  # will verbose summary
#' identical(res, as.list(1:3))
#'
#' # Example 2
#' x <- 1:20
#' y <- x + rnorm(20)
#'
#' # decorator, add a line with slope 1 with given intercept
#' abline_xy <- function(b){
#'   function(f){
#'     function(...){
#'       f(...)
#'       intercept <- get_dots('intercept', 0, ...)
#'       abline(a = intercept, b = b)
#'     }
#'   }
#' }
#'
#' # orig, plot whatever x vs jittered+intercept
#' plot_xy <- function(x, intercept = rnorm(1)){
#'   plot(x, jitter(x, amount = 3) + intercept)
#' }
#'
#' # new function that decorate plot_xy with abline_xy, and
#' # returns the intercept
#' plot_xy2 <- decorate_function(plot_xy, abline_xy, b = 1)
#'
#' # alternatively, you might also want to try
#' plot_xy2 <- plot_xy %D% abline_xy(b = 1)
#'
#' plot_xy2(x = 1:20)
#'
#' @export
decorate_function <- function(orig, decor, ...){

  stopifnot2(is.function(orig) && is.function(decor),
             msg = 'Decoration orig and decor must be functions')
  current_call <- match.call()
  current_call[['orig']] <- NULL
  current_call[['decor']] <- NULL
  decor_name <- match.call(decor, call = current_call)
  decor_name[[1]] <- substitute(decor)
  re <- decor(...)(orig)

  stopifnot2(is.function(re), msg = 'Decoration must return a function')

  orig_name <- attr(orig, 'dipsaus_decorators')
  if(is.null(orig_name)){
    attr(re, 'dipsaus_origin')  <- orig
  } else {
    attr(re, 'dipsaus_origin') <- attr(orig, 'dipsaus_origin')
  }
  attr(re, 'dipsaus_decorators') <- c(
    decor_name,
    orig_name
  )

  class(re) <- c('dipsaus_decorated', class(re))
  re
}

#' @rdname decorate_function
#' @export
`%D%` <- function(lhs, rhs){
  rhs <- substitute(rhs)
  parent_env <- parent.frame()
  call <- as_call(decorate_function, orig = lhs, decor = rhs[[1]], .list = as.list(rhs)[-1])

  return(eval(call, envir = parent_env))
}

#' @export
print.dipsaus_decorated <- function(x, ...){

  call <- as_call(quote(list), .list = formals(x))
  call[[1]] <- quote('Decorated function')
  print(call)
  calls <- attr(x, 'dipsaus_decorators')
  cat('- Decorators: \n')
  for(d in rev(calls)){
    cat('  ', end = '')
    print(d)
  }
  cat('- Original function:\n')
  fs <- utils::capture.output(print(attr(x, 'dipsaus_origin')))
  cat(paste0('  ', fs, collapse = '\n'), end = '\n')
}


#' @title Get or check elements from dots \code{'...'}
#' @description Get information from \code{'...'} without
#' evaluating the arguments.
#' @param envir R environment
#' @param ..name character name of the argument
#' @param ..default R object to return if argument not found
#' @param ... dots that contains argument
#'
#' @return \code{missing_dots} returns logical vector with lengths matching
#' with dot lengths. \code{get_dots} returns value corresponding to the name.
#'
#' @examples
#'
#'
#' # ------------------------ Basic Usage ---------------------------
#'
#' # missing_dots(environment()) is a fixed usage
#'
#' my_function <- function(...){
#'   missing_dots(environment())
#' }
#' my_function(,)
#'
#' # get_dots
#' plot2 <- function(...){
#'   title = get_dots('main', 'There is no title', ...)
#'   plot(...)
#'   title
#' }
#'
#' plot2(1:10)
#' plot2(1:10, main = 'Scatter Plot of 1:10')
#'
#' # ------------------------ Comparisons ----------------------------
#' f1 <- function(...){ get_dots('x', ...) }
#' f2 <- function(...){ list(...)[['x']] }
#' delayedAssign('y', { cat('y is evaluated!') })
#'
#' # y will not evaluate
#' f1(x = 1, y = y)
#'
#' # y gets evaluated
#' f2(x = 1, y = y)
#'
#' # -------------------- Decorator example --------------------------
#' ret_range <- function(which_range = 'y'){
#'   function(f){
#'     function(...){
#'       f(...)
#'       y_range <- range(get_dots(which_range, 0, ...))
#'       y_range
#'     }
#'   }
#' }
#' plot_ret_yrange <- plot %D% ret_range('y')
#' plot_ret_yrange(x = 1:10, y = rnorm(10))
#'
#'
#' @export
get_dots <- function(..name, ..default = NULL, ...){
  call <- as.list(match.call(expand.dots = TRUE))[-1]
  call <- call[!names(call) %in% c('..name', '..default')]
  if(..name %in% names(call)){
    idx <- which(names(call) == ..name)[1]
    return(...elt(idx))
  }else{
    return(..default)
  }
}

#' @rdname get_dots
#' @export
missing_dots <- function(envir = parent.frame()){
  if(!is.environment(envir)){
    stop("missing_dots: `envir` must be an environment")
  }
  check_missing_dots(envir)
}

#' Test whether function has certain arguments
#' @param fun function
#' @param arg characters of function arguments
#' @param dots whether \code{fun}'s dots (\code{...}) counts
#' @examples
#'
#' a <- function(n = 1){}
#'
#' # Test whether `a` has argument called 'b'
#' test_farg(a, 'b')
#'
#' # Test whether `a` has argument called 'b' and 'n'
#' test_farg(a, c('b', 'n'))
#'
#' # `a` now has dots
#' a <- function(n = 1, ...){}
#'
#' # 'b' could goes to dots and a(b=...) is still valid
#' test_farg(a, 'b')
#'
#' # strict match, dots doesn't count
#' test_farg(a, 'b', dots = FALSE)
#'
#' @export
test_farg <- function(fun, arg, dots = TRUE){
  stopifnot2(is.character(arg) || is.numeric(arg),
             msg = 'test_farg: arg must be either characters or integers')
  fm <- names(formals(fun))
  has_dots <- dots && ('...' %in% fm)
  if(has_dots){
    return(rep(TRUE, length(arg)))
  }
  if(is.character(arg)){
    arg %in% fm
  } else {
    arg <= length(fm)
  }
}


#' Check whether a function, environment comes from a namespace
#' @description
#' A coarse way to find if a function comes from a package.
#'
#' @param x function, environment, language (with environment attached)
#' @param recursive whether to recursively search parent environments
#' @returns logical true if \code{x} or its environment is
#' defined in a namespace; returns false if the object is atomic, or defined
#' in/from global environment, or an empty environment.
#' @examples
#'
#'
#' is_from_namespace(baseenv())        # TRUE
#' is_from_namespace(utils::read.csv)  # TRUE
#'
#' x <- function(){}
#' is_from_namespace(NULL)             # FALSE
#' is_from_namespace(x)                # FALSE
#' is_from_namespace(emptyenv())       # FALSE
#'
#' # Let environment of `x` be base environment
#' # (exception case)
#' environment(x) <- baseenv()
#' is_from_namespace(x)        # TRUE
#'
#'
#' @export
is_from_namespace <- function(x, recursive = TRUE) {
  if(is.null(x)) { return(FALSE) }
  if( recursive ) {
    recursive <- TRUE
  } else {
    recursive <- FALSE
  }
  return( is_env_from_package(x, recursive) )
}
