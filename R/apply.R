#' Apply each elements with index as second input
#' @param X a vector (atomic or list)
#' @param FUN the function to be applied to each element of \code{X}: see `Details`.
#' @param ... passed to apply methods
#' @param .method method to use, default is \code{\link{sapply}}
#' @description Apply function with an index variable as the second input.
#' @details \code{FUN} will be further passed to the apply methods. Unlike
#' \code{\link{lapply}}, \code{FUN} is expected to have at least two arguments.
#' The first argument is each element of \code{X}, the second argument is the
#' index number of the element.
#' @return a list or matrix depends on \code{.method}. See \code{\link[base]{lapply}}
#' @export
iapply <- function(X, FUN, ..., .method = c('sapply', 'lapply', 'vapply')) {
  .method <- match.arg(.method)
  .formals <- formals(FUN)

  if( length(.formals) >= 2 || '...' %in% names(.formals) ){
    do.call(
      .method,
      list( seq_along(X), function(ii, ...){
        FUN(X[[ii]], ii, ...)
      }, ...)
    )
  }else{
    do.call(.method, list(X, FUN, ...))
  }

}


#' @title Make aggregate pipe-friendly
#' @param x an R object
#' @param ... other parameters passed to \code{\link[stats]{aggregate}}
#' @seealso \code{\link[stats]{aggregate}}
#' @description  A pipe-friendly wrapper of \code{\link[stats]{aggregate}}
#' when using formula as input.
#' @return Results from \code{\link[stats]{aggregate}}
#' @examples
#'
#' library(magrittr)
#' data(ToothGrowth)
#'
#' ToothGrowth %>%
#'   do_aggregate(len ~ ., mean)
#'
#' @export
do_aggregate <- function(x, ...) {
  if(is.data.frame(x)){
    if(is.null(sys.call()[['by']])){
      return(stats::aggregate(data = x, ...))
    }
  }
  return(stats::aggregate(x, ...))
}


#' @title Python-style \code{"for-else"} function
#' @description Provide Python-style \code{"for-else"} that works as
#' follows: for each element, execute "for" block, if there is break
#' while executing "for" block, then just stop and ignore the "else"
#' statement, otherwise run "else" block.
#' @param x iterative R objects such as list, vector, etc.
#' @param FUN function that applies to each \code{x}
#' @param ALT_FUN function that takes no argument or other types of R
#' object
#' @return If any \code{FUN} returns anything other than \code{NULL},
#' then the function returns the first none \code{NULL} object. If
#' all \code{x} fed to \code{FUN} return \code{NULL}, then this
#' function returns \code{ALT_FUN} (if \code{ALT_FUN} is not a function)
#' or the result of \code{ALT_FUN()}.
#' @examples
#'
#' # --------------------------- Basic Usage ------------------------------
#'
#' # 1. ALT_FUN get executed because FUN returns NULL for all items in x
#' forelse(
#'   1:10,
#'   function(x){
#'     cat('The input is ', x, end = '\n')
#'     if( x > 10) return(x) else return(NULL)
#'   },
#'   function(){
#'     cat('ALT_FUN is executed!\n')
#'     'wow'
#'   }
#' )
#'
#' # 2. FUN returns non-NULL object
#' forelse(
#'   1:10,
#'   function(x){
#'     cat('The input is ', x, end = '\n')
#'     if( x %% 2 == 0 ) return(x) else return(NULL)
#'   },
#'   'wow'
#' )
#'
#' # --------------------------- Performance ------------------------------
#' FUN <- function(x){
#'   Sys.sleep(0.01)
#'   if( x %% 2 == 0 ) return(x) else return(NULL)
#' }
#'
#' microbenchmark::microbenchmark({
#'   forelse(1:10, FUN, 'wow')
#' }, {
#'   y <- unlist(lapply(1:10, FUN))
#'   if(length(y)){
#'     y <- y[[1]]
#'   }else{
#'     y <- 'wow'
#'   }
#' }, {
#'   y <- NULL
#'   for(x in 1:10){ y <- FUN(x) }
#'   if(is.null(y)){ y <- 'wow' }
#' }, times = 3)
#'
#'
#' @export
forelse <- function(x, FUN, ALT_FUN = NULL) {
  if(is.function(ALT_FUN)){
    alt <- ALT_FUN
  } else {
    alt <- function(){
      ALT_FUN
    }
  }
  .Call(`_dipsaus_cpp_forelse`, x, FUN, alt)
}

