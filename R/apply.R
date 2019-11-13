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



