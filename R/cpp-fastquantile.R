#' Calculate single quantile for numerical values
#' @description Slightly faster than \code{\link[stats]{quantile}} with
#' \code{na.rm=TRUE}. The internal implementation uses the 'C++' function
#' \code{std::nth_element}, which is significantly faster than base R
#' implementation when the length of input \code{x} is less than \code{1e7}.
#' @param x numerical vector (integers or double)
#' @param q number from 0 to 1
#' @return Identical to \code{quantile(x, q, na.rm=TRUE)}
#' @examples
#'
#' # create input x with NAs
#' x <- rnorm(10000)
#' x[sample(10000, 10)] <- NA
#'
#' # compute median
#' res <- fastquantile(x, 0.5)
#' res
#'
#' # base method
#' res == quantile(x, 0.5, na.rm = TRUE)
#' res == median(x, na.rm = TRUE)
#'
#' # Comparison
#' microbenchmark::microbenchmark(
#'   {
#'     fastquantile(x, 0.5)
#'   },{
#'     quantile(x, 0.5, na.rm = TRUE)
#'   },{
#'     median(x, na.rm = TRUE)
#'   }
#' )
#'
#' @export
fastquantile <- function(x, q){
  stopifnot(q >= 0 && q <= 1)
  if(!is.double(x)){
    return(quantile2(as.double(x), q))
  }
  quantile2(x, q)
}

