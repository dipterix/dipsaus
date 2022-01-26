#' @title Standard error of mean
#' @description Ported from \code{'rutabaga'} package, calculates standard error
#' of mean. The sample size is determined by number of none-\code{NA} numbers
#' by default
#' @param x R object
#' @param na.rm whether to remove \code{NA}; default is false
#' @param ... passed to other methods
#' @seealso \code{\link{mean_se}}
#' @return A numerical number that is the standard error of the mean
#' @examples
#'
#' x <- rnorm(100)
#'
#' ste_mean(x)
#'
#' # internal implementation
#' identical(ste_mean(x), sd(x) / sqrt(100))
#'
#' @export
ste_mean <- function(x, na.rm = FALSE, ...) {
  UseMethod("ste_mean")
}

#' @rdname ste_mean
#' @export
ste_mean.default <- function(x, na.rm = FALSE, ...) {
  n <- sum(!is.na(x))
  if(n < 2){
    return(NA)
  }
  stats::sd(x, na.rm = na.rm) / sqrt(n)
}

#' Calculates mean and standard error of mean
#' @seealso \code{\link{ste_mean}}
#' @param x R numerical object
#' @param na.rm whether to remove \code{NA}; default is false
#' @return A named vector containing the \code{\link{mean}} and standard error
#' of mean (\code{\link{ste_mean}}).
#' @examples
#'
#' # Mean should be near 0 (mean of standard normal)
#' # standard error of mean should be near 0.01
#' mean_se(rnorm(10000))
#'
#' @export
mean_se <- function(x, na.rm = FALSE) {
  c(mean = mean(x, na.rm = na.rm), se = ste_mean(x, na.rm = na.rm))
}

