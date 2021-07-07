#' @title Calculate Covariance Matrix in Parallel
#'
#' @description Speed up covariance calculation for large matrices. The
#' default behavior is similar \code{\link{cov}}. Please remove any \code{NA}
#' prior to calculation.
#'
#' @param x a numeric vector, matrix or data frame; a matrix is highly
#' recommended to maximize the performance
#' @param y NULL (default) or a vector, matrix or data frame with compatible
#' dimensions to x; the default is equivalent to \code{y = x}
#' @param col1 integers indicating the subset (columns) of \code{x} to
#' calculate the covariance; default is all the columns
#' @param col2 integers indicating the subset (columns) of \code{y} to
#' calculate the covariance; default is all the columns
#' @param df a scalar indicating the degrees of freedom; default is
#' \code{nrow(x)-1}
#'
#' @return A covariance matrix of \code{x} and \code{y}. Note that there is no
#' \code{NA} handling. Any missing values will lead to \code{NA} in the
#' resulting covariance matrices.
#'
#' @examples
#'
#' # Get numbers of threads to 2
#' RcppParallel::setThreadOptions(numThreads = 2)
#'
#' x <- matrix(rnorm(400), nrow = 100)
#'
#' # Call `cov(x)` to compare
#' fastcov2(x)
#'
#' # Calculate covariance of subsets
#' fastcov2(x, col1 = 1, col2 = 1:2)
#'
#' # Speed comparison
#' x <- matrix(rnorm(100000), nrow = 1000)
#' microbenchmark::microbenchmark(
#'   fastcov2 = {
#'     fastcov2(x, col1 = 1:50, col2 = 51:100)
#'   },
#'   cov = {
#'     cov(x[,1:50], x[,51:100])
#'   },
#'   unit = 'ms', times = 10
#' )
#'
#'
#' @export
fastcov2 <- function(x, y = NULL, col1, col2, df){
  if(!is.matrix(x)){
    x <- as.matrix(x)
  }
  if(is.null(y)){
    y <- x
  } else {
    if(!is.matrix(x)){
      y <- as.matrix(y)
    }
  }

  stopifnot(nrow(x) == nrow(y))


  if(missing(col1)){
    col1 <- seq_len(ncol(x))
  } else {
    stopifnot(all(col1 >= 1 & col1 <= ncol(x)))
  }

  if(missing(col2)){
    col2 <- seq_len(ncol(y))
  } else {
    stopifnot(all(col2 >= 1 & col2 <= ncol(y)))
  }
  if(!length(col1) || !length(col2)){
    return(matrix(NA, nrow = length(col1), ncol = length(col2)))
  }

  cm1 <- colMeans(x[, col1, drop = FALSE])
  cm2 <- colMeans(y[, col2, drop = FALSE])

  nobs <- nrow(x)
  if(missing(df)){
    df <- nobs - 1
  }
  fastcov(x, y, nobs, col1, col2, cm1, cm2, df)
}