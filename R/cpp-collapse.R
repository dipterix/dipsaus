#' @title Collapse Sensors And Calculate Summations/Mean
#'
#' (stable)
#'
#' @param x A numeric multi-mode tensor (array), without \code{NA}
#' @param keep Which dimension to keep
#' @param average collapse to sum or mean
#'
#' @return a collapsed array with values to be mean or summation along
#' collapsing dimensions
#'
#'
#' @examples
#' # Example 1
#' x = matrix(1:16, 4)
#'
#' # Keep the first dimension and calculate sums along the rest
#' collapse(x, keep = 1)
#' rowSums(x)  # Should yield the same result
#'
#' # Example 2
#' x = array(1:120, dim = c(2,3,4,5))
#' result = collapse(x, keep = c(3,2))
#' compare = apply(x, c(3,2), sum)
#' sum(abs(result - compare)) # The same, yield 0 or very small number (1e-10)
#'
#' # Example 3 (performance)
#' RcppParallel::setThreadOptions(numThreads = -1) # auto multicores
#' # Small data, no big difference, even slower
#' x = array(rnorm(240), dim = c(4,5,6,2))
#' microbenchmark::microbenchmark(
#'   result = collapse(x, keep = c(3,2)),
#'   compare = apply(x, c(3,2), sum),
#'   times = 1L, check = function(v){
#'     max(abs(range(do.call('-', v)))) < 1e-10
#'   }
#' )
#'
#' # large data big difference
#' x = array(rnorm(prod(300,200,105)), c(300,200,105,1))
#' microbenchmark::microbenchmark(
#'   result = collapse(x, keep = c(3,2)),
#'   compare = apply(x, c(3,2), sum),
#'   times = 1L , check = function(v){
#'     max(abs(range(do.call('-', v)))) < 1e-10
#'   })
#'
#' @export
collapse <- function(x, keep, average = FALSE) {

  if(any(is.complex(x))){
    re <- collapse(Re(x), keep = keep, average = average)
    im <- collapse(Im(x), keep = keep, average = average)
    return(re + 1i * im)
  }

  stopifnot2(is.numeric(x), msg = 'x must be numeric or complex')

  dims <- dim(x)
  keep_sorted <- sort(keep)

  re <- .Call("_dipsaus_collapser", x, dims, keep_sorted)
  dim(re) <- dims[keep_sorted]

  if(!isTRUE(all.equal(keep_sorted, keep))){
    re <- aperm(re, perm = order(order(keep)))
  }

  if(average){
    re <- re / prod(dims[-keep_sorted])
  }

  return(re)
}



#' @title Shift Array by Index
#' @description Re-arrange arrays in parallel
#' @param x array, must have at least matrix
#' @param shift_idx which index is to be shifted
#' @param shift_by which dimension decides \code{shift_amount}
#' @param shift_amount shift amount along \code{shift_idx}
#' @details
#' A simple use-case for this function is to think of a matrix where each row
#' is a signal and columns stand for time. The objective is to align (time-lock)
#' each signal according to certain events. For each signal, we want to shift
#' the time points by certain amount.
#'
#' In this case, the shift amount is defined by \code{shift_amount}, whose
#' length equals to number of signals. \code{shift_idx=2} as we want to shift
#' time points (column, the second dimension) for each signal. \code{shift_by=1}
#' because the shift amount is depend on the signal number.
#'
#' @examples
#' x <- matrix(1:10, nrow = 2, byrow = TRUE)
#' z <- shift_array(x, 2, 1, c(1,2))
#'
#' y <- NA * x
#' y[1,1:4] = x[1,2:5]
#' y[2,1:3] = x[2,3:5]
#'
#' # Check if z ang y are the same
#' z - y
#'
#' # array case
#' # x is Trial x Frequency x Time
#' x <- array(1:27, c(3,3,3))
#'
#' # Shift time for each trial, amount is 1, -1, 0
#' shift_amount <- c(1,-1,0)
#' z <- shift_array(x, 3, 1, shift_amount)
#' par(mfrow = c(3, 2))
#' for( ii in 1:3 ){
#'   image(t(x[ii, ,]), ylab = 'Frequency', xlab = 'Time',
#'         main = paste('Trial', ii))
#'   image(t(z[ii, ,]), ylab = 'Frequency', xlab = 'Time',
#'         main = paste('Shifted amount:', shift_amount[ii]))
#' }
#'
#' @export
shift_array <- function(x, shift_idx, shift_by, shift_amount) {
  dims <- dim(x)
  if(is.null(dims)){
    dims <- c(length(x), 1)
  }
  ndims <- length(dims)

  shift_idx <- as.integer(shift_idx - 1L)
  shift_by <- as.integer(shift_by - 1L)
  shift_amount <- as.integer(shift_amount)

  stopifnot2(shift_idx < ndims && shift_by < ndims &&
               shift_idx >= 0 && shift_by >= 0,
             msg = "Indices exceed maximum dimension")

  stopifnot2(dims[shift_by + 1] == length(shift_amount),
             msg = "shift_amount must have equal length to the dimension at shift_by")

  stopifnot2(is.numeric(x) || is.complex(x), msg = "x must be numeric")

  if(is.complex(x)){
    y <- .Call("_dipsaus_arrayShift", Re(x), shift_idx, shift_by, shift_amount, dims)
    x <- .Call("_dipsaus_arrayShift", Im(x), shift_idx, shift_by, shift_amount, dims)
    y + 1i * x

  }else{
    .Call("_dipsaus_arrayShift", x, shift_idx, shift_by, shift_amount, as.integer(dims))
  }

}
