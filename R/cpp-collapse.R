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

  if(any(!is.finite(x))){
    x[!is.finite(x)] <- 0
  }

  if(any(is.complex(x))){
    re <- collapse(Re(x), keep = keep, average = average)
    im <- collapse(Im(x), keep = keep, average = average)
    return(re + 1i * im)
  }

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


#' @export
shift_array <- function(x, time_idx, shift_idx, shift_amount) {
  dims <- dim(x)
  if(is.null(dims)){
    dims <- c(length(x), 1)
  }
  ndims = length(dims)

  time_idx = as.integer(time_idx - 1L)
  shift_idx = as.integer(shift_idx - 1L)
  shift_amount = as.integer(shift_amount)

  stopifnot2(time_idx < ndims && shift_idx < ndims &&
               time_idx >= 0 && shift_idx >= 0,
             msg = "Indices exceed maximum dimension")

  stopifnot2(dims[shift_idx] == length(shift_amount),
             msg = "shift_amount must have equal length to the dimension at shift_idx")

  stopifnot2(is.numeric(x) || is.complex(x), msg = "x must be numeric")

  if(is.complex(x)){
    y = .Call("_dipsaus_arrayShift", Re(x), time_idx, shift_idx, shift_amount, dims)
    x = .Call("_dipsaus_arrayShift", Im(x), time_idx, shift_idx, shift_amount, dims)
    y + 1i * x

  }else{
    .Call("_dipsaus_arrayShift", x, time_idx, shift_idx, shift_amount, as.integer(dims))
  }

}