#' @title Collapse Sensors And Calculate Summations/Mean
#'
#' (stable)
#'
#' @param x A numeric multi-mode tensor (array), without \code{NA}
#' @param keep Which dimension to keep
#' @param average collapse to sum or mean
#'
#' @return a collapsed array with values to be mean or summation along collapsing dimensions
#'
#'
#' @examples
#' # Example 1
#' x = matrix(1:16, 4)
#' collapse(x, keep = 1) # Keep the first dimension and calculate sums along the rest
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
#'   times = 20L
#' )
#'
#' \donttest{
#' \dontrun{
#' # large data big difference
#' x = array(rnorm(prod(300,200,105)), c(300,200,105,1))
#' microbenchmark::microbenchmark(
#'   result = collapse(x, keep = c(3,2)),
#'   compare = apply(x, c(3,2), sum),
#'   times = 10L
#' )
#' }
#' }
#' @export
collapse <- function(x, keep, average = FALSE) {

  if(any(!is.finite(x))){
    x[!is.finite(x)] = 0
  }

  if(any(is.complex(x))){
    re = collapse(Re(x), keep = keep, average = average)
    im = collapse(Im(x), keep = keep, average = average)
    return(re + 1i * im)
  }

  dims = dim(x)
  keep_sorted = sort(keep)

  re = .Call("_dipsaus_collapser", x, dims, keep_sorted)
  dim(re) = dims[keep_sorted]

  if(!isTRUE(all.equal(keep_sorted, keep))){
    re = aperm(re, perm = order(order(keep)))
  }

  if(average){
    re = re / prod(dims[-keep_sorted])
  }

  return(re)
}