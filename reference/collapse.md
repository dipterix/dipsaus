# Collapse Sensors And Calculate Summations/Mean

Collapse Sensors And Calculate Summations/Mean

## Usage

``` r
collapse(x, keep, average = FALSE)
```

## Arguments

- x:

  A numeric multi-mode tensor (array), without `NA`

- keep:

  Which dimension to keep

- average:

  collapse to sum or mean

## Value

a collapsed array with values to be mean or summation along collapsing
dimensions

## Examples

``` r
# Example 1
x = matrix(1:16, 4)

# Keep the first dimension and calculate sums along the rest
collapse(x, keep = 1)
#> [1] 28 32 36 40
rowSums(x)  # Should yield the same result
#> [1] 28 32 36 40

# Example 2
x = array(1:120, dim = c(2,3,4,5))
result = collapse(x, keep = c(3,2))
compare = apply(x, c(3,2), sum)
sum(abs(result - compare)) # The same, yield 0 or very small number (1e-10)
#> [1] 0

# Example 3 (performance)
# Small data, no big difference, even slower
x = array(rnorm(240), dim = c(4,5,6,2))
microbenchmark::microbenchmark(
  result = collapse(x, keep = c(3,2)),
  compare = apply(x, c(3,2), sum),
  times = 1L, check = function(v){
    max(abs(range(do.call('-', v)))) < 1e-10
  }
)
#> Unit: microseconds
#>     expr      min       lq     mean   median       uq      max neval
#>   result  892.275  892.275  892.275  892.275  892.275  892.275     1
#>  compare 1529.714 1529.714 1529.714 1529.714 1529.714 1529.714     1

# large data big difference
x = array(rnorm(prod(300,200,105)), c(300,200,105,1))
microbenchmark::microbenchmark(
  result = collapse(x, keep = c(3,2)),
  compare = apply(x, c(3,2), sum),
  times = 1L , check = function(v){
    max(abs(range(do.call('-', v)))) < 1e-10
  })
#> Unit: milliseconds
#>     expr       min        lq      mean    median        uq       max neval
#>   result  56.61961  56.61961  56.61961  56.61961  56.61961  56.61961     1
#>  compare 115.21262 115.21262 115.21262 115.21262 115.21262 115.21262     1
```
