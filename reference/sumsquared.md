# Fast Calculation of Sum-squared for Large Matrices/Vectors

Calculate `sum(x^2)`, but faster when the number of elements exceeds
1000.

## Arguments

- x:

  double, integer, or logical vector/matrix

## Value

A numerical scalar

## Examples

``` r
x <- rnorm(10000)
sumsquared(x)
#> [1] 9893.091

# Compare speed
microbenchmark::microbenchmark(
  cpp = {sumsquared(x)},
  r = {sum(x^2)}
)
#> Unit: microseconds
#>  expr    min      lq     mean  median      uq    max neval
#>   cpp 37.830 37.9210 38.81193 38.1060 38.3965 52.418   100
#>     r 24.536 24.8715 25.66799 25.2475 25.6730 41.488   100
```
