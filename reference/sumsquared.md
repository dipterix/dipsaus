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
#>  expr    min     lq     mean median      uq    max neval
#>   cpp 37.831 37.951 38.91125 38.122 38.4565 52.698   100
#>     r 24.475 24.872 25.68211 25.202 26.2090 34.193   100
```
