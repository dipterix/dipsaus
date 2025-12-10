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
#> [1] 9894.404

# Compare speed
microbenchmark::microbenchmark(
  cpp = {sumsquared(x)},
  r = {sum(x^2)}
)
#> Unit: microseconds
#>  expr    min     lq     mean  median      uq    max neval
#>   cpp 37.820 37.941 38.67255 38.0655 38.3315 50.083   100
#>     r 24.415 24.836 26.02418 25.2770 26.3795 35.997   100
```
