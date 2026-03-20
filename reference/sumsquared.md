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
#> [1] 9876.546

# Compare speed
microbenchmark::microbenchmark(
  cpp = {sumsquared(x)},
  r = {sum(x^2)}
)
#> Unit: microseconds
#>  expr    min      lq     mean  median      uq    max neval
#>   cpp 37.840 37.9205 38.68206 38.1310 38.2970 50.845   100
#>     r 25.337 27.6715 28.44008 28.1575 28.8535 44.694   100
```
