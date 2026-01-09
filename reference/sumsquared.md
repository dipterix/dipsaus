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
#>   cpp 37.921 38.0910 38.92248 38.2265 38.3865 59.541   100
#>     r 26.600 27.4315 28.35141 27.8970 28.4380 40.195   100
```
