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
#>  expr    min     lq     mean  median      uq    max neval
#>   cpp 42.613 42.714 43.39727 42.8735 43.0535 56.464   100
#>     r 29.243 29.769 30.64515 29.9995 30.4205 40.840   100
```
