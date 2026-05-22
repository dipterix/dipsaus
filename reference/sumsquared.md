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
#>  expr    min      lq     mean median     uq    max neval
#>   cpp 42.673 42.7435 43.50449 42.904 43.124 58.947   100
#>     r 29.474 30.1300 31.01257 30.455 30.866 43.355   100
```
