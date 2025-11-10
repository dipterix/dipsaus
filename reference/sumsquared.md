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
#> [1] 10007.41

# Compare speed
microbenchmark::microbenchmark(
  cpp = {sumsquared(x)},
  r = {sum(x^2)}
)
#> Unit: microseconds
#>  expr    min      lq     mean median     uq    max neval
#>   cpp 37.870 38.0510 38.84878 38.161 38.442 52.508   100
#>     r 25.628 26.3595 27.47366 26.740 27.682 60.653   100
```
