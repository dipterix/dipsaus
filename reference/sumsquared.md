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
#>   cpp 26.512 26.559 26.91876 26.6620 26.7210 34.317   100
#>     r 15.418 16.864 17.90971 17.5245 18.3385 28.557   100
```
