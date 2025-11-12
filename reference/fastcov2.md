# Calculate Covariance Matrix in Parallel

Speed up covariance calculation for large matrices. The default behavior
is similar [`cov`](https://rdrr.io/r/stats/cor.html). Please remove any
`NA` prior to calculation.

## Usage

``` r
fastcov2(x, y = NULL, col1, col2, df)
```

## Arguments

- x:

  a numeric vector, matrix or data frame; a matrix is highly recommended
  to maximize the performance

- y:

  NULL (default) or a vector, matrix or data frame with compatible
  dimensions to x; the default is equivalent to `y = x`

- col1:

  integers indicating the subset (columns) of `x` to calculate the
  covariance; default is all the columns

- col2:

  integers indicating the subset (columns) of `y` to calculate the
  covariance; default is all the columns

- df:

  a scalar indicating the degrees of freedom; default is `nrow(x)-1`

## Value

A covariance matrix of `x` and `y`. Note that there is no `NA` handling.
Any missing values will lead to `NA` in the resulting covariance
matrices.

## Examples

``` r
x <- matrix(rnorm(400), nrow = 100)

# Call `cov(x)` to compare
fastcov2(x)
#>             [,1]        [,2]        [,3]       [,4]
#> [1,]  1.18978269 -0.20988622  0.08160001 0.04268662
#> [2,] -0.20988622  1.01999925 -0.09347488 0.03700865
#> [3,]  0.08160001 -0.09347488  0.91967892 0.06524996
#> [4,]  0.04268662  0.03700865  0.06524996 0.86823349

# Calculate covariance of subsets
fastcov2(x, col1 = 1, col2 = 1:2)
#>          [,1]       [,2]
#> [1,] 1.189783 -0.2098862

# Speed comparison
x <- matrix(rnorm(100000), nrow = 1000)
microbenchmark::microbenchmark(
  fastcov2 = {
    fastcov2(x, col1 = 1:50, col2 = 51:100)
  },
  cov = {
    cov(x[,1:50], x[,51:100])
  },
  unit = 'ms', times = 10
)
#> Unit: milliseconds
#>      expr      min       lq     mean   median       uq      max neval
#>  fastcov2 1.415498 1.454831 1.600557 1.607385 1.677726 1.891214    10
#>       cov 5.369615 5.382488 5.576789 5.419753 5.455164 7.025910    10

```
