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
#> [1,]  1.19327738 -0.21555903  0.05763812 0.04147075
#> [2,] -0.21555903  1.02903958 -0.06026528 0.03834091
#> [3,]  0.05763812 -0.06026528  0.89162207 0.05189107
#> [4,]  0.04147075  0.03834091  0.05189107 0.86620947

# Calculate covariance of subsets
fastcov2(x, col1 = 1, col2 = 1:2)
#>          [,1]      [,2]
#> [1,] 1.193277 -0.215559

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
#>  fastcov2 1.468978 1.495309 1.753184 1.539242 1.931986 2.505724    10
#>       cov 2.592879 2.624458 2.650203 2.636081 2.643233 2.842467    10

```
