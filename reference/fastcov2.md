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
#>             [,1]       [,2]        [,3]       [,4]
#> [1,]  1.17251985 -0.1463312  0.08239227  0.1336536
#> [2,] -0.14633119  1.1509356 -0.12009329 -0.0259273
#> [3,]  0.08239227 -0.1200933  0.91439945  0.0385713
#> [4,]  0.13365361 -0.0259273  0.03857130  0.8697899

# Calculate covariance of subsets
fastcov2(x, col1 = 1, col2 = 1:2)
#>         [,1]       [,2]
#> [1,] 1.17252 -0.1463312

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
#>  fastcov2 1.448833 1.493139 1.559657 1.511407 1.625168 1.781064    10
#>       cov 2.600174 2.614745 2.810604 2.632206 2.651033 4.337501    10

```
