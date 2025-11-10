# Standard error of mean

Ported from `'rutabaga'` package, calculates standard error of mean. The
sample size is determined by number of none-`NA` numbers by default

## Usage

``` r
ste_mean(x, na.rm = FALSE, na_as_zero = na.rm, ...)

# Default S3 method
ste_mean(x, na.rm = FALSE, na_as_zero = na.rm, ...)
```

## Arguments

- x:

  R object

- na.rm:

  whether to remove `NA`; default is false

- na_as_zero:

  whether convert `NA` to zero

- ...:

  passed to other methods

## Value

A numerical number that is the standard error of the mean

## See also

[`mean_se`](https://dipterix.org/dipsaus/reference/mean_se.md)

## Examples

``` r
x <- rnorm(100)

ste_mean(x)
#> [1] 0.08731551

# internal implementation
identical(ste_mean(x), sd(x) / sqrt(100))
#> [1] TRUE
```
