# Calculates mean and standard error of mean

Calculates mean and standard error of mean

## Usage

``` r
mean_se(x, na.rm = FALSE, se_na_as_zero = na.rm)
```

## Arguments

- x:

  R numerical object

- na.rm:

  whether to remove `NA`; default is false

- se_na_as_zero:

  see `na_as_zero` in
  [`ste_mean`](https://dipterix.org/dipsaus/reference/ste_mean.md)

## Value

A named vector containing the [`mean`](https://rdrr.io/r/base/mean.html)
and standard error of mean
([`ste_mean`](https://dipterix.org/dipsaus/reference/ste_mean.md)).

## See also

[`ste_mean`](https://dipterix.org/dipsaus/reference/ste_mean.md)

## Examples

``` r
# Mean should be near 0 (mean of standard normal)
# standard error of mean should be near 0.01
mean_se(rnorm(10000))
#>         mean           se 
#> 0.0001562168 0.0100043383 
```
