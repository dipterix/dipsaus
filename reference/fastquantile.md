# Calculate single quantile for numerical values

Slightly faster than [`quantile`](https://rdrr.io/r/stats/quantile.html)
with `na.rm=TRUE`. The internal implementation uses the 'C++' function
`std::nth_element`, which is significantly faster than base R
implementation when the length of input `x` is less than `1e7`.

## Usage

``` r
fastquantile(x, q)
```

## Arguments

- x:

  numerical vector (integers or double)

- q:

  number from 0 to 1

## Value

Identical to `quantile(x, q, na.rm=TRUE)`

## Examples

``` r
# create input x with NAs
x <- rnorm(10000)
x[sample(10000, 10)] <- NA

# compute median
res <- fastquantile(x, 0.5)
res
#> [1] 0.01298609

# base method
res == quantile(x, 0.5, na.rm = TRUE)
#>  50% 
#> TRUE 
res == median(x, na.rm = TRUE)
#> [1] TRUE

# Comparison
microbenchmark::microbenchmark(
  {
    fastquantile(x, 0.5)
  },{
    quantile(x, 0.5, na.rm = TRUE)
  },{
    median(x, na.rm = TRUE)
  }
)
#> Unit: microseconds
#>                                    expr     min       lq     mean   median
#>            {     fastquantile(x, 0.5) }  86.942 128.9160 142.7329 136.7155
#>  {     quantile(x, 0.5, na.rm = TRUE) } 286.625 302.1235 357.3997 315.9390
#>         {     median(x, na.rm = TRUE) } 202.768 215.8680 263.2801 226.3325
#>        uq     max neval
#>  164.1515 198.200   100
#>  451.9830 507.096   100
#>  349.6725 425.494   100
```
