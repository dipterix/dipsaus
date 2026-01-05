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
#> [1] -0.001544295

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
#>                                    expr     min       lq     mean  median
#>            {     fastquantile(x, 0.5) }  85.780 149.2130 162.2334 158.977
#>  {     quantile(x, 0.5, na.rm = TRUE) } 247.191 270.4400 328.7885 285.478
#>         {     median(x, na.rm = TRUE) } 168.214 192.1585 254.0171 211.665
#>        uq     max neval
#>  182.0845 211.484   100
#>  418.7520 476.590   100
#>  331.0680 363.679   100
```
