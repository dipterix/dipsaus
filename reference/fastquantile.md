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
#>            {     fastquantile(x, 0.5) }  83.055 118.2805 131.3049 126.8215
#>  {     quantile(x, 0.5, na.rm = TRUE) } 285.642 302.5195 351.7048 316.5255
#>         {     median(x, na.rm = TRUE) } 205.854 219.2885 260.2862 231.0905
#>       uq     max neval
#>  145.486 220.140   100
#>  433.609 479.433   100
#>  251.504 426.886   100
```
