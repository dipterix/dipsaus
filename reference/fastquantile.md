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
#>            {     fastquantile(x, 0.5) }  89.267 125.6395 138.2661 137.9675
#>  {     quantile(x, 0.5, na.rm = TRUE) } 281.706 299.2430 364.0372 313.3150
#>         {     median(x, na.rm = TRUE) } 202.618 219.3750 272.9893 230.8510
#>        uq     max neval
#>  155.0445 177.812   100
#>  461.8320 523.227   100
#>  355.9145 409.284   100
```
