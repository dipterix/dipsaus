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
#>                                    expr     min       lq     mean  median
#>            {     fastquantile(x, 0.5) }  58.517 126.6780 142.3841 142.306
#>  {     quantile(x, 0.5, na.rm = TRUE) } 187.829 204.3030 280.3861 224.038
#>         {     median(x, na.rm = TRUE) } 128.010 137.4335 199.5018 148.040
#>       uq     max neval
#>  167.939 200.898   100
#>  381.311 509.927   100
#>  301.643 520.822   100
```
