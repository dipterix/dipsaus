# Make aggregate pipe-friendly

A pipe-friendly wrapper of
[`aggregate`](https://rdrr.io/r/stats/aggregate.html) when using formula
as input.

## Usage

``` r
do_aggregate(x, ...)
```

## Arguments

- x:

  an R object

- ...:

  other parameters passed to
  [`aggregate`](https://rdrr.io/r/stats/aggregate.html)

## Value

Results from [`aggregate`](https://rdrr.io/r/stats/aggregate.html)

## See also

[`aggregate`](https://rdrr.io/r/stats/aggregate.html)

## Examples

``` r
data(ToothGrowth)

ToothGrowth |>
  do_aggregate(len ~ ., mean)
#>   supp dose   len
#> 1   OJ  0.5 13.23
#> 2   VC  0.5  7.98
#> 3   OJ  1.0 22.70
#> 4   VC  1.0 16.77
#> 5   OJ  2.0 26.06
#> 6   VC  2.0 26.14
```
