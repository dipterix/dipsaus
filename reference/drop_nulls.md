# Drop `NULL` values from list or vectors

Drop `NULL` values from list or vectors

## Usage

``` r
drop_nulls(x, .invalids = list("is.null"))
```

## Arguments

- x:

  list to check

- .invalids:

  a list of functions, or function name. Default is 'is.null'.

## Value

list or vector containing no invalid values

## Examples

``` r
x <- list(NULL,NULL,1,2)
drop_nulls(x)  # length of 2
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
```
