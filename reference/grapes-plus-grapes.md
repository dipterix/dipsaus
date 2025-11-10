# Plus-minus operator

Plus-minus operator

## Usage

``` r
a %+-% b
```

## Arguments

- a, b:

  numeric vectors, matrices or arrays

## Value

`a +/- b`, the dimension depends on `a+b`. If `a+b` is a scalar, returns
a vector of two; in the case of vector, returns a matrix; all other
cases will return an array with the last dimension equal to 2.

## Examples

``` r
# scalar
1 %+-% 2   # -1, 3
#> [1]  3 -1

# vector input
c(1,2,3) %+-% 2   # matrix
#>      [,1] [,2]
#> [1,]    3   -1
#> [2,]    4    0
#> [3,]    5    1

# matrix input
matrix(1:9, 3) %+-% 2   # 3x3x2 array
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    3    6    9
#> [2,]    4    7   10
#> [3,]    5    8   11
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]   -1    2    5
#> [2,]    0    3    6
#> [3,]    1    4    7
#> 
```
