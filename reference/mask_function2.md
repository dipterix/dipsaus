# Mask a function with given variables

Modifies the default behavior of the function by adding one environment
layer on top of input function. The masked variables are assigned
directly to the environment.

## Usage

``` r
mask_function2(f, ..., .list = list())
```

## Arguments

- f:

  any function

- ..., .list:

  name-value pairs to mask the function

## Value

a masked function

## Examples

``` r
a <- 123
f1 <- function(){
  a + 1
}
f1()   # 124
#> [1] 124

f2 <- mask_function2(f1, a = 1)
f2()   # a is masked with value 1, return 2
#> [1] 2

environment(f1)  # global env
#> <environment: 0x559a99e0b6d0>
environment(f2)  # masked env
#> <environment: 0x559a997422f0>

env <- environment(f2)
identical(parent.env(env), environment(f1))  # true
#> [1] TRUE
env$a  # masked variables: a=1
#> [1] 1
```
