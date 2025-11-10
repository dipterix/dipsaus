# Function to clear all elements within environment

Function to clear all elements within environment

## Usage

``` r
clear_env(env, ...)
```

## Arguments

- env:

  environment to clean, can be an R environment, or a
  [`fastmap2`](https://dipterix.org/dipsaus/reference/fastmap2.md)
  instance

- ...:

  ignored

## Examples

``` r
env = new.env()
env$a = 1
print(as.list(env))
#> $a
#> [1] 1
#> 

clear_env(env)
print(as.list(env))
#> list()
```
