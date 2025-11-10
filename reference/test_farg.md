# Test whether function has certain arguments

Test whether function has certain arguments

## Usage

``` r
test_farg(fun, arg, dots = TRUE)
```

## Arguments

- fun:

  function

- arg:

  characters of function arguments

- dots:

  whether `fun`'s dots (`...`) counts

## Examples

``` r
a <- function(n = 1){}

# Test whether `a` has argument called 'b'
test_farg(a, 'b')
#> [1] FALSE

# Test whether `a` has argument called 'b' and 'n'
test_farg(a, c('b', 'n'))
#> [1] FALSE  TRUE

# `a` now has dots
a <- function(n = 1, ...){}

# 'b' could goes to dots and a(b=...) is still valid
test_farg(a, 'b')
#> [1] TRUE

# strict match, dots doesn't count
test_farg(a, 'b', dots = FALSE)
#> [1] FALSE
```
