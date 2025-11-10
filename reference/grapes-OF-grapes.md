# Get an element with condition that it must be from a list or vector

Get an element with condition that it must be from a list or vector

## Usage

``` r
lhs %OF% rhs
```

## Arguments

- lhs:

  the element of candidate

- rhs:

  the constraint

## Value

Returns an element of length one that will be from `rhs`

## Examples

``` r
# C is from LETTERS, therefore returns `C`
"C" %OF% LETTERS
#> [1] "C"


# `lhs` is not from `rhs`, hence return the first element of LETTERS
'9' %OF% LETTERS
#> [1] "A"
NULL %OF% LETTERS
#> [1] "A"

# When there are multiple elements from `lhs`, select the first that
# matches the constraint
c('9', "D", "V") %OF% LETTERS
#> [1] "D"
```
