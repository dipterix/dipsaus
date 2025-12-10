# A JavaScript style of creating functions

A JavaScript style of creating functions

## Usage

``` r
args %=>% expr
```

## Arguments

- args:

  function arguments: see
  [`formals`](https://rdrr.io/r/base/formals.html)

- expr:

  R expression that forms the body of functions: see
  [`body`](https://rdrr.io/r/base/body.html)

## Value

A function that takes `args` as parameters and `expr` as the function
body

## Examples

``` r
# Formal arguments
c(a) %=>% {
  print(a)
}
#> function (a) 
#> {
#>     print(a)
#> }
#> <environment: 0x55b9dbe11ec8>

# Informal arguments
list(a=) %=>% {
  print(a)
}
#> function (a) 
#> {
#>     print(a)
#> }
#> <environment: 0x55b9dbe11ec8>

# Multiple inputs
c(a, b = 2, ...) %=>% {
  print(c(a, b, ...))
}
#> function (a, b = 2, ...) 
#> {
#>     print(c(a, b, ...))
#> }
#> <environment: 0x55b9dbe11ec8>

# ----- JavaScript style of forEach -----
# ### Equivalent JavaScript Code:
# LETTERS.forEach((el, ii) => {
#   console.log('The index of letter ' + el + ' in "x" is: ' + ii);
# });

iapply(LETTERS, c(el, ii) %=>% {
  cat2('The index of letter ', el, ' in ', sQuote('x'), ' is: ', ii)
}) -> results
#> The index of letter  A  in  ‘x’  is:  1 
#> The index of letter  B  in  ‘x’  is:  2 
#> The index of letter  C  in  ‘x’  is:  3 
#> The index of letter  D  in  ‘x’  is:  4 
#> The index of letter  E  in  ‘x’  is:  5 
#> The index of letter  F  in  ‘x’  is:  6 
#> The index of letter  G  in  ‘x’  is:  7 
#> The index of letter  H  in  ‘x’  is:  8 
#> The index of letter  I  in  ‘x’  is:  9 
#> The index of letter  J  in  ‘x’  is:  10 
#> The index of letter  K  in  ‘x’  is:  11 
#> The index of letter  L  in  ‘x’  is:  12 
#> The index of letter  M  in  ‘x’  is:  13 
#> The index of letter  N  in  ‘x’  is:  14 
#> The index of letter  O  in  ‘x’  is:  15 
#> The index of letter  P  in  ‘x’  is:  16 
#> The index of letter  Q  in  ‘x’  is:  17 
#> The index of letter  R  in  ‘x’  is:  18 
#> The index of letter  S  in  ‘x’  is:  19 
#> The index of letter  T  in  ‘x’  is:  20 
#> The index of letter  U  in  ‘x’  is:  21 
#> The index of letter  V  in  ‘x’  is:  22 
#> The index of letter  W  in  ‘x’  is:  23 
#> The index of letter  X  in  ‘x’  is:  24 
#> The index of letter  Y  in  ‘x’  is:  25 
#> The index of letter  Z  in  ‘x’  is:  26 
```
