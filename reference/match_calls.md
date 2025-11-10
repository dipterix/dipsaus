# Recursively match calls and modify arguments

Recursively match calls and modify arguments

## Usage

``` r
match_calls(
  call,
  recursive = TRUE,
  replace_args = list(),
  quoted = FALSE,
  envir = parent.frame(),
  ...
)
```

## Arguments

- call:

  an `R` expression

- recursive:

  logical, recursively match calls, default is true

- replace_args:

  named list of functions, see examples

- quoted:

  logical, is `call` quoted

- envir:

  which environment should call be evaluated

- ...:

  other parameters passing to `match.call`

## Value

A nested call with all arguments matched

## Examples

``` r
library(dipsaus); library(shiny)

# In shiny modules, we might want to add ns() to inputIds
# In this example, textInput(id) will become textInput(ns(id))
match_calls(lapply(1:20, function(i){
  textInput(paste('id_', i), paste('Label ', i))
}), replace_args = list(
  inputId = function(arg, call){ as.call(list(quote(ns), arg)) }
))
#> lapply(X = 1:20, FUN = function(i) {
#>     textInput(inputId = ns(paste("id_", i)), label = paste("Label ", 
#>         i))
#> })
```
