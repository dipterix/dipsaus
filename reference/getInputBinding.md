# Obtain registered input bindings

Obtain registered input bindings

## Usage

``` r
getInputBinding(fname, pkg = NULL, envir = parent.frame())
```

## Arguments

- fname:

  input function name, character or quoted expression such as
  `'shiny::textInput'` or `numericInput`.

- pkg:

  (optional), name of package

- envir:

  environment to evaluate `fname` if `pkg` is not provided

## Value

a list containing: 1. \`JavaScript\` input binding name; 2. \`R\`
updating function name

## Examples

``` r
library(dipsaus)

# Most recommended usage
getInputBinding('compoundInput2', pkg = 'dipsaus')
#> $binding
#> [1] "dipsaus.compoundInput2"
#> 
#> $update_function
#> [1] "dipsaus.updateCompoundInput2"
#> 
#> $call_function
#> [1] "dipsaus::compoundInput2"
#> 

# Other usages
getInputBinding('shiny::textInput')
#> $binding
#> [1] "shiny.textInput"
#> 
#> $update_function
#> [1] "shiny::updateTextInput"
#> 
#> $call_function
#> [1] "shiny::textInput"
#> 


getInputBinding(shiny::textInput)
#> $binding
#> [1] "shiny.textInput"
#> 
#> $update_function
#> [1] "shiny::updateTextInput"
#> 
#> $call_function
#> [1] "shiny::textInput"
#> 

getInputBinding(compoundInput2, pkg = 'dipsaus')
#> $binding
#> [1] "dipsaus.compoundInput2"
#> 
#> $update_function
#> [1] "dipsaus.updateCompoundInput2"
#> 
#> $call_function
#> [1] "dipsaus::compoundInput2"
#> 

# Bad usage, raise errors in some cases
if (FALSE) { # \dontrun{
## You need to library(shiny), or set envir=asNamespace('shiny'), or pkg='shiny'
getInputBinding('textInput')
getInputBinding(textInput) # also fails

## Always fails
getInputBinding('dipsaus::compoundInput2', pkg = 'dipsaus')
} # }
```
