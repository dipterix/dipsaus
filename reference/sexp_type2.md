# Get Internal Storage Type

Get internal (`C`) data types; See
<https://cran.r-project.org/doc/manuals/r-release/R-ints.pdf> Page 1 for
more different `SEXPTYPE`s.

## Usage

``` r
sexp_type2(x)

# S3 method for class 'sexp_type2'
as.character(x, ...)

# S3 method for class 'sexp_type2'
print(x, ...)
```

## Arguments

- x:

  any R object

- ...:

  ignored

## Value

An integer of class `"sexp_type2"`

## See also

[`storage.mode`](https://rdrr.io/r/base/mode.html)

## Examples

``` r
# 1 vs 1L

# Integer case
sexp_type2(1L)
#> 13 INTSXP     integer vectors 

# double
sexp_type2(1)
#> 14 REALSXP    numeric vectors 

# Built-in function
sexp_type2(`+`)
#> 8 BUILTINSXP  builtin functions 

# normal functions
sexp_type2(sexp_type2)
#> 3 CLOSXP      closures 

# symbols (quoted names)
sexp_type2(quote(`+`))
#> 1 SYMSXP      symbols 

# Calls (quoted expressions)
sexp_type2(quote({`+`}))
#> 6 LANGSXP     language objects 

```
