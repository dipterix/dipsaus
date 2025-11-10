# Check whether a function, environment comes from a namespace

A coarse way to find if a function comes from a package.

## Usage

``` r
is_from_namespace(x, recursive = TRUE)
```

## Arguments

- x:

  function, environment, language (with environment attached)

- recursive:

  whether to recursively search parent environments

## Value

logical true if `x` or its environment is defined in a namespace;
returns false if the object is atomic, or defined in/from global
environment, or an empty environment.

## Examples

``` r

is_from_namespace(baseenv())        # TRUE
#> [1] TRUE
is_from_namespace(utils::read.csv)  # TRUE
#> [1] TRUE

x <- function(){}
is_from_namespace(NULL)             # FALSE
#> [1] FALSE
is_from_namespace(x)                # FALSE
#> [1] FALSE
is_from_namespace(emptyenv())       # FALSE
#> [1] FALSE

# Let environment of `x` be base environment
# (exception case)
environment(x) <- baseenv()
is_from_namespace(x)        # TRUE
#> [1] TRUE

```
