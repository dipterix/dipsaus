# Left-hand side checked assignment Provides a way to assign default values to variables. If the statement \``lhs`\` is invalid or `NULL`, this function will try to assign `value`, otherwise nothing happens.

Left-hand side checked assignment Provides a way to assign default
values to variables. If the statement \``lhs`\` is invalid or `NULL`,
this function will try to assign `value`, otherwise nothing happens.

## Usage

``` r
lhs %?<-% value
```

## Arguments

- lhs:

  an object to check or assign

- value:

  value to be assigned if lhs is NULL

## Value

Assign value on the right-hand side to the left-hand side if `lhs` does
not exist or is `NULL`

## Examples

``` r
# Prepare, remove aaa if exists
if(exists('aaa', envir = globalenv(), inherits = FALSE)){
  rm(aaa, envir = globalenv())
}

# Assign
aaa %?<-% 1; print(aaa)
#> [1] 1

# However, if assigned, nothing happens
aaa = 1;
aaa %?<-% 2;
print(aaa)
#> [1] 1

# in a list
a = list()
a$e %?<-% 1; print(a$e)
#> [1] 1
a$e %?<-% 2; print(a$e)
#> [1] 1
```
