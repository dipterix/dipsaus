# Right-hand side checked assignment Provides a way to avoid assignment to the left-hand side. If the statement \``value`\` is invalid or `NULL`, this function will not assign values and nothing happens.

Right-hand side checked assignment Provides a way to avoid assignment to
the left-hand side. If the statement \``value`\` is invalid or `NULL`,
this function will not assign values and nothing happens.

## Usage

``` r
lhs %<-?% value
```

## Arguments

- lhs:

  an object to be assigned to

- value:

  value to be checked

## Value

Assign value on the right-hand side to the left-hand side if `value`
does exists and is not `NULL`

## Examples

``` r
# Prepare, remove aaa if exists
if(exists('aaa', envir = globalenv(), inherits = FALSE)){
  rm(aaa, envir = globalenv())
}

# aaa will not be assigned. run `print(aaa)` will raise error
aaa %<-?% NULL

# Assign
aaa %<-?% 1
print(aaa)
#> [1] 1

# in a list
a = list()
a$e %<-?% bbb; print(a$e)
#> NULL
a$e %<-?% 2; print(a$e)
#> [1] 2
```
