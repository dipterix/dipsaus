# A Wrapper for `fastmap::fastqueue`

A Wrapper for
[`fastmap::fastqueue`](https://r-lib.github.io/fastmap/reference/fastqueue.html)

## Usage

``` r
fastqueue2(init = 20L, missing_default = NULL)

# S3 method for class 'fastqueue2'
x[[i]]

# S3 method for class 'fastqueue2'
x[i, j = NULL, ...]

# S3 method for class 'fastqueue2'
print(x, ...)

# S3 method for class 'fastqueue2'
length(x)

# S3 method for class 'fastqueue2'
as.list(x, ...)
```

## Arguments

- init, missing_default:

  passed to
  [`fastmap::fastqueue`](https://r-lib.github.io/fastmap/reference/fastqueue.html)

- x:

  a `'fastqueue2'` object

- i, j:

  integer index

- ...:

  integer indices or passed to other methods

## Value

A list of `'fastqueue2'` instance

## Examples

``` r
x <- fastqueue2()

# add elements
x$madd(1, "b", function(){ "c" }, 4, "5")

# print information
print(x)
#> <Queue, size=5>

# get the second element without changing the queue
x[[2]]
#> [1] "b"

# remove and get the first element
x$remove()
#> [1] 1

# the second item
x[[2]]
#> function () 
#> {
#>     "c"
#> }
#> <environment: 0x555fe501baa8>

# first two items in a list
x[c(1,2)]
#> [[1]]
#> [1] "b"
#> 
#> [[2]]
#> function () 
#> {
#>     "c"
#> }
#> <environment: 0x555fe501baa8>
#> 

print(x)
#> <Queue, size=4>
as.list(x)
#> [[1]]
#> [1] "b"
#> 
#> [[2]]
#> function () 
#> {
#>     "c"
#> }
#> <environment: 0x555fe501baa8>
#> 
#> [[3]]
#> [1] 4
#> 
#> [[4]]
#> [1] "5"
#> 
```
