# A Wrapper for `fastmap::fastmap`

[`fastmap`](https://r-lib.github.io/fastmap/reference/fastmap.html)
provides a key-value store where the keys are strings and the values are
any R objects. It differs from normal environment that
[`fastmap`](https://r-lib.github.io/fastmap/reference/fastmap.html)
avoids memory leak. `fastmap2` is a wrapper for `fastmap`, which
provides several generic functions such that it has similar behaviors to
lists or environments

## Usage

``` r
fastmap2(missing_default = NULL)

# S3 method for class 'fastmap2'
x[[name]]

# S3 method for class 'fastmap2'
x$name

# S3 method for class 'fastmap2'
x[[name]] <- value

# S3 method for class 'fastmap2'
x$name <- value

# S3 method for class 'fastmap2'
x[i, j = NULL, ...]

# S3 method for class 'fastmap2'
x[i, j = NULL, ...] <- value

# S3 method for class 'fastmap2'
names(x)

# S3 method for class 'fastmap2'
print(x, ...)

# S3 method for class 'fastmap2'
length(x)

# S3 method for class 'fastmap2'
as.list(x, recursive = FALSE, sorted = FALSE, ...)
```

## Arguments

- missing_default:

  passed to
  [`fastmap::fastmap`](https://r-lib.github.io/fastmap/reference/fastmap.html)

- x:

  a `'fastmap2'` object

- name:

  name, or key of the value

- value:

  any R object

- i, j:

  vector of names

- ...:

  passed to other methods

- recursive:

  whether to recursively apply
  [`as.list`](https://rdrr.io/r/base/list.html)

- sorted:

  whether to sort names; default is false

## Value

A list of `'fastmap2'` instance

## Examples

``` r
## --------------------------- Basic Usage --------------------------
map <- fastmap2()
map$a = 1
map$b = 2
print(map)
#> <Map, size=2, keys=[b, a]>

map[c('a', 'b')]
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
# Alternative way
map['a', 'b']
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 

map[c('c', 'd')] <- 3:4
# or
map['e', 'f'] <- 5:6

# The order is not guaranteed, unless sort=TRUE
as.list(map)
#> $f
#> [1] 6
#> 
#> $e
#> [1] 5
#> 
#> $d
#> [1] 4
#> 
#> $c
#> [1] 3
#> 
#> $b
#> [1] 2
#> 
#> $a
#> [1] 1
#> 
as.list(map, sort=TRUE)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3
#> 
#> $d
#> [1] 4
#> 
#> $e
#> [1] 5
#> 
#> $f
#> [1] 6
#> 

names(map)
#> [1] "f" "e" "d" "c" "b" "a"
length(map)
#> [1] 6

## ----------------------- NULL value handles -----------------------
map$b <- NULL
names(map)   # 'b' still exists!
#> [1] "f" "e" "d" "c" "b" "a"
as.list(map) # 'b' is NULL, but still there
#> $f
#> [1] 6
#> 
#> $e
#> [1] 5
#> 
#> $d
#> [1] 4
#> 
#> $c
#> [1] 3
#> 
#> $b
#> NULL
#> 
#> $a
#> [1] 1
#> 

# to remove 'b', you have to use `@remove` method
map$`@remove`('b')

## ---------------- Native fastmap::fastmap methods -----------------

# whether map has 'a'
map$`@has`('a')
#> [1] TRUE

# Remove a name from list
map$`@remove`('a')

# remove all from list
map$`@reset`()
print(map)
#> <Map, size=0, keys=[]>
```
