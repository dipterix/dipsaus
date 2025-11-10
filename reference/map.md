# Create R object map.

Provides five types of map that fit in different use cases.

## Usage

``` r
session_map(map = fastmap::fastmap())

rds_map(path = tempfile())

text_map(path = tempfile())
```

## Arguments

- map:

  a
  [`fastmap::fastmap()`](https://r-lib.github.io/fastmap/reference/fastmap.html)
  list

- path:

  directory path where map data should be stored

## Value

An `R6` instance that inherits
[`AbstractMap`](https://dipterix.org/dipsaus/reference/AbstractMap.md)

## Details

There are five types of map implemented. They all inherit class
[`AbstractMap`](https://dipterix.org/dipsaus/reference/AbstractMap.md).
There are several differences in use case scenarios and they backend
implementations.

- `session_map`:

  A session map takes a
  [`fastmap`](https://r-lib.github.io/fastmap/reference/fastmap.html)
  object. All objects are stored in current R session. This means you
  cannot access the map from other process nor parent process. The goal
  of this map is to share the data across different environments and to
  store global variables, as long as they share the same map object. If
  you are looking for maps that can be shared by different processes,
  check the rest map types. The closest map type is `rds_map`.

- `rds_map`:

  An 'RDS' map uses file system to store values. The values are stored
  separately in '.rds' files. Compared to session maps, 'RDS' map can be
  shared across different R process. It's recommended to store large
  files in `rds_map`. If the value is not large in RAM, `text_map` is
  recommended.

- `text_map`:

  A 'text' map uses file system to store values. Similar to `rds_map`,
  it can be stored across multiple processes as long as the maps share
  the same file directory. However, unlike `rds_map`, `text_map` the
  `text_map` can only store basic data values, namely atom data types.
  The supported types are: numeric, character, vector, list, matrix It's
  highly recommended to convert factors to characters. Do NOT use if the
  values are functions or environments. The recommended use case
  scenario is when the speed is not the major concern, and you want to
  preserve data with backward compatibility. Otherwise it's highly
  recommended to use `rds_map`.

## Examples

``` r
# ----------------------Basic Usage ----------------------

# Define a path to your map.
path = tempfile()
map <- rds_map(path)

# Reset
map$reset()

# Check if the map is corrupted.
map$validate()

# You have not set any key-value pairs yet.
# Let's say two parallel processes (A and B) are sharing this map.
# Process A set values
map$keys()
#> NULL

# Start push
# set a normal message
map$set(key = 'a', value = 1)

# set a large object
map$set(key = 'b', value = rnorm(100000))

# set an object with hash of another object
map$set(key = 'c', value = 2, signature = list(
  parameter1 = 123,
  parameter2 = 124
))

# Check what's in the map from process B
mapB <- rds_map(path)
mapB$keys()
#> ==YQ ==Yg ==Yw 
#>  "a"  "b"  "c" 
mapB$keys(include_signatures = TRUE)
#>      keys                                   
#> ==YQ "a"  "6717f2823d3202449301145073ab8719"
#> ==Yg "b"  "8676c5e7c471e149da6e2968f0d3b674"
#> ==Yw "c"  "f3b07dfebb13e24c3b1a5e11ec21a15d"

# Number of key-values pairs in the map.
mapB$size()
#> [1] 3

# Check if key exists
mapB$has(c('1','a', 'c'))
#>     1     a     c 
#> FALSE  TRUE  TRUE 

# Check if key exists and signature also matches
mapB$has('c', signature = list(
  parameter1 = 123,
  parameter2 = 124
))
#>    c 
#> TRUE 

# Signature changed, then return FALSE. This is especially useful when
# value is really large and reading the value takes tons of time
mapB$has('c', signature = list(
  parameter1 = 1244444,
  parameter2 = 124
))
#>     c 
#> FALSE 

# Destroy the map's files altogether.
mapB$destroy()

if (FALSE) { # \dontrun{
  # Once destroyed, validate will raise error
  mapB$validate()
} # }

```
