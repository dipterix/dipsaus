# Get or check elements from dots `'...'`

Get information from `'...'` without evaluating the arguments.

## Usage

``` r
get_dots(..name, ..default = NULL, ...)

missing_dots(envir = parent.frame())
```

## Arguments

- ..name:

  character name of the argument

- ..default:

  R object to return if argument not found

- ...:

  dots that contains argument

- envir:

  R environment

## Value

`missing_dots` returns logical vector with lengths matching with dot
lengths. `get_dots` returns value corresponding to the name.

## Examples

``` r

# ------------------------ Basic Usage ---------------------------

# missing_dots(environment()) is a fixed usage

my_function <- function(...){
  missing_dots(environment())
}
my_function(,)
#> [1] TRUE TRUE

# get_dots
plot2 <- function(...){
  title = get_dots('main', 'There is no title', ...)
  plot(...)
  title
}

plot2(1:10)

#> [1] "There is no title"
plot2(1:10, main = 'Scatter Plot of 1:10')

#> [1] "Scatter Plot of 1:10"

# ------------------------ Comparisons ----------------------------
f1 <- function(...){ get_dots('x', ...) }
f2 <- function(...){ list(...)[['x']] }
delayedAssign('y', { cat('y is evaluated!') })

# y will not evaluate
f1(x = 1, y = y)
#> [1] 1

# y gets evaluated
f2(x = 1, y = y)
#> y is evaluated!
#> [1] 1

# -------------------- Decorator example --------------------------
ret_range <- function(which_range = 'y'){
  function(f){
    function(...){
      f(...)
      y_range <- range(get_dots(which_range, 0, ...))
      y_range
    }
  }
}
plot_ret_yrange <- plot %D% ret_range('y')
plot_ret_yrange(x = 1:10, y = rnorm(10))

#> [1] -1.154247  1.492278

```
