# Python-style `"for-else"` function

Provide Python-style `"for-else"` that works as follows: for each
element, execute "for" block, if there is break while executing "for"
block, then just stop and ignore the "else" statement, otherwise run
"else" block.

## Usage

``` r
forelse(x, FUN, ALT_FUN = NULL)
```

## Arguments

- x:

  iterative R objects such as list, vector, etc.

- FUN:

  function that applies to each `x`

- ALT_FUN:

  function that takes no argument or other types of R object

## Value

If any `FUN` returns anything other than `NULL`, then the function
returns the first none `NULL` object. If all `x` fed to `FUN` return
`NULL`, then this function returns `ALT_FUN` (if `ALT_FUN` is not a
function) or the result of `ALT_FUN()`.

## Examples

``` r
# --------------------------- Basic Usage ------------------------------

# 1. ALT_FUN get executed because FUN returns NULL for all items in x
forelse(
  1:10,
  function(x){
    cat('The input is ', x, end = '\n')
    if( x > 10) return(x) else return(NULL)
  },
  function(){
    cat('ALT_FUN is executed!\n')
    'wow'
  }
)
#> The input is  1 
#> The input is  2 
#> The input is  3 
#> The input is  4 
#> The input is  5 
#> The input is  6 
#> The input is  7 
#> The input is  8 
#> The input is  9 
#> The input is  10 
#> ALT_FUN is executed!
#> [1] "wow"

# 2. FUN returns non-NULL object
forelse(
  1:10,
  function(x){
    cat('The input is ', x, end = '\n')
    if( x %% 2 == 0 ) return(x) else return(NULL)
  },
  'wow'
)
#> The input is  1 
#> The input is  2 
#> [1] 2

# --------------------------- Performance ------------------------------
FUN <- function(x){
  Sys.sleep(0.01)
  if( x %% 2 == 0 ) return(x) else return(NULL)
}

microbenchmark::microbenchmark({
  forelse(1:10, FUN, 'wow')
}, {
  y <- unlist(lapply(1:10, FUN))
  if(length(y)){
    y <- y[[1]]
  }else{
    y <- 'wow'
  }
}, {
  y <- NULL
  for(x in 1:10){ y <- FUN(x) }
  if(is.null(y)){ y <- 'wow' }
}, times = 3)
#> Unit: milliseconds
#>                                                                                                                       expr
#>                                                                                          {     forelse(1:10, FUN, "wow") }
#>  {     y <- unlist(lapply(1:10, FUN))     if (length(y)) {         y <- y[[1]]     }     else {         y <- "wow"     } }
#>           {     y <- NULL     for (x in 1:10) {         y <- FUN(x)     }     if (is.null(y)) {         y <- "wow"     } }
#>        min        lq      mean    median        uq       max neval
#>   20.20794  20.24039  20.25293  20.27284  20.27542  20.27801     3
#>  100.83893 101.00002 101.05392 101.16110 101.16142 101.16174     3
#>  100.77586 100.90815 100.97959 101.04045 101.08146 101.12247     3

```
