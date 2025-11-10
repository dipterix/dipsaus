# Apply, but in parallel

Apply, but in parallel

## Usage

``` r
lapply_async2(
  x,
  FUN,
  FUN.args = list(),
  callback = NULL,
  plan = TRUE,
  future.chunk.size = NULL,
  future.seed = sample.int(1, n = 1e+05 - 1),
  ...
)
```

## Arguments

- x:

  vector, list

- FUN:

  function to apply on each element of `x`

- FUN.args:

  more arguments to feed into `FUN`

- callback:

  function to run after each iteration

- plan:

  logical, or character or `future` plan; see Details.

- future.chunk.size, future.seed:

  see also `future_lapply`. If you want the callbacks to be called
  immediately after each loop, then set it to `1`, which is not optimal
  but the only way right now.

- ...:

  passed to [`plan`](https://future.futureverse.org/reference/plan.html)

## Value

same as `with(FUN.args, lapply(x, function(el){eval(body(FUN))}))`

## Details

When `plan` is logical, `FALSE` means use current plan. If `plan=TRUE`,
then it equals to `plan='multicore'`. For characters, `plan` can be
`'multicore'`, `'callr'`, `'sequential'`, `'multisession'`,
`'multiprocess'`, etc. Alternatively, you could pass future
[`plan`](https://future.futureverse.org/reference/plan.html) objects.

## See also

[`make_forked_clusters`](https://dipterix.org/dipsaus/reference/make_forked_clusters.md)

## Examples

``` r
library(future)
plan(sequential)

# Use sequential plan
# 1. Change `plan` to 'multicore', 'multisession', or TRUE to enable
# multi-core, but still with progress information
# 2. Change plan=FALSE will use current future plan
res <- lapply_async2(100:200, function(x){
  return(x+1)
}, callback = function(e){
  sprintf('Input=%d', e)
}, plan = 'sequential')

# Disable callback message, then the function reduce to
# normal `future.apply::future_lapply`
res <- lapply_async2(100:200, function(x){
  return(x+1)
}, callback = NULL, plan = FALSE)

if(interactive()) {

  # PID are different, meaning executing in different sessions
  lapply_async2(1:4, function(x){
    Sys.getpid()
  })
}
```
