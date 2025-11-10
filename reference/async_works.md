# Run jobs in other R sessions without waiting

This function has been deprecated. Please use
[`lapply_callr`](https://dipterix.org/dipsaus/reference/lapply_callr.md)
instead.

## Usage

``` r
async_works(
  X,
  FUN,
  ...,
  .globals = NULL,
  .name = "Untitled",
  .rs = FALSE,
  .wait = TRUE,
  .chunk_size = Inf,
  .nworkers = future::availableCores(),
  .simplify = FALSE,
  .quiet = FALSE,
  .log
)
```

## Arguments

- X:

  vector or list to be applied

- FUN:

  function with the first argument to be each element of `X`

- ...:

  further arguments to be passed to `FUN`

- .globals:

  global variables to be evaluated in `FUN`

- .name:

  job names, used if backed by `rstudioapi` jobs

- .rs:

  whether to use `rstudioapi` jobs

- .wait:

  whether to wait for the results

- .chunk_size:

  used only when `.wait=FALSE`, chunk size for each workers at a time.
  Only useful for printing progress messages, but might slow down the
  process when `.chunk_size` is too small

- .nworkers:

  number of workers at a time

- .simplify:

  whether to simplify the results, i.e. merge list of results to vectors
  or arrays

- .quiet:

  whether to suppress the printing messages

- .log:

  internally used

## Value

If `.wait=TRUE`, returns the applied results of `FUN` on each of `X`.
The result types depend on `.simplify` (compare the difference between
[`lapply`](https://rdrr.io/r/base/lapply.html) and
[`sapply`](https://rdrr.io/r/base/lapply.html)). If `.wait=FALSE`, then
returns a function that can check the result. The function takes
`timeout` argument that blocks the session at most `timeout` seconds
waiting for the results. See examples.

## Details

Unlike `future` package, where the global variables can be automatically
detected, `async_works` require users to specify global variables
explicitly via `.globals`

`async_works` is almost surely slower than `future.apply` packages.
However, it provides a functionality that `future.apply` can hardly
achieve: being non-block. When setting `.wait=FALSE`, the process will
run in the background, and one may run as many of these tasks as they
want. This is especially useful when large data generating process
occurs ( such as read in from a file, process, generate summarizing
reports).

## Examples

``` r
if (FALSE) { # \dontrun{
# requires a sub-process to run the code

# Basic usage
a <- 1
async_works(1:10, function(ii){
  ii + a # sub-process don't know a, hence must pass a as globals
}, .globals = list(a = a))

# non-blocking case
system.time({
  check <- async_works(1:10, function(ii){
    # simulating process, run run run
    Sys.sleep(ii)
    Sys.getpid()
  }, .wait = FALSE)
})

# check the results
res <- check(timeout = 0.1)
attr(res, 'resolved') # whether it's resolved

# block the session waiting for the results
res <- check(timeout = Inf)
attr(res, 'resolved')


} # }
```
