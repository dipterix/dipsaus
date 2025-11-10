# Apply function with `rs_exec`

Apply function with `rs_exec`

## Usage

``` r
lapply_callr(
  x,
  fun,
  ...,
  .callback = NULL,
  .globals = list(),
  .ncores = future::availableCores(),
  .packages = attached_packages(),
  .focus_on_console = TRUE,
  .rs = FALSE,
  .quiet = FALSE,
  .name = "",
  .wait = TRUE
)
```

## Arguments

- x:

  vector or list

- fun:

  function

- ...:

  passed to function, see [`lapply`](https://rdrr.io/r/base/lapply.html)

- .callback:

  a function takes zero, one, or two arguments and should return a
  string to show in the progress

- .globals:

  a named list that `fun` relies on

- .ncores:

  number of cores to use; only used when `.wait=TRUE`

- .packages:

  packages to load

- .focus_on_console:

  whether to focus on console once finished; is only used when `.rs` is
  true

- .rs:

  whether to create 'RStudio' jobs; default is false

- .quiet:

  whether to suppress progress message

- .name:

  the name of progress and jobs

- .wait:

  whether to wait for the results; default is true, which blocks the
  main session waiting for results.

## Value

When `.wait=TRUE`, returns a list that should be, in most of the cases,
identical to [`lapply`](https://rdrr.io/r/base/lapply.html); when
`.wait=FALSE`, returns a function that collects results.

## See also

[`rs_exec`](https://dipterix.org/dipsaus/reference/rs_exec.md)

## Examples

``` r
if(interactive()){

  lapply_callr(1:3, function(x, a){
    c(Sys.getpid(), a, x)
  }, a = 1)

  lapply_callr(1:30, function(x)
    {
      Sys.sleep(0.1)
      sprintf("a + x = %d", a + x)
    }, .globals = list(a = 1),
    .callback = I, .name = "Test")

}
```
