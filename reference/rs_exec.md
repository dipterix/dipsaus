# Schedule a Background Job

Utilizes 'RStudio' job scheduler if correct environment is detected,
otherwise call system command via `Rscript`

## Usage

``` r
rs_exec(
  expr,
  name = "Untitled",
  quoted = FALSE,
  rs = TRUE,
  as_promise = FALSE,
  wait = FALSE,
  packages = NULL,
  focus_on_console = FALSE,
  ...,
  nested_ok = FALSE
)
```

## Arguments

- expr:

  R expression

- name:

  used by 'RStudio' as name of the job

- quoted:

  is `expr` quoted

- rs:

  whether to use 'RStudio' by default

- as_promise:

  whether to return as a
  [`promise`](https://rstudio.github.io/promises/reference/promise.html)
  object; default is no

- wait:

  whether to wait for the result.

- packages:

  packages to load in the sub-sessions

- focus_on_console:

  whether to return back to console after creating jobs; useful when
  users want to focus on writing code; default is false. This feature
  works with 'RStudio' (`>=1.4`)

- ...:

  internally used

- nested_ok:

  whether nested `rs_exec` is allowed; default is false; Set to true to
  allow nested parallel code, but use at your own risk.

## Value

If `wait=TRUE`, returns evaluation results of `expr`, otherwise a
function that can track the state of job.

## Details

'RStudio' provides interfaces
[`jobRunScript`](https://rstudio.github.io/rstudioapi/reference/jobRunScript.html)
to schedule background jobs. However, this functionality only applies
using 'RStudio' IDE. When launching R from other places such as
terminals, the job scheduler usually result in errors. In this case, the
alternative is to call system command via `Rscript`

The expression `expr` will run a clean environment. Therefore R objects
created outside of the context will be inaccessible from within the
child environment, and packages except for base packages will not be
loaded.

There is a small difference when running within and without 'RStudio'.
When running via `Rscript`, the environment will run under `vanilla`
argument, which means no load, no start-up code. If you have start-up
code stored at `~/.Rprofile`, the start-up code will be ignored. When
running within 'RStudio', the start-up code will be executed. As of
`rstudioapi` version 0.11, there is no 'vanilla' option. This feature is
subject to change in the future.

## Examples

``` r
if(interactive()){
  h <- rs_exec(
    {
      Sys.sleep(2)
      print(Sys.getpid())
    },
    wait = FALSE, name = 'Test',
    focus_on_console = TRUE
  )
  code <- h()
  print(code)

  # wait 3 seconds
  Sys.sleep(3)
  code <- h()
  attributes(code)
}
```
