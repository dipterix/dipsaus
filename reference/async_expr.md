# Apply R expressions in a parallel way

Apply R expressions in a parallel way

## Usage

``` r
async_expr(
  .X,
  .expr,
  .varname = "x",
  envir = parent.frame(),
  .pre_run = NULL,
  .ncore = future::availableCores(),
  ...
)
```

## Arguments

- .X:

  a vector or a list to apply evaluation on

- .expr:

  R expression, unquoted

- .varname:

  variable name representing element of each `.X`

- envir:

  environment to evaluate expressions

- .pre_run:

  expressions to be evaluated before looping.

- .ncore:

  number of CPU cores

- ...:

  passed to
  [`future::future`](https://future.futureverse.org/reference/future.html)

## Value

a list whose length equals to `.X`. The value of each item returned
depends on whether `async` is called. See details for workflow.

## Details

`async_expr` uses `lapply` and
[`future::future`](https://future.futureverse.org/reference/future.html)
internally. Within each loop, an item in `".X"` will be assigned to
variable `"x"` (defined by `".varname"`) and enter the evaluation.
During the evaluation, function `async` is provided. Expressions within
`async` will be evaluated in another session, otherwise will be
evaluated in current session. Below is the workflow:

- Run `.pre_run`

- For `i` in `seq_along(.X)`:

  - 1\. Assign `x` with `.X[[i]]`, variable name `x` is defined by
    `.varname`

  - 2\. Evaluate `expr` in current session.

    - a\. If `async` is not called, return evaluated `expr`

    - b\. If `async(aync_expr)` is called, evaluate `aync_expr` in
      another session, and return the evaluation results if `aync_expr`
