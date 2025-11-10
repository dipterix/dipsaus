# Evaluate expressions

Evaluate expressions

## Usage

``` r
eval_dirty(expr, env = parent.frame(), data = NULL, quoted = TRUE)
```

## Arguments

- expr:

  R expression or 'rlang' quo

- env:

  environment to evaluate

- data:

  dataframe or list

- quoted:

  Is the expression quoted? By default, this is `TRUE`. This is useful
  when you don't want to use an expression that is stored in a variable;
  see examples

## Value

the executed results of `expr` evaluated with side effects.

## Details

`eval_dirty` uses [`base::eval()`](https://rdrr.io/r/base/eval.html)
function to evaluate expressions. Compare to
[`rlang::eval_tidy`](https://rlang.r-lib.org/reference/eval_tidy.html),
which won't affect original environment, `eval_dirty` causes changes to
the environment. Therefore if `expr` contains assignment, environment
will be changed in this case.

## Examples

``` r
env = new.env(); env$a = 1
rlang::eval_tidy(quote({a <- 111}), env = env)
print(env$a)  # Will be 1. This is because eval_tidy has no side effect
#> [1] 1

eval_dirty(quote({a <- 111}), env)
print(env$a)  # 111, a is changed
#> [1] 111

# Unquoted case
eval_dirty({a <- 222}, env, quoted = FALSE)
print(env$a)
#> [1] 222
```
