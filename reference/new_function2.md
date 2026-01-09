# Create new function that supports 'quasi-quosure' syntax

Create new function that supports 'quasi-quosure' syntax

## Usage

``` r
new_function2(
  args = alist(),
  body = {
 },
  env = parent.frame(),
  quote_type = c("unquoted", "quote", "quo"),
  quasi_env = parent.frame()
)
```

## Arguments

- args:

  named list of function formals

- body:

  function body expression, supports 'quasi-quosure' syntax

- env:

  declare environment of the function

- quote_type:

  character, whether `body` is unquoted, quoted, or a 'quo' object (from
  'rlang' package)

- quasi_env:

  where the 'quasi-quosure' should be evaluated, default is parent
  environment

## Value

a function

## Details

An unquoted body expression will be quoted, all the expressions with
'quasi-quosure' like `!!var` will be evaluated and substituted with the
value of `var`. For a 'quosure',
[`quo_squash`](https://rlang.r-lib.org/reference/quo_squash.html) will
be applied. A quoted expression will not be substitute, but will be
expanded if any 'quasi-quosure' detected

`args` must be a `list` object, see
[`formals`](https://rdrr.io/r/base/formals.html). For arguments with no
default values, or quoted defaults, use
[`alist`](https://rdrr.io/r/base/list.html). An `arg=alist(a=)` will
result in a function like `function(a){...}`. See examples for more
details.

## See also

[`new_function`](https://rlang.r-lib.org/reference/new_function.html)

## Examples

``` r
# ------------ standard usage ------------
x <- 1:10
f1 <- new_function2(alist(a=), { print(a + x) }, env = environment())
f1(0)
#>  [1]  1  2  3  4  5  6  7  8  9 10

x <- 20:23
f1(0)  # result changed as x changed
#> [1] 20 21 22 23

# ------------ 'quasi-quosure' syntax ------------
x <- 1:10
f2 <- new_function2(alist(a=), { print(a + !!x) })
print(f2)
#> function (a) 
#> {
#>     print(a + 1:10)
#> }
#> <environment: 0x560135e759e0>

f2(0)
#>  [1]  1  2  3  4  5  6  7  8  9 10

x <- 20:23
f2(0)  # result doesn't change as f2 doesn't depend on x anymore
#>  [1]  1  2  3  4  5  6  7  8  9 10

# ------------ argument settings ------------

default <- 123

# default with values pre-specified
new_function2(list(a = default))   # function (a = 123){}
#> function (a = 123) 
#> {
#> }
#> <environment: 0x560135a7ba10>

# default with values unevaluated
new_function2(list(a = quote(default)))   # function (a = default){}
#> function (a = default) 
#> {
#> }
#> <environment: 0x5601359e5c90>
new_function2(alist(a = default))
#> function (a = default) 
#> {
#> }
#> <environment: 0x5601357dfc50>

# missing default
new_function2(alist(a = ))    # function (a){}
#> function (a) 
#> {
#> }
#> <environment: 0x560135622ce0>

```
