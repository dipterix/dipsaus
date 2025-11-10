# Captures Evaluation Output of Expressions as One Single String

Evaluate expression and captures output as characters, then concatenate
as one single string.

## Usage

``` r
capture_expr(expr, collapse = "\n", type = c("output", "message"), ...)
```

## Arguments

- expr:

  R expression

- collapse:

  character to concatenate outputs

- type, ...:

  passed to
  [`capture.output`](https://rdrr.io/r/utils/capture.output.html)

## Value

Character of length 1: output captured by
[`capture.output`](https://rdrr.io/r/utils/capture.output.html)

## Examples

``` r
x <- data.frame(a=1:10)
x_str <- capture_expr({
  print(x)
})

x_str
#> [1] "    a\n1   1\n2   2\n3   3\n4   4\n5   5\n6   6\n7   7\n8   8\n9   9\n10 10"

cat(x_str)
#>     a
#> 1   1
#> 2   2
#> 3   3
#> 4   4
#> 5   5
#> 6   6
#> 7   7
#> 8   8
#> 9   9
#> 10 10

```
