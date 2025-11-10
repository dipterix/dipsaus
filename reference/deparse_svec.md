# Convert Integer Vectors To String

Convert Integer Vectors To String

## Usage

``` r
deparse_svec(
  nums,
  connect = "-",
  concatenate = TRUE,
  collapse = ",",
  max_lag = 1
)
```

## Arguments

- nums:

  integer vector

- connect:

  character used to connect consecutive numbers

- concatenate:

  connect strings if there are multiples

- collapse:

  if concatenate, character used to connect strings

- max_lag:

  defines "consecutive", min = 1

## Value

strings representing the input vector. For example, `c(1, 2, 3)` returns
"1-3".

## See also

[`parse_svec`](https://dipterix.org/dipsaus/reference/parse_svec.md)

## Examples

``` r
deparse_svec(c(1:10, 15:18))
#> [1] "1-10,15-18"
```
