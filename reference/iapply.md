# Apply each elements with index as second input

Apply function with an index variable as the second input.

## Usage

``` r
iapply(X, FUN, ..., .method = c("sapply", "lapply", "vapply"))
```

## Arguments

- X:

  a vector (atomic or list)

- FUN:

  the function to be applied to each element of `X`: see \`Details\`.

- ...:

  passed to apply methods

- .method:

  method to use, default is
  [`sapply`](https://rdrr.io/r/base/lapply.html)

## Value

a list or matrix depends on `.method`. See
[`lapply`](https://rdrr.io/r/base/lapply.html)

## Details

`FUN` will be further passed to the apply methods. Unlike
[`lapply`](https://rdrr.io/r/base/lapply.html), `FUN` is expected to
have at least two arguments. The first argument is each element of `X`,
the second argument is the index number of the element.
