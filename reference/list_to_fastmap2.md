# Copy elements to `fastmap2`

Copy elements to `fastmap2`

## Usage

``` r
list_to_fastmap2(li, map = NULL)
```

## Arguments

- li:

  a list or an environment

- map:

  `NULL` or a `fastmap2` instance

## Value

If `map` is not `NULL`, elements will be added to `map` and return
`map`, otherwise create a new instance.
