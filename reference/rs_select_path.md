# Use 'RStudio' to Select a Path on the Server

Use 'RStudio' to Select a Path on the Server

## Usage

``` r
rs_select_path(is_directory = TRUE)
```

## Arguments

- is_directory:

  whether the path should be a directory

## Value

Raise error if
[`rs_avail`](https://dipterix.org/dipsaus/reference/rs_avail.md) fails,
otherwise returns the selected path
