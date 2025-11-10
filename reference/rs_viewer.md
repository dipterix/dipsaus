# Get 'RStudio' Viewer, or Return Default

Get 'RStudio' Viewer, or Return Default

## Usage

``` r
rs_viewer(
  ...,
  default = TRUE,
  version_needed = "1.3",
  child_ok = FALSE,
  shiny_ok = FALSE
)
```

## Arguments

- ...:

  passed to
  [`viewer`](https://rstudio.github.io/rstudioapi/reference/viewer.html)

- default:

  if [`rs_avail`](https://dipterix.org/dipsaus/reference/rs_avail.md)
  fails, the value to return. Default is `TRUE`

- version_needed, child_ok, shiny_ok:

  passed to
  [`rs_avail`](https://dipterix.org/dipsaus/reference/rs_avail.md)

## Value

If
[`viewer`](https://rstudio.github.io/rstudioapi/reference/viewer.html)
can be called and 'RStudio' is running, then launch 'RStudio' internal
viewer. Otherwise if `default` is a function such as
[`browseURL`](https://rdrr.io/r/utils/browseURL.html), then call the
function with given arguments. If `default` is not a function, return
`default`
