# Verify 'RStudio' version

Verify 'RStudio' version

## Usage

``` r
rs_avail(version_needed = "1.3", child_ok = FALSE, shiny_ok = FALSE)
```

## Arguments

- version_needed:

  minimum version required

- child_ok:

  check if the current R process is a child process of the main RStudio
  session.

- shiny_ok:

  if set false, then check if 'Shiny' is running, return false if shiny
  reactive domain is not `NULL`

## Value

whether 'RStudio' is running and its version is above the required

## See also

[`isAvailable`](https://rstudio.github.io/rstudioapi/reference/isAvailable.html)
