# Provides Unique Session ID According to Current R Session

Provides Unique Session ID According to Current R Session

## Usage

``` r
session_uuid(pid = Sys.getpid(), attributes = FALSE)
```

## Arguments

- pid:

  R session process ID, default is
  [`Sys.getpid()`](https://rdrr.io/r/base/Sys.getpid.html)

- attributes:

  whether to append data used to calculate ID as attributes, default is
  false

## Value

Character string
