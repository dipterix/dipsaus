# Color Output

Color Output

## Usage

``` r
cat2(
  ...,
  level = "DEBUG",
  print_level = FALSE,
  file = "",
  sep = " ",
  fill = FALSE,
  labels = NULL,
  append = FALSE,
  end = "\n",
  pal = list(DEBUG = "grey60", INFO = "#1d9f34", WARNING = "#ec942c", ERROR = "#f02c2c",
    FATAL = "#763053", DEFAULT = "grey60"),
  use_cli = TRUE,
  bullet = "auto"
)
```

## Arguments

- ...:

  to be printed

- level:

  'DEBUG', 'INFO', 'WARNING', 'ERROR', or 'FATAL' (total 5 levels)

- print_level:

  if true, prepend levels before messages

- file, sep, fill, labels, append:

  pass to [`base::cat`](https://rdrr.io/r/base/cat.html)

- end:

  character to append to the string

- pal:

  a named list defining colors see details

- use_cli:

  logical, whether to use package 'cli'

- bullet:

  character, if use 'cli', which symbol to show. see
  [`symbol`](https://cli.r-lib.org/reference/symbol.html)

## Value

none.

## Details

There are five levels of colors by default: 'DEBUG', 'INFO', 'WARNING',
'ERROR', or FATAL. Default colors are: 'DEBUG' (`grey60`), 'INFO'
(`#1d9f34`), 'WARNING' (`#ec942c`), 'ERROR' (`#f02c2c`), 'FATAL'
(`#763053`) and 'DEFAULT' (`#000000`, black). If level is not in preset
five levels, the color will be "default"-black color.
