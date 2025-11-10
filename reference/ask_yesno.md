# Ask and Return True or False from the Terminal

Ask a question and read from the terminal in interactive scenario

## Usage

``` r
ask_yesno(
  ...,
  end = "",
  level = "INFO",
  error_if_canceled = TRUE,
  use_rs = TRUE,
  ok = "Yes",
  cancel = "No",
  rs_title = "Yes or No:"
)
```

## Arguments

- ..., end, level:

  passed to [`cat2`](https://dipterix.org/dipsaus/reference/cat2.md)

- error_if_canceled:

  raise error if canceled

- use_rs:

  whether to use `rstudioapi` if possible

- ok:

  button label for yes

- cancel:

  button label for no

- rs_title:

  message title if 'RStudio' question box pops up.

## Value

logical or `NULL` or raise an error. If "yes" is entered, returns
`TRUE`; if "no" is entered, returns `FALSE`; if "c" is entered,
`error_if_canceled=TRUE` will result in an error, otherwise return
`NULL`

## Details

The prompt string will ask for an yes or no question. Users need to
enter "y", "yes" for yes, "n", "no" or no, and "c" for cancel
(case-insensitive).

This can only be used in an
[`interactive`](https://rdrr.io/r/base/interactive.html) session.

## See also

[`cat2`](https://dipterix.org/dipsaus/reference/cat2.md),
[`readline`](https://rdrr.io/r/base/readline.html),
[`ask_or_default`](https://dipterix.org/dipsaus/reference/ask_or_default.md)

## Examples

``` r
if(interactive()){
ask_yesno('Do you know how hard it is to submit an R package and ',
          'pass the CRAN checks?')
ask_yesno('Can I pass the CRAN check this time?')
}
```
