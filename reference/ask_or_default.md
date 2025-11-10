# Read a Line from the Terminal, but with Default Values

Ask a question and read from the terminal in interactive scenario

## Usage

``` r
ask_or_default(..., default = "", end = "", level = "INFO")
```

## Arguments

- ..., end, level:

  passed to [`cat2`](https://dipterix.org/dipsaus/reference/cat2.md)

- default:

  default value to return in case of blank input

## Value

A character from the user's input, or the default value. See details.

## Details

The prompt string will ask a question, providing defaults. Users need to
enter the answer. If the answer is blank (no space), then returns the
default, otherwise returns the user input.

This can only be used in an
[`interactive`](https://rdrr.io/r/base/interactive.html) session.

## See also

[`cat2`](https://dipterix.org/dipsaus/reference/cat2.md),
[`readline`](https://rdrr.io/r/base/readline.html),
[`ask_yesno`](https://dipterix.org/dipsaus/reference/ask_yesno.md)

## Examples

``` r
if(interactive()){
ask_or_default('What is the best programming language?',
               default = 'PHP')
}
```
