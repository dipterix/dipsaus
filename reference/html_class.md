# Combine, add, or remove 'HTML' classes

Combine 'HTML' classes to produce nice, clean 'HTML' class string via
`combine_html_class`, or to remove a class via `remove_html_class`

## Usage

``` r
combine_html_class(...)

remove_html_class(target, class)
```

## Arguments

- ...:

  one or more characters, classes to combine; duplicated classes will be
  removed

- target:

  characters, class list

- class:

  one or more characters, classes to be removed from `target`

## Value

A character string of new 'HTML' class

## Examples

``` r
# Combine classes "a b c d e"
combine_html_class("a", "b  a", c("c", " d", "b"), list("e ", "a"))
#> [1] "a b c d e"

# Remove class
remove_html_class("a b   c  e", c("b", "c "))
#> [1] "a e"
```
