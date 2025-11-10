# Parse Text Into Numeric Vectors

Parse Text Into Numeric Vectors

## Usage

``` r
parse_svec(text, sep = ",", connect = "-:|", sort = FALSE, unique = TRUE)
```

## Arguments

- text:

  string with chunks, e.g. `"1-10, 14, 16-20, 18-30"` has 4 chunks

- sep:

  default is ",", character used to separate chunks

- connect:

  characters defining connection links for example "1:10" is the same as
  "1-10"

- sort:

  sort the result

- unique:

  extract unique elements

## Value

a numeric vector. For example, "1-3" returns `c(1, 2, 3)`

## See also

[`deparse_svec`](https://dipterix.org/dipsaus/reference/deparse_svec.md)

## Examples

``` r
parse_svec('1-10, 13:15,14-20')
#>  [1]  1  2  3  4  5  6  7  8  9 10 13 14 15 16 17 18 19 20
```
