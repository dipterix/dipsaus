# Convert color to Hex string

Convert color to Hex string

## Usage

``` r
col2hexStr(col, alpha = NULL, prefix = "#", ...)
```

## Arguments

- col:

  character or integer indicating color

- alpha:

  `NULL` or numeric, transparency. See
  [`grDevices::rgb`](https://rdrr.io/r/grDevices/rgb.html)

- prefix:

  character, default is `"#"`

- ...:

  passing to
  [`adjustcolor`](https://rdrr.io/r/grDevices/adjustcolor.html)

## Value

characters containing the hex value of each color. See details

## Details

`col2hexStr` converts colors such as 1, 2, 3, "red", "blue", ... into
hex strings that can be easily recognized by \`HTML\`, \`CSS\` and
\`JavaScript\`. Internally this function uses
[`adjustcolor`](https://rdrr.io/r/grDevices/adjustcolor.html) with two
differences:

1.  the returned hex string does not contain alpha value if `alpha` is
    `NULL`;

2.  the leading prefix "#" can be customized

## See also

[`adjustcolor`](https://rdrr.io/r/grDevices/adjustcolor.html)

## Examples

``` r
col2hexStr(1, prefix = '0x')      # "0x000000"
#> [1] "0x000000"
col2hexStr('blue')                # "#0000FF"
#> [1] "#0000FF"

# Change default palette, see "grDevices::colors()"
grDevices::palette(c('orange3', 'skyblue1'))
col2hexStr(1)                     # Instead of #000000, #CD8500
#> [1] "#CD8500"
```
