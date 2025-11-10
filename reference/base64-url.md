# Encode or decode 'base64'

Compatible with results from package `'base64url'`, but implemented with
package `'base64enc'`. I simply do not like it when I have to depend on
two packages that can achieve the same goal. This implementation is
slower. If you have `'base64url'` installed, please use that version.

## Usage

``` r
base64_urlencode(x)

base64_urldecode(x)
```

## Arguments

- x:

  character vector to encode or decode

## Value

character vector of the same length as `x`

## Examples

``` r
x = "plain text"
encoded = base64_urlencode(x)
decoded = base64_urldecode(encoded)
print(encoded)
#> [1] "cGxhaW4gdGV4dA"
print(decoded)
#> [1] "plain text"
```
