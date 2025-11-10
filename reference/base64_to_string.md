# Convert "Base64" Data to String

Decode "Base64" data to its generating characters

## Usage

``` r
base64_to_string(what)
```

## Arguments

- what:

  characters, encoded "Base64" data

## Value

String

## Examples

``` r
input <- "The quick brown fox jumps over the lazy dog"

# Base64 encode
what <- base64enc::base64encode(what = charToRaw(input))

# Base64 decode
base64_to_string(what)
#> [1] "The quick brown fox jumps over the lazy dog"
```
