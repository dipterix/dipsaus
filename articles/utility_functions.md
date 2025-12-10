# Utility Functions

``` r
library(dipsaus)
```

#### 1. Cat strings with levels: `DEBUG`, `INFO`, `WARNING`, `ERROR`, and `FATAL`

``` r
cat2('Debug passed!', level = 'DEBUG')
#> ✔ Debug passed!

cat2('You are all set.', level = 'INFO')
#> ♥ You are all set.

cat2('Wait a second...', level = 'WARNING')
#> ⚠ Wait a second...

cat2('Ooops', level = 'ERROR')
#> ✖ Ooops

cat2('Bi--Doop---', level = 'FATAL')
#> ✖ Bi--Doop---
#> Error: 
#> ...
```

*Level `FATAL` will raise errors.*

#### 2. `parse_svec` and `deparse_svec`

`parse_svec` converts characters like `"7-10,14-15"` to numeric vectors

``` r
parse_svec("7-10,14-15")
#> [1]  7  8  9 10 14 15
```

`deparse_svec` reverse the process

``` r
deparse_svec(c(2,5,3,1,7))
#> [1] "1-3,5,7"
```

Parameter `max_lag` in `deparse_svec` changes the threshold of integers
collapsed together:

``` r
deparse_svec(c(1,2,4,7,11))
#> [1] "1-2,4,7,11"

deparse_svec(c(1,2,4,7,11), max_lag = 2)
#> [1] "1-4,7,11"

deparse_svec(c(1,2,4,7,11), max_lag = 3)
#> [1] "1-7,11"
```

#### 3. System Information

The goal is to work as an alternative to retrieve system information.
For example, R doesn’t provide functions to get memory limits, or CPU
chip-set information on Linux or MacOS.

``` r
# Total RAM in bytes
get_ram()
#> 16772579328.0 B

# Print-friendly
to_ram_size(get_ram(), 1024)
#> 15.6 GB

# WARNING: $free is the total RAM - R usage, is no the actual free RAM
mem_limit2()
#> $total
#> 16772579328.0 B
#> $free
#> 16707093328.0 B
```

#### 4. Interactive Questions

`askYesNo` comes with `utils` package allows to ask yes/no questions and
returns logical value. If `cancel` is entered, then the function returns
`NA`, if answers other than `yes`, `no` or `cancel` is given, it raise
an error.
[`dipsaus::ask_yesno`](https://dipterix.org/dipsaus/reference/ask_yesno.md)
will ask the question again and again until the user actually say `yes`
or `no`

``` r
> ask_yesno('Please answer an yes/no question, ok?')
## ♥ Please answer an yes/no question, ok? (Yes/no): 
> qweee
## ⚠ Please answer Y/yes, N/no, or c to cancel. (Yes/no): 
> ttt
## ⚠ Please answer Y/yes, N/no, or c to cancel. (Yes/no): 
> y
## [1] TRUE
```

`ask_or_default` instead of asking a yes/no question, it asks a question
with default answer. User don’t have to type the answers if the they
accept defaults:

``` r
> ask_or_default("What is your password", default = 'I will not tell you!')
## ♥ What is your password
##   [default is ‘I will not tell you!’] 
>
## [1] "I will not tell you!"
```
