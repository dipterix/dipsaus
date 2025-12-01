# Calculate time difference and return a number

Calculate time difference and return a number

## Usage

``` r
time_delta(t1, t2, units = "secs")
```

## Arguments

- t1:

  time start

- t2:

  time end

- units:

  character, choices are `'secs'`, `'mins'`, `'hours'`, and `'days'`

## Value

numeric difference of time in units specified

## Examples

``` r
a = Sys.time()
Sys.sleep(0.3)
b = Sys.time()

time_delta(a, b) # In seconds, around 0.3
#> [1] 0.3015194
time_delta(a, b, 'mins') # in minutes, around 0.005
#> [1] 0.005025323
```
