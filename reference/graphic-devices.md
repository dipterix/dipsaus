# Create a group of named graphic devices

Create a group of named graphic devices

## Usage

``` r
dev_create(..., env = parent.frame(), attributes = list())

get_dev_attr(which, dev = grDevices::dev.cur(), ifnotfound = NULL)
```

## Arguments

- ...:

  named expressions to launch devices

- env:

  environment to evaluate expressions

- attributes:

  named list; names correspond to device names and values are attributes
  to set to the devices

- which:

  which attribute to obtain

- dev:

  which device to search for attributes

- ifnotfound:

  value to return if attribute is not found

## Value

A list of functions to query, control, and switch between devices

## Examples

``` r
if (FALSE)  ## Unix-specific example

# Create multiple named devices, setting attributes to the second graph
devs <- dev_create(
  line = X11(), points = x11(),
  attributes = list(points = list(pch = 16))
)

# switch to device named "points"

devs$dev_which('points')
#> Error: object 'devs' not found

# Plot points, with pch given as preset
plot(1:10, pch = get_dev_attr(which = 'pch', ifnotfound = 1))


# switch to "line" device
devs$dev_switch('line')
#> Error: object 'devs' not found
plot(1:100, type='l')


# Create another group with conflict name
dev_another <- dev_create(line = X11())
#> Warning: unable to open connection to X11 display ''
#> Error in .External2(C_X11, d$display, d$width, d$height, d$pointsize,     d$gamma, d$colortype, d$maxcubesize, d$bg, d$canvas, d$fonts,     NA_integer_, d$xpos, d$ypos, d$title, type, antialias, d$family,     optionSymbolFont(d$symbolfamily)): unable to start device X11cairo

# Query device name with 'line'
dev_another$dev_which('line')  # 4
#> Error: object 'dev_another' not found
devs$dev_which('line')  # 2, doesn't conflict with the new groups
#> Error: object 'devs' not found

dev.list()
#> agg_record_578689825 
#>                    2 
# close one or more device
dev_another$dev_off('line')
#> Error: object 'dev_another' not found
dev.list()
#> agg_record_578689825 
#>                    2 

# close all devices
devs$dev_off()
#> Error: object 'devs' not found
dev.list()
#> agg_record_578689825 
#>                    2 

 # \dontrun{}
```
