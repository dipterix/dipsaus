# Get Memory Size

Get Memory Size

## Usage

``` r
get_ram()
```

## Value

System RAM in bytes, or `NA` if not supported.

## Details

The function `get_ram` only supports 'MacOS', 'Windows', and 'Linux'.
'Solaris' or other platforms will return `NA`. Here are the system
commands used to detect memory limits:

- 'Windows':

  Uses command `'wmic.exe'` in the 'Windows' system folder. Notice this
  command-line tool might not exist on all 'Windows' machines. `get_ram`
  will return `NA` if it cannot locate the command-line tool.

- 'MacOS':

  Uses command `'sysctl'` located at `'/usr/sbin/'` or `'/sbin/'`.
  Alternatively, you can edit the environment variable `'PATH'` to
  include the command-line tools if `'sysctl'` is missing. `get_ram`
  will return `NA` if it cannot locate `'sysctl'`.

- 'Linux':

  Uses the file `'/proc/meminfo'`, possibly the first entry
  `'MemTotal'`. If the file is missing or entry `'MemTotal'` cannot be
  located, `get_ram` will return `NA`.

## Examples

``` r
get_ram()
#> 16772575232.0 B
```
