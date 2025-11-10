# Check if a package is installed

Check if a package is installed

## Usage

``` r
package_installed(pkgs, all = FALSE)
```

## Arguments

- pkgs:

  vector of package names

- all:

  only returns TRUE if all packages are installed. Default is FALSE.

## Value

logical, if packages are installed or not. If `all=TRUE`, return a
logical value of whether all packages a re installed.

## Examples

``` r
# Check if package base and dipsaus are installed
package_installed(c('base', 'dipsaus'))
#>    base dipsaus 
#>    TRUE    TRUE 

# Check if all required packages are installed
package_installed(c('base', 'dipsaus'), all = TRUE)
#> [1] TRUE
```
