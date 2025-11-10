# Check If Packages Are Installed, Returns Missing Packages

Check If Packages Are Installed, Returns Missing Packages

## Usage

``` r
check_installed_packages(
  pkgs,
  libs = base::.libPaths(),
  auto_install = FALSE,
  ...
)
```

## Arguments

- pkgs:

  vector of packages to install

- libs:

  paths of libraries

- auto_install:

  automatically install packages if missing

- ...:

  other parameters for `install.packages`

## Value

package names that are not installed
