# Add secondary 'CRAN'-like repository to the 'RStudio' settings

Add self-hosted repository, such as 'drat', 'r-universe' to 'RStudio'
preference. Please restart 'RStudio' to take changes into effect.

## Usage

``` r
rs_set_repos(name, url, add = TRUE)
```

## Arguments

- name:

  repository name, must be unique and readable

- url:

  the website address of the repository, starting with schemes such as
  `'https'`.

- add:

  whether to add to existing repository; default is true

## Value

a list of settings.

## Details

'RStudio' allows to add secondary 'CRAN'-like repository to its
preference, such that users can add on-going self-hosted developing
repositories (such as package `'drat'`, or 'r-universe'). These
repositories will be set automatically when running
[`install.packages`](https://rdrr.io/r/utils/install.packages.html).
