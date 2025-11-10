# Print Directory Tree

Print Directory Tree

## Usage

``` r
print_directory_tree(
  target,
  root = "~",
  child,
  dir_only = FALSE,
  collapse = NULL,
  ...
)
```

## Arguments

- target:

  target directory path, relative to `root`

- root:

  root directory, default is `'~'`

- child:

  child files in target; is missing, then list all files

- dir_only:

  whether to display directory children only

- collapse:

  whether to concatenate results as one single string

- ...:

  pass to [`list.files`](https://rdrr.io/r/base/list.files.html) when
  list all files

## Value

Characters, print-friendly directory tree.
