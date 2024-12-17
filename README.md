# Dipping Sauce for Data Analysis and Visualization

<!-- badges: start -->
[![CRAN-version](https://www.r-pkg.org/badges/version/dipsaus)](https://CRAN.R-project.org/package=dipsaus)
[![Develop](https://dipterix.r-universe.dev/badges/dipsaus)](https://dipterix.r-universe.dev/)
[![CRAN-downloads](http://cranlogs.r-pkg.org/badges/grand-total/dipsaus)](https://CRAN.R-project.org/package=dipsaus)
[![R-check](https://github.com/dipterix/dipsaus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dipterix/dipsaus/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


Package `dipsaus` provides `add-ons` to various packages such as `shiny`, `rlang`, `future`, etc. to enhance these packages. To install the package,

```r
# To install the development package
# pak::pak('dipterix/dipsaus')

# To install from CRAN
install.packages("dipsaus")
```

`dipsaus` provides `add-ons` from the following perspectives:

### 1. `Shiny` Customized Widgets ([Vignette](https://dipterix.org/dipsaus/articles/shiny_customized_widgets.html))

* `compoundInput2` assembles any shiny native inputs and create variable-length input;
* `actionButtonStyled` add styles to shiny default `actionButton` and its updating function can also update the styles or enable/disable the button;
* `sync_shiny_inputs` synchronize among shiny inputs without causing dead-locks in UI changes;
* `set_shiny_input` to set shiny `input` object;
* `flex_div` to display elements with `flex` layout;
* `html_asis` escapes string so that they will be displayed 'as-is' on websites;
* `progress2` shows shiny progress bar, but it also works without shiny;
* `shiny_is_running` check if shiny is running;

### 2. Parallel Functions

* `collapse` is a parallel solution to calculate sum/mean along arrays, providing `4x` speed-ups for large arrays;
* `make_forked_clusters` enables `multicore` (forked clusters) in `future` package even in `RStudio` environment;
* `baseline_array` to baseline arrays in multicore;
* `shift_array` to shift arrays along given dimensions;
* `lapply_async2` uses `future` package, but with progress bar either in console or in shiny apps;
* `fastcov2` calculates covariance matrices in parallel;
* `rs_exec` starts a new R session or job in `RStudio`;

### 3. R Expressions ([Vignette](https://dipterix.org/dipsaus/articles/r_expr_addons.html))

* `eval_dirty` works as `base::eval`, but supports quosures generated from `rlang::quo`;
* `match_calls` match calls in a nested manner, and support changing the call arguments recursively;
* `%?<-%` assigns default values to left-hand object. E.g. `a %?<-% 1` assigns `a` to be `1` if `a` is `NULL` or does not exist;
* `%=>%` provides JavaScript-style of creating functions;
* `new_function2` creates new function with quasi-quotation;
* `mask_function2` modifies function and mask certain elements within a function;
* `capture_expr` captures messages and always return results as one-line string;
* `forelse` provides Python-like for-else syntax;
* `test_farg` tests whether function contains desired arguments;

### 4. Utility Functions ([Vignette](https://dipterix.org/dipsaus/articles/utility_functions.html))

* `check_installed_packages`, `package_installed` checks if package(s) are installed;
* `col2hexStr` convert color to hex strings that are friendly to `HTML`, `CSS` and `JavaScript`;
* `parse_svec` converts a string into integer vectors, `deparse_svec` converts integer vectors back into a compact string;
* `drop_nulls` removes invalid items within lists;
* `get_ram`, `mem_limit2` gets total RAM size as an alternatives for `mem.limits` in non-windows environment;
* `to_ram_size` provides simple way to convert numbers to printable storage sizes in given units;
* `time_delta` calculates time differences and returns a number in given units;
* `ask_yesno`, `ask_or_default` interactively ask for user's input either in console or in `RStudio`;
* `fastmap2` and `fastqueue2` provides a wrapper of `fastmap` package, giving the object list-like operations;
* `shared_finalizer` finalizes multiple elements when last element is garbage collected;
* `dev_create` allows managing and grouping graphic devices;
* `print_directory_tree` prints directory and containing files into tree-structures;
* `rs_*` functions wrapping `rstudioapi` functions, but work without `RStudio`;
* `sexp_type2` get internal data type code;
* `to_datauri` converts file to `base64` formats.
