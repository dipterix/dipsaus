# A Dipping Sauce to Data Analysis and Visualization

[![Travis build status](https://travis-ci.org/dipterix/dipsaus.svg?branch=master)](https://travis-ci.org/dipterix/dipsaus)
[![CRAN-version](https://www.r-pkg.org/badges/version/dipsaus)](https://CRAN.R-project.org/package=dipsaus)
<!-- [![Cran-version](http://cranlogs.r-pkg.org/badges/grand-total/dipsaus)](https://CRAN.R-project.org/package=dipsaus) -->


Package `dipsaus` provides `add-ons` to various packages such as `shiny`, `rlang`, `future`, etc. to enhance these packages. To install the development version package from `Github`,

```r
devtools::install_github('dipterix/dipsaus')
```

`dipsaus` provides `add-ons` from the following perspectives:

### 1. `Shiny` Customized Widgets ([Vignette](https://dipterix.github.io/dipsaus/articles/shiny_customized_widgets.html))

* `compoundInput2` assembles any shiny native inputs and create variable-length input;
* `actionButtonStyled` add styles to shiny default `actionButton` and its updating function can also update the styles or enable/disable the button;
* `sync_shiny_inputs` synchronize among shiny inputs without causing dead-locks in UI changes;
* `set_shiny_input` to set shiny `input` object;

### 2. Parallel Functions

* `collapse` is a parallel solution to calculate sum/mean along arrays, providing `4x` speed-ups for large arrays;
* `make_forked_clusters` enables `multicore` (forked clusters) in `future` package even in `RStudio` environment;
* `async_expr`, `async_lapply` applies to vectors/lists in parallel ways;
* `make_async_evaluator` queues arbitrary number of tasks and keeps them running in the background without blocking the main session. ([vignette](https://dipterix.github.io/dipsaus/articles/async_evaluator.html));
* `baseline_array` to baseline arrays in `RcppParallel`;
* `shift_array` to shift arrays along given dimensions;

### 3. Queue, Maps, Persist Containers

<!--* `qs_queue`, `text_queue`, `rds_queue`, `redis_queue` queue R objects and share across sessions; -->
* `qs_map`, `text_map`, `rds_map`, `redis_map` preserve R objects and share across sessions;
* `PersistContainer` provides two-level cache mechanism to save R objects in current session or preserve them on the local disk.

### 4. R Expressions ([Vignette](https://dipterix.github.io/dipsaus/articles/r_expr_addons.html))

* `eval_dirty` works as `base::eval`, but supports quosures generated from `rlang::quo`;
* `match_calls` match calls in a nested manner, and support changing the call arguments recursively;
* `%?<-%` assigns default values to left-hand object. E.g. `a %?<-% 1` assigns `a` to be `1` if `a` is `NULL` or does not exist.

### 5. Utility Functions ([Vignette](https://dipterix.github.io/dipsaus/articles/utility_functions.html))

* `cat2` act as base function `cat` but provides different levels with different colored outputs;
* `check_installed_packages`, `package_installed` checks if package(s) are installed;
* `col2hexStr` convert color to hex strings that are friendly to `HTML`, `CSS` and `JavaScript`;
* `parse_svec` converts a string into integer vectors, `deparse_svec` converts integer vectors back into a compact string;
* `drop_nulls` removes invalid items within lists;
* `get_cpu` gets CPU types and chipset names;
* `get_ram`, `mem_limit2` gets total RAM size as an alternatives for `mem.limits` in non-windows environment;
* `to_ram_size` provides simple way to convert numbers to printable storage sizes in given units;
* `time_delta` calculates time differences and returns a number in given units.

