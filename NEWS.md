dipsaus 0.2.9
=======

## Changes

* Added `is_from_namespace` to check if a function is from a namespace
* Reexported `jsonlite` functions
* Fixed potential imbalance protection in `quantile2`
* Fixed a documentation issue

dipsaus 0.2.8
=======

## Changes

* Added `fancyFileInput` with customized `CSS` to original `shiny::fileInput`

dipsaus 0.2.7
=======

## Changes

* Added more default shortcuts
* Used `system2` instead of `system` in `rs_runjob_alt` for more controls
* Upgraded `JavaScript` libraries
* Fixed `lapply_async2` not respecting chunk size when `callback` is unspecified
* Removed `cpp11` compiling flags to use system defaults
* Actively clear users cache directory when directories are empty

dipsaus 0.2.6
=======

## Changes

* Fixed `fastmap2` and `fastqueue2` when subsetting with missing index resulting in errors
* Re-exports some `cli`, `rlang` functions
* Using the most recent fixed version of `tinythread` that correctly join `pthread`
* Allowed `rs_exec` to return as a promise object
* Removed `base64url` from dependence and implemented equivalent functions using `base64enc`
* Removed `prepare_install2`
* Removed `startup` from dependence
* Fixed `Javascript` vulnerability issues

dipsaus 0.2.5
=======

## Changes

* Removed `RcppParallel`, using `TinyThread` only to avoid `TBB` related installation issues
* Removed `autoconf` script, using a much simpler way to generate compiler flags
* Added keyboard shortcut to run debugging code easily
* Allowed `parse_svec` to have space around connectors
* Added a new method to `baseline_array` for absolute contrast

## Bug Fixes

* Fixed `TinyThread` memory leak issues

dipsaus 0.2.4
=======

## Major Changes

* Automatically set system environments to be the same as master process in `rs_exec`
* Added `rs_set_repos` to set `RStudio` preference, allowing to add `CRAN`-like repositories when running inside of `RStudio` 

## Minor Changes

* Upgraded `JavaScript` libraries to fix the dependence vulnerability issues

dipsaus 0.2.3
=======

## Major Changes

* Added `rstudioapi` wrapper `rs_edit_file` to allow editing files directly in `RStudio`

## Minor Changes

* `parse_svec` handles multiple lengths all together, and `parse_svec(1:10)` is equivalent to `parse_svec("1:10")`
* Allowed `rs_exec` to run nested. However, there is a risk of parent process ends before the nested child process finishes. In this case, parent temporary directory will be destroyed, and child process will become orphan (results will become unavailable) 

## Bug Fixes

* Fixed "offset-parent" issue in `compoundInput2` when max-height is unset, but overflow is still hidden for the select inputs

dipsaus 0.2.2
=======

## Major Changes
* Added a special `%OF%` to make sure the result is constrained

## Minor Changes

* Replaced some functions from `stringr` to self-implemented base-R functions, plan to remove `stringr` in the future
* Create `shiny_input_bindings` list on package load

## Bug Fixes

* Fixed `print_directory_tree` where an `if` condition generates warning for vectors with lengths greater than `1`

dipsaus 0.2.1
=======

## Major Changes
* Removed `synchronicity` package from dependency
* `dipsaus_lock` and `dipsaus_unlock` is exclusive-only and will be deprecated in the future
* `PersistContainer` is scheduled to be deprecated in the future
* Re-export `detectCores` and `digest`
* Added `get_credential` to generate strong passwords for different services using one single master password

## Enhancement
* `sync_shiny_inputs` uses `fastmap` instead of environments
* `sync_shiny_inputs` uses the new `bindEvent` scheme to replace `observeEvent`
* Added `sorted` argument to `as.list.fastmap2`
* Ignored `...` argument in `as.list.fastqueue2`
* `rs_show_shortcut` shows current shortcut expressions

## Bug Fixes
* `compoundInput2` now activates callback functions once using `updateCompoundInput2`
* Fixed `get_ram` not working on windows when user has no access to run `wmic` command
* Allow `lapply_async` progress bar to show in `shiny` applications (with backward compatibility)


dipsaus 0.2.0
=======

## Major Changes
* Allow `fastmap2` to `as.list` recursively
* Added `mean_se` to calculate mean and standard error of mean
* Re-exported `digest` from `digest` package
* Added `combine_html_class` and `remove_html_class` to manipulate `HTML` classes
* Force enabled `ANSI` color display for `rs_exec` when the jobs run in `RStudio`

## Minor Changes
* Cleaned `get_os` so it does not depend on `stringr` anymore

## Big Fixes
* Fixed `progress2` not working properly in non-interactive sessions (but `shiny` is still running)

dipsaus 0.1.9
=======

## Minor Changes
* `compoundInput2` now respects the `par(fg)` and sets foreground accordingly

## Big Fixes
* Fixed `missing_dots` freezing the session when incorrectly called
* Fixed `autoconf` warnings

dipsaus 0.1.8
=======

## Major Changes
* Added `lapply_callr` to replace `async_workers` with cleaner and more robust implementation
* Added `as_pipe` to make any functions to pipe-friendly
* Added `rs_add_shortcut` to allow customized shortcuts in `RStudio`
* Added `missing_dots` to detect if dots are missing values

## Bug Fixes
* Fixed a bug in `lapply_async2` when failing to restore plan

## Enhancements
* Updated `JavaScript` libraries

dipsaus 0.1.7
=======

## Major Changes
* Added `fastquantile` to estimate single quantile, which is faster than base R
* Added `RStudio` shortcuts, allowing customized code from shortcuts
* Added `shiny_alert2` that launches large alerts in shiny applications
* Added `%<-?%` to only assign when right-hand side expression is valid and not null
* Removed `qs_map`, `redis_map`
* Removed dependence `qs`, `RcppRedis`, `htmltools`
* Removed `make_async_evaluator`


dipsaus 0.1.6
=======

## Major Changes
* Added `fastcov2` to calculate covariance in parallel
* Added `sumsquared` to calculate sum-squared faster
* Added `sexp_type` to obtain internal data type code

## Bug Fixes
* Robust `get_ram` function. The function will return `NA` on `Solaris`, and actual memories in byte on other platforms.
* Fixed `rs_focus_console` not doing its job
* Fixed `rs_exec` when `wait` is true but not actually waiting

## Enhancements
* Updated `JavaScript` libraries to fix vulnerability issues
* Allow `rs_exec` to return to console after submitting jobs (`RStudio` version 1.4 required)
* Removed unused functions in `README.md`.

dipsaus 0.1.4, 0.1.5
=======

## Major Changes
* Removed function `get_cpu`
* Added function to convert `base64` to its encoding characters

## Bug Fixes
* Robust `get_ram` function. The function will return `NA` on `Solaris`, and actual memories in byte on other platforms.

dipsaus 0.1.3
=======

## Major Changes
* Added a wrapper for `fastmap::fastqueue` (`fastqueue2`) that shows friendly messages and can query elements
* Added wrapper method to convert any `Base64` string to its generating string
* Soft-deprecate `lock` and `unlock` functions
* Soft remove the experimental function `make_async_evaluator` to embrace the new `restbatch` package

dipsaus 0.1.2
=======

## Major Changes
* Added `make_forked_clusters` and `lapply_async2` now restores previous future strategy one exiting

## Minor Changes
* `make_forked_clusters` allows to set a backup strategy once forked process fails (for example, on windows)

## Bug Fixes
* RAM usage is corrected on windows
* Bumped JavaScript dependence `ini`

dipsaus 0.1.1
=======

## Major Changes
* Added `async_works` to allow scheduling works in the background using multiple sessions.

## Minor Changes
* Instead of showing "finished", progress bar now shows square box once finished for cleaner output
* `rs_exec` now allows packages to be loaded before scripts
* Internally added `attached_packages` to detect attached packages

## Bug Fixes
* Fixed `lapply_async2` when plan is `multisession`, added future seed

dipsaus 0.1.0
=======

## Major Changes
* Removed `uglifyjs` and use `terser` instead
* `rs_exec` now respects `wait` when running as `RStudio` jobs
* `use_shiny_dipsaus` can wrap around `HTML` tags now

## Bug Fixes
* `use_shiny_dipsaus` now correctly add `JavaScript` and `css` tags
* `uglifyjs` is not maintained any more and it might cause vulnerability issues, fixed the dependency versions

dipsaus 0.0.9
=======

## Major Changes
* Added `rstudioapi` related functions. When running with 'RStudio', take advantages of 'RStudio' user interface; when running in console, or non-interactive context, use default methods.
  + `rs_active_project`: 'RStudio' current active project name
  + `rs_save_all`: save all editing document when 'RStudio' is running
  + `rs_exec`: run job in 'RStudio', otherwise use `Rscript`
  + `ask_yesno`, `ask_or_default` also uses 'RStudio' dialogue
  + other functions starting with `rs`
* `add_to_session` stores key-value pairs in shiny reactive sessions
* `clear_env` clear elements in environments or `fastmap2` instances
* `do_nothing` literally does nothing
* `shiny_is_running` returns whether current context is within a shiny app
* `test_farg` tests whether a function contains certain arguments
* `capture_expr` captures outputs when evaluating expression and returns a string
* `get_dev_attr` get attributes from graphic devices
* `print_directory_tree` generates directory tree as text

## Minor Changes
* `dev_create` can now set attributes and those attributes can be obtained by function `get_dev_attr` or method `dev_attributes`

## Bug Fixes
* soft-deprecated `prepare_install` that might cause infinite loops


dipsaus 0.0.8
=======

## Major Changes
* Removed dependency on `crayon`, added `yaml`
* Added `shared_finalizer` to finalize multiple objects only when last one is garbage collected
* Removed `autoconf` script and `Makevars` as no special flags are needed
* added `flex_div` to generate `div` with `flex` layout and calculate size automatically

dipsaus 0.0.7
=======

## Major Changes
* Removed dependency on `txtq` and functions related
* Added `list_to_fastmap2` to convert a list to `fastmap2`
* Exported `to_datauri`, allowing any files, especially images to be translated to base-64 code and can be directly used in websites
* `mask_function2` to mask variables within function by adding one environment layer on top of the function
* `new_function2` creates function that supports 'quasi-quosure' syntax

## Minor Changes
* Allow maximum height set for `compoundInput2`
* `fastmap2` now accept multi-assignment with single value

## Bug Fixes
* `fastmap2` subset method now accept integers instead raising errors


dipsaus 0.0.6
=======

## Major Changes
* Added support for R 3.5
* Added `%D%` a decorator function that works like python decorators
* Added `get_dots` that can get variable from `...` without evaluating other variables

## Minor Changes
* `lapply_async2` now supporting more load-balance settings

## Bug Fixes
* 11 JavaScript library vulnerability bug fixed

dipsaus 0.0.5
=======

## Major Changes
* Added `dev_create` to control switching among graphical devices
* Added `use_shiny_dipsaus()` to import all styles and JavaScript
* Added a wrapper for `fastmap::fastmap` - `fastmap2`, which provides several generic functions, making the original `fastmap` more like an environment
* Added `lapply_async2`, a naive version of `future.lapply`, but with callbacks, which is useful if play-back message is required. This is experimental. In the future when package `progressr` is online, this function will be adjusted.

## Bug Fixes
* Resolved memory leaks in `compoundInput2`
* Use `fastmap2` instead of environments to manage `set_shiny_input`

dipsaus 0.0.4
=======

## Major Changes
* Canceled exporting queues because of strange behavior in windows. (plan to fix this for the next version)
* Implemented function to convert Base64 `DataURI` string to image
* `cat2` now persist logger files on local drive, and can be exported via `packup_logger`
* `set_shiny_input` to set shiny `input` object
* `baselineArray` calculates baseline for arrays with internal parallel support
* `shift_array` to fast shift array along certain dimension
* `%+-%` plus-minus operator; `%=>%` a "JavaScript" style of creating functions

## Bug Fixes
* Fixed `compoundInput2` not initialized when rendered in `shiny::renderUI`

dipsaus 0.0.3
=======

## Major Changes

### Parallel 
* Added four cross-session map types `qs_map`, `text_map`, `rds_map`, and `redis_map`
* Created `R6` class `PersistContainer`, designed to cache key-value pairs on the local hard disk
* Implemented function `make_async_evaluator` that works as scheduler to queue arbitrary number of tasks without blocking main R session

## Minor Changes
* Renamed `txtq_queue` to `text_queue`
* Added vignettes
* Minor fixes


dipsaus 0.0.2
=======

## Major Changes

### Shiny
* Use `webpack` to manage `JavaScript` libraries
* Added new shiny widget `actionButtonStyled`
* Implemented `sync_shiny_inputs` to synchronize among shiny inputs without causing dead-locks in UI changes
* `progress2` allows debugging shiny progress instances in non- reactive context. It uses `shiny::Progress` in shiny reactive context, and just print out messages when shiny reactive domain is `NULL`

### Parallel 
* Implemented `collapse`, a parallel solution to calculate sum/mean along arrays, providing `4x` speed-ups for large arrays;
* Added `async_expr` functions using `future` package for parallel evaluation of R expressions.
* Four cross-session queue types `qs_queue`, `rds_queue`, `txtq_queue`, `redis_queue` allow pushing arbitrary R objects into queue in one session and pop them from another session.

### R-language
* `eval_dirty` works as `base::eval`, but supports quosures generated from `rlang::quo`
* `match_calls` match calls in a nested manner, and support changing the call arguments recursively
* `%?<-%` to assign default values to `lhs`

## Minor Changes
* `cat2` act as base function `cat` but provides different levels with different colored outputs;
* `check_installed_packages`, `package_installed` checks if package(s) are installed;
* `col2hexStr` convert color to hex strings that are friendly to `HTML`, `CSS` and `JavaScript`;
* `parse_svec` converts a string into integer vectors, `deparse_svec` converts integer vectors back into a compact string;
* `drop_nulls` removes invalid items within lists;
* `get_cpu` gets CPU types and chip-set names;
* `get_ram`, `mem_limit2` gets total RAM size as an alternatives for `mem.limits` in non-windows environment;
* `to_ram_size` provides simple way to convert numbers to printable storage sizes in given units;
* `time_delta` calculates time differences and returns a number in given units.


dipsaus 0.0.1
=======

## Major Changes

* Implemented full `JavaScript` support for `compoundInput2` as a new `shiny` input to
* Added utility function `col2hexStr` to convert color to hex value
* Added utility function `match_calls` to match and modify calls

## Minor Changes

* Added `label_color` to `compoundInput2` to enable colored group labels

dipsaus 0.0.0
=======

* Initial private beta release!
