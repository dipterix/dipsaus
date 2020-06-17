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
