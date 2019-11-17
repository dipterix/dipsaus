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
* `get_cpu` gets CPU types and chipset names;
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
