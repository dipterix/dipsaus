# Tests for missing_dots() / check_missing_dots()
#
# These tests capture the expected behavior of check_missing_dots (C++
# implementation) through its R interface missing_dots().
#
# The C++ implementation was originally using Rf_findVarInFrame() to locate
# the `...` symbol in the calling environment's frame.  That entry point was
# replaced with R_getVarEx() (public C API, R >= 4.5.0) to comply with CRAN
# policy on non-API entry points.  The tests below were written BEFORE the
# replacement to lock down the exact behaviour, and are used to verify that
# the new implementation is identical.

test_that("missing_dots: basic usage in a function with dots", {
  f <- function(...) missing_dots(environment())

  # No arguments supplied → zero-length logical vector
  expect_identical(f(), logical(0))

  # Single non-missing argument
  expect_identical(f(1), FALSE)

  # Multiple non-missing arguments
  expect_identical(f(1, 2, 3), c(FALSE, FALSE, FALSE))

  # Single missing argument (bare comma)
  expect_identical(f(,), c(TRUE, TRUE))

  # Mixed: first missing, second not
  expect_identical(f(, 1), c(TRUE, FALSE))

  # Mixed: first not, second missing
  expect_identical(f(1, ), c(FALSE, TRUE))

  # Mixed interleaved
  expect_identical(f(1, , 3), c(FALSE, TRUE, FALSE))

  # All missing (three bare commas → four missing slots)
  expect_identical(f(, , ,), c(TRUE, TRUE, TRUE, TRUE))
})

test_that("missing_dots: named arguments behave the same as positional", {
  f <- function(...) missing_dots(environment())

  # Named non-missing
  expect_identical(f(a = 1, b = 2), c(FALSE, FALSE))

  # Named with one missing
  expect_identical(f(a = 1, b = , c = 3), c(FALSE, TRUE, FALSE))

  # All named and missing
  expect_identical(f(a = , b = ), c(TRUE, TRUE))
})

test_that("missing_dots: arguments are NOT evaluated", {
  f <- function(...) missing_dots(environment())

  evaluated <- FALSE

  # delayedAssign creates a promise in this environment; when R wraps it as a
  # dots entry for f(), the inner promise must NOT be forced by missing_dots.
  delayedAssign("lazy_arg", {
    evaluated <<- TRUE
    99
  })

  result <- f(1, lazy_arg)
  expect_false(evaluated,
    info = "missing_dots must not force/evaluate promise arguments")
  expect_identical(result, c(FALSE, FALSE))
})

test_that("missing_dots: rejects non-environment argument", {
  expect_error(missing_dots(1L),
    regexp = "must be an environment",
    info   = "passing an integer should error")

  expect_error(missing_dots(NULL),
    regexp = "must be an environment",
    info   = "passing NULL should error")

  expect_error(missing_dots(list()),
    regexp = "must be an environment",
    info   = "passing a list should error")
})

test_that("missing_dots: parent.frame() default works at one level of indirection", {
  outer <- function(...) {
    inner <- function() missing_dots(parent.frame())
    inner()
  }

  expect_identical(outer(1, , 3), c(FALSE, TRUE, FALSE))
})

test_that("missing_dots: check_missing_dots C-level accepts environment only", {
  # The underlying C function should reject non-environments just like the
  # R wrapper does.
  expect_error(check_missing_dots(42L),   regexp = "environment")
  expect_error(check_missing_dots("x"),   regexp = "environment")
  expect_error(check_missing_dots(NULL),  regexp = "environment")
})

# ---------------------------------------------------------------------------
# Tests for is_namespace() / is_from_namespace()
#
# These lock down behaviour before and after replacing Rf_findVarInFrame with
# R_getVarEx in the is_namespace() C++ implementation.
# ---------------------------------------------------------------------------

test_that("is_namespace: identifies namespace environments correctly", {
  # Package namespaces → TRUE
  expect_true(is_namespace(getNamespace("dipsaus")))
  expect_true(is_namespace(getNamespace("base")))

  # Ordinary environments → FALSE
  expect_false(is_namespace(globalenv()))
  expect_false(is_namespace(baseenv()))   # base *package* env ≠ base namespace
  expect_false(is_namespace(emptyenv()))
  expect_false(is_namespace(new.env()))
})

test_that("is_from_namespace: functions and environments from packages", {
  # Base env itself is treated as from a package (it lives in the base namespace
  # parent chain)
  expect_true(is_from_namespace(baseenv()))

  # A function exported from utils lives in the utils namespace
  expect_true(is_from_namespace(utils::read.csv))

  # NULL, plain values, empty env → FALSE
  expect_false(is_from_namespace(NULL))
  expect_false(is_from_namespace(emptyenv()))

  # Anonymous function intended to represent a "user-space" function -> FALSE.
  # We must explicitly set its enclosing env to globalenv() because when tests
  # run via devtools::load_all() the test body's parent chain is rooted in the
  # dipsaus namespace, making any inline `function(){}` appear to come from the
  # namespace.  Setting the environment to globalenv() is the context-
  # independent way to represent a function that is not from any package.
  anon <- function() {}
  environment(anon) <- globalenv()
  expect_false(is_from_namespace(anon))

  # Function whose enclosing env is a plain new.env() → FALSE even recursively.
  # Create f normally (so `function` is in scope), then move its enclosing env.
  # Using local({}, envir = e) would fail when e's parent is emptyenv() because
  # `{` and `function` cannot be found during evaluation.
  e <- new.env(parent = emptyenv())
  f <- function() {}
  environment(f) <- e
  expect_false(is_from_namespace(f, recursive = FALSE))
  expect_false(is_from_namespace(f, recursive = TRUE))
})
