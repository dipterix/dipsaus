# Modified from https://github.com/wlandau/txtq
# Original Author: Eli Lilly and Company
NULL

assert_file <- function(x) {
  if (!file.exists(x)) {
    stop("invalid: file ", shQuote(x), " does not exist", call. = FALSE)
  }
  invisible()
}

assert_dir <- function(x) {
  assert_file(x)
  if (!dir.exists(x)) {
    stop(
      "invalid: file ",
      shQuote(x),
      " is not a directory",
      call. = FALSE
    )
  }
  invisible()
}

assert_file_scalar <- function(x) {
  assert_file(x)
  tryCatch({
    y <- scan(x, quiet = TRUE, what = integer())
    stopifnot(length(y) == 1L)
  },
  error = function(e) {
    stop(
      "invalid: file ",
      shQuote(x),
      " must contain an integer of length 1.",
      call. = FALSE
    )
  })
}


dir_create <- function(x) {
  if (!file.exists(x)) {
    dir.create(x, showWarnings = FALSE, recursive = TRUE)
  }
  if (!dir.exists(x)) {
    stop("txtq cannot create directory at ", shQuote(x), call. = FALSE)
  }
  invisible()
}

# Avoid truncation in base::file.create() # nolint
file_create <- function(x) {
  if (!file.exists(x)) {
    file.create(x)
  }
  invisible()
}

microtime <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS9 %z GMT")
}
