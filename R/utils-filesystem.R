
assert_file <- function(x) {
  if (file.exists(x)) {
    return(invisible())
  }
  cat2("File ", shQuote(x), " does not exist", level = 'FATAL')
}

assert_dir <- function(x) {
  if (dir.exists(x)) {
    return(invisible())
  }
  cat2("Path ", shQuote(x), " is not a directory", level = 'FATAL')
}

assert_file_scalar <- function(x, allow_decimal = FALSE) {
  assert_file(x)
  y <- readLines(x, n = 1, warn = FALSE)
  is_scalar <- FALSE
  if(allow_decimal){
    is_scalar <- stringr::str_detect(y, '^[0-9]*[.]{0,1}[0-9]*$') && stringr::str_detect(y, '[0-9]+')
  }else{
    is_scalar <- stringr::str_detect(y, '^[0-9]+$')
  }

  if( !is_scalar ){
    cat2('File ', shQuote(x), ' contains no ', ifelse(allow_decimal, 'number', 'integer'),
         ' of length 1', level = 'FATAL')
  }
  return(invisible())
}


dir_create <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive)
  }
  if (check && !dir.exists(x)) {
    cat2('Cannot create directory at ', shQuote(x), level = 'FATAL')
  }
  invisible(normalizePath(x))
}

file_create <- function(x, showWarnings = FALSE, recursive = TRUE) {
  if (!file.exists(x)) {
    dir <- dirname(x)
    if( recursive && !dir.exists(dir) ){
      dir_create(dir)
    }
    file.create(x, showWarnings = showWarnings)
  }
  invisible(normalizePath(x))
}

microtime <- function() {
  strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS9", usetz = TRUE)
}
