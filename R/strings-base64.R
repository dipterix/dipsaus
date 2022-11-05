base64_urlinternal_encoder <- function(x) {
  base64enc::base64encode(charToRaw(x))
}
base64_urlinternal_decoder <- function(x) {
  rawToChar(base64enc::base64decode(what = x), multiple = FALSE)
}

#' @name base64-url
#' @title Encode or decode 'base64'
#' @description Compatible with results from package \code{'base64url'},
#' but implemented with package \code{'base64enc'}
#' @param x character vector to encode or decode
#' @returns character vector of the same length as \code{x}
#'
#' @export
base64_urlencode <- function(x) {
  if(!length(x)) { return(character(x)) }
  re <- vapply(enc2utf8(as.character(x)), base64_urlinternal_encoder,
               "", USE.NAMES = FALSE)
  re <- gsub("[=]{0,}$", "", re)
  re <- gsub("[+]", "-", re)
  gsub("/", "_", re)
}

base64_urldecode <- function(x) {
  x <- gsub("-", "+", x)
  x <- gsub("_", "/", x)
  vapply(x, base64_urlinternal_decoder, "", USE.NAMES = FALSE)
}
