#' @importFrom digest digest
#' @export
digest::digest

#' Digest R object with source reference removed
#' @param object,... passed to \code{\link[digest]{digest}}
#' @param keep_source whether to keep the code that generates the object;
#' default is false
#' @seealso \code{\link{remove_source}}
#' @export
digest2 <- function(object, ..., keep_source = FALSE) {
  if(!keep_source) {
    remove_source(object)
  }
  digest::digest(object, ...)
}


#' @importFrom parallel detectCores
#' @export
parallel::detectCores

#' @importFrom cli ansi_strip
#' @export
cli::ansi_strip

#' @importFrom cli tree
#' @export
cli::tree

#' @importFrom rlang entrace
#' @export
rlang::entrace

#' @importFrom rlang cnd_entrace
#' @export
rlang::cnd_entrace

#' @importFrom rlang abort
#' @export
rlang::abort

#' @importFrom rlang error_cnd
#' @export
rlang::error_cnd

#' @importFrom rlang trace_back
#' @export
rlang::trace_back
