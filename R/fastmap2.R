#' @title A Wrapper for \code{fastmap::fastmap}
#' @description  \code{\link[fastmap]{fastmap}}
#' provides a key-value store where the keys are strings and the
#' values are any R objects. It differs from normal environment that
#' \code{\link[fastmap]{fastmap}} avoids memory leak. \code{fastmap2}
#' is a wrapper for \code{fastmap}, which provides several generic
#' functions such that it has similar behaviors to lists or
#' envitonments.
#' @param missing_default passed to \code{fastmap::fastmap}
#' @param i,j vector of names
#' @param name name, or key of the value
#' @param value any R object
#' @param x a \code{'fastmap2'} object
#' @param ... passed to other methods
#' @return A list of \code{'fastmap2'} instance
#' @examples
#'
#' ## --------------------------- Basic Usage --------------------------
#' map <- fastmap2()
#' map$a = 1
#' map$b = 2
#' print(map)
#'
#' map[c('a', 'b')]
#' # Alternative way
#' map['a', 'b']
#'
#' map[c('c', 'd')] <- 3:4
#' # or
#' map['e', 'f'] <- 5:6
#'
#' # The order is not guaranteed, unless sort=TRUE
#' as.list(map)
#' as.list(map, sort=TRUE)
#'
#' names(map)
#' length(map)
#'
#' ## ----------------------- NULL value handles -----------------------
#' map$b <- NULL
#' names(map)   # 'b' still exists!
#' as.list(map) # 'b' is NULL, but still there
#'
#' # to remove 'b', you have to use `@remove` method
#' map$`@remove`('b')
#'
#' ## ---------------- Native fastmap::fastmap methods -----------------
#'
#' # whether map has 'a'
#' map$`@has`('a')
#'
#' # Remove a name from list
#' map$`@remove`('a')
#'
#' # remove all from list
#' map$`@reset`()
#' print(map)
#'
#' @name fastmap2
#' @export
fastmap2 <- function(missing_default = NULL){
  map <- fastmap::fastmap(missing_default = missing_default)
  class(map) <- c('fastmap2', 'list')
  map
}

#' @rdname fastmap2
#' @export
`[[.fastmap2` <- function(x, name){
  if( startsWith(name, '@') ){
    .subset2(x, substring(name, 2))
  }else{
    .subset2(x, 'get')(name)
  }
}

#' @rdname fastmap2
#' @export
`$.fastmap2` <- `[[.fastmap2`

#' @rdname fastmap2
#' @export
`[[<-.fastmap2` <- function(x, name, value){
  .subset2(x, 'set')(name, value)
  return(x)
}

#' @rdname fastmap2
#' @export
`$<-.fastmap2` <- `[[<-.fastmap2`

#' @rdname fastmap2
#' @export
`[.fastmap2` <- function(x, i, j = NULL, ...){
  .subset2(x, 'mget')(unlist(c(i, j, ...)))
}

#' @rdname fastmap2
#' @export
`[<-.fastmap2` <- function(x, i, j = NULL, ..., value){
  i <- unlist(c(i, j, ...))
  stopifnot2(length(value) == length(i),
             msg='value must be the same length as name')
  do.call(.subset2(x, 'mset'), structure(as.list(value), names = i))
  x
}

#' @rdname fastmap2
#' @export
`names.fastmap2` <- function(x){
  re <- .subset2(x, 'keys')()
  if(!length(re)){ re = NULL }
  re
}

#' @rdname fastmap2
#' @export
`print.fastmap2` <- function(x, ...){
  cat('<Map, size=', .subset2(x, 'size')(),
      ', keys=[', paste(.subset2(x, 'keys')(), collapse = ', '),
      ']>\n', sep = '')
  invisible(x)
}

#' @rdname fastmap2
#' @export
`length.fastmap2` <- function(x){
  .subset2(x, 'size')()
}

#' @rdname fastmap2
#' @export
as.list.fastmap2 <- function(x, ...){
  .subset2(x, 'as_list')(...)
}