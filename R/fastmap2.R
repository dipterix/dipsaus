#' @title A Wrapper for \code{fastmap::fastmap}
#' @description  \code{\link[fastmap]{fastmap}}
#' provides a key-value store where the keys are strings and the
#' values are any R objects. It differs from normal environment that
#' \code{\link[fastmap]{fastmap}} avoids memory leak. \code{fastmap2}
#' is a wrapper for \code{fastmap}, which provides several generic
#' functions such that it has similar behaviors to lists or
#' environments
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

#' @title Copy elements to \code{fastmap2}
#' @param li a list or an environment
#' @param map \code{NULL} or a \code{fastmap2} instance
#' @return If \code{map} is not \code{NULL}, elements will be added
#' to \code{map} and return \code{map}, otherwise create a new instance.
#' @export
list_to_fastmap2 <- function(li, map = NULL){
  stopifnot2(is.null(map) || inherits(map, 'fastmap2'), msg = 'map must be either NULL or fastmap2')
  if(is.null(map)){
    map <- fastmap2()
  }
  for(nm in names(li)){
    if(nm != ''){
      map[[nm]] <- li[[nm]]
    }
  }
  map
}

#' @title Migrate a \code{fastmap2} object to a new one
#' @param from,to \code{fastmap2} object
#' @param override whether to override keys in \code{to} if they exist
#' @return Map \code{to}
#' @seealso \code{\link{fastmap2}}
update_fastmap2 <- function(from, to, override = TRUE){
  if(override){
    new_list <- .subset2(from, 'as_list')()
  } else{
    keys <- .subset2(from, 'keys')()
    keys <- keys[!keys %in% .subset2(to, 'keys')()]
    new_list <- .subset2(from, 'mget')(keys)
  }
  if(length(new_list)){
    .subset2(to, 'mset')(.list = new_list)
  }
  return(to)
}

#' @rdname fastmap2
#' @export
`[[.fastmap2` <- function(x, name){
  name <- as.character(name)
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
  .subset2(x, 'set')(as.character(name), value)
  return(x)
}

#' @rdname fastmap2
#' @export
`$<-.fastmap2` <- `[[<-.fastmap2`

#' @rdname fastmap2
#' @export
`[.fastmap2` <- function(x, i, j = NULL, ...){
  .subset2(x, 'mget')(as.character(unlist(c(i, j, ...))))
}

#' @rdname fastmap2
#' @export
`[<-.fastmap2` <- function(x, i, j = NULL, ..., value){
  i <- unlist(c(i, j, ...))
  # instead of throwing error,
  stopifnot2(length(value) <= 1 || length(value) == length(i),
             msg='value must be the same length as name')
  if( length(value) == length(i) ){
    .subset2(x, 'mset')(.list = structure(as.list(value), names = as.character(i)))
  } else {
    # set for each key
    for(k in i){
      .subset2(x, 'set')(as.character(k), value)
    }
  }

  x
}

#' @rdname fastmap2
#' @export
`names.fastmap2` <- function(x){
  re <- .subset2(x, 'keys')()
  if(!length(re)){ re <- NULL }
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
