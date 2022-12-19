#' @title A Wrapper for \code{fastmap::fastqueue}
#' @param init,missing_default passed to \code{fastmap::fastqueue}
#' @param x a \code{'fastqueue2'} object
#' @param i,j integer index
#' @param ... integer indices or passed to other methods
#' @return A list of \code{'fastqueue2'} instance
#' @examples
#'
#' x <- fastqueue2()
#'
#' # add elements
#' x$madd(1, "b", function(){ "c" }, 4, "5")
#'
#' # print information
#' print(x)
#'
#' # get the second element without changing the queue
#' x[[2]]
#'
#' # remove and get the first element
#' x$remove()
#'
#' # the second item
#' x[[2]]
#'
#' # first two items in a list
#' x[c(1,2)]
#'
#' print(x)
#' as.list(x)
#'
#' @name fastqueue2
#' @export
fastqueue2 <- function(init = 20L, missing_default = NULL){
  queue <- fastmap::fastqueue(init = init, missing_default = missing_default)
  head <- 0
  count <- 0
  i <- NA
  ev <- new.env(parent = environment(.subset2(queue, 'as_list')))
  queue$at <- dipsaus::new_function2(args = alist(i=), {
    if(is.na(i) || i < 1L || i > count){
      stop("subscript out of bounds")
    }
    q[[head - count + i]]
  }, env = ev)
  queue$mat <- dipsaus::new_function2(args = alist(i=), {
    q[head - count + i]
  }, env = ev)
  class(queue) <- c('fastqueue2', 'list')
  queue
}

#' @title Copy elements to \code{fastqueue2}
#' @param li a list or an environment
#' @param queue \code{NULL} or a \code{fastqueue2} instance
#' @return If \code{map} is not \code{NULL}, elements will be added
#' to \code{map} and return \code{map}, otherwise create a new instance.
#' @export
list_to_fastqueue2 <- function(li, queue = NULL){
  stopifnot2(is.null(queue) || inherits(queue, 'fastqueue2'), msg = 'queue must be either NULL or fastqueue2')
  if(is.null(queue)){
    queue <- fastqueue2()
  }
  # for(ii in seq_along(li)){
  #   .subset2(queue, "add")(li[[ii]])
  # }
  if(!is.list(li)){
    li <- as.list(li)
  }
  if(length(li)){
    .subset2(queue, "madd")(.list = li)
  }
  queue
}


#' @rdname fastqueue2
#' @export
`[[.fastqueue2` <- function(x, i){
  .subset2(x, "at")(i)
}

#' @export
`[[<-.fastqueue2` <- function(x, i, value){
  stop("Cannot set index of a queue. Use `$add` or `$madd` method.")
  return(x)
}

#' @export
`$<-.fastqueue2` <- `[[<-.fastqueue2`

#' @rdname fastqueue2
#' @export
`[.fastqueue2` <- function(x, i, j = NULL, ...){
  if(missing(i)) {
    return( .subset2(x, "as_list")(...) )
  } else {
    return( .subset2(x, "mat")(unlist(c(i, j, ...))) )
  }

}

#' @export
`[<-.fastqueue2` <- function(x, i, j = NULL, ..., value){
  stop("Cannot subset-assign a queue. Use `$add` or `$madd` method.")
  x
}

#' @rdname fastqueue2
#' @export
`print.fastqueue2` <- function(x, ...){
  cat('<Queue, size=', .subset2(x, 'size')(), '>\n', sep = '')
  invisible(x)
}

#' @rdname fastqueue2
#' @export
`length.fastqueue2` <- function(x){
  .subset2(x, 'size')()
}

#' @rdname fastqueue2
#' @export
as.list.fastqueue2 <- function(x, ...){
  .subset2(x, 'as_list')()
}
