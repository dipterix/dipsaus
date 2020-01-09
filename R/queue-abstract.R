#' @title Defines abstract queue class
#'
#' @description This class is inspired by \url{https://cran.r-project.org/package=txtq}.
#' The difference is \code{AbstractQueue} introduce an abstract class that can
#' be extended and can queue not only text messages, but also arbitrary R
#' objects, including expressions and environments. All the queue types in this
#' package inherit this class.
#'
#' @name AbstractQueue
#'
#' @section Abstract Public Methods:
#'
#' Methods start with \code{@@...} are not thread-safe. Most of them are not
#' used directly by users. However, you might want to override them if you
#' inherit this abstract class. Methods marked as "(override)" are not
#' implemented, meaning you are supposed to implement the details. Methods
#' marked as "(optional)" usually have default alternatives.
#'
#' \describe{
#' \item{\code{initialize(...)} (override)}{
#' The constructor. Usually three things to do during the process:
#' 1. set \code{get_locker} \code{free_locker} if you don't want to use the
#' default lockers. 2. set lock file (if using default lockers). 3. call
#' \code{self$connect(...)}
#' }
#' \item{\code{get_locker()}, \code{free_locker()} (optional)}{
#' Default is \code{NULL} for each methods, and queue uses an internal
#' \code{private$default_get_locker} and \code{private$default_free_locker}.
#' These two methods are for customized locker, please
#' implement these two methods as functions during \code{self$initialization}
#' \code{get_locker} obtains and lock access (exclusive), and \code{free_locker}
#' frees the locker. Once implemented, \code{private$exclusive} will take care
#' the rest. Type: function; parameters: none; return: none
#' }
#' \item{\code{@@get_head()}, \code{@@set_head(v)} (override)}{
#' Get head so that we know where we are in the queue \code{self$@@get_head()}
#' should return a integer indicating where we are at the queue
#' \code{self$@@set_head(v)} stores that integer. Parameter \code{v} is always
#' non-negative, this is guaranteed. Users are not supposed to call these
#' methods directly, use \code{self$head} and \code{self$head<-} instead.
#' However, if you inherit this class, you are supposed to override the methods.
#' }
#' \item{\code{@@get_total()}, \code{@@set_total(v)} (override)}{
#' Similar to \code{@@get_head} and \code{@@set_head}, defines the total items
#' ever stored in the queue. total-head equals current items in the queue.
#' }
#' \item{\code{@@inc_total(n=1)} (optional)}{
#' Increase total, usually this doesn't need to be override, unless you are
#' using files to store total and want to decrease number of file connections
#' }
#' \item{\code{@@append_header(msg, ...)} (override)}{
#' \code{msg} will be vector of strings, separated by "|", containing encoded
#' headers: `time`, `key`, `hash`, and `message`. to decode what's inside, you
#' can use \code{self$print_items(stringr::str_split_fixed(msg, '\\|', 4))}.
#' \strong{Make sure} to return a number, indicating number of items stored.
#' Unless handled elsewhere, usually \code{return(length(msg))}.
#' }
#' \item{\code{@@store_value(value, key)} (override)}{
#' Defines how to store value. `key` is unique identifier generated from
#' time, queue ID, and value. Usually I use it as file name or key ID in
#' database. value is an arbitrary R object to store. you need to store value
#' somewhere and return a string that will be passed as `hash` in
#' \code{self$restore_value}.
#' }
#' \item{\code{restore_value(hash, key, preserve = FALSE)} (override)}{
#' Method to restore value from given combination of `hash` and `key`.
#' `hash` is the string returned by \code{@@store_value}, and `key` is the same
#' as key in \code{@@store_value}. preserve is a indicator of whether to
#' preserve the value for future use. If set to \code{FALSE}, then you are
#' supposed to free up the resource related to the value. (such as free memory
#' or disk space)
#' }
#' \item{\code{@@log(n = -1, all = FALSE) (override)}}{
#' get \code{n} items from what you saved to during \code{@@append_header}.
#' \code{n} less equal than 0 means listing all possible items.
#' If \code{all=TRUE}, return all items (number of rows should equals to
#' \code{self$total}), including popped items. If \code{all=FALSE}, only
#' return items in the queue (number of rows is \code{self$count}). The
#' returned value should be a \code{n x 4} matrix. Usually I use
#' \code{stringr::str_split_fixed(..., '\\|', 4)}. Please see all other
#' types implemented for example.
#' }
#' \item{\code{@@reset(...)} (override)}{
#' Reset queue, remove all items and reset head, total to be 0.
#' }
#' \item{\code{@@clean()} (override)}{
#' Clean the queue, remove all the popped items.
#' }
#' \item{\code{@@validate()} (override)}{
#' Validate the queue. Stop if the queue is broken.
#' }
#' \item{\code{@@connect(con, ...)} (override)}{
#' Set up connection. Usually should be called at the end of
#' \code{self$initialization} to connect to a database, a folder, or an
#' existing queue you should do checks whether the connection is new or it's
#' an existing queue.
#' }
#' \item{\code{connect(con, ...)} (optional)}{
#' Thread-safe version. sometimes you need to override this function instead
#' of \code{@@connect}, because \code{private$exclusive} requires \code{lockfile}
#' to exist and to be locked. If you don't have lockers ready, or need to set
#' lockers during the connection, override this one.
#' }
#' \item{\code{destroy()} (optional)}{
#' Destroy a queue, free up space and call
#' \code{delayedAssign('.lockfile', {stop(...)}, assign.env=private)} to raise
#' error if a destroyed queue is called again later.
#' }
#' }
#'
#' @section Public Methods:
#'
#' Usually don't need to override unless you know what you are doing.
#'
#' \describe{
#' \item{\code{push(value, message='',...)}}{
#' Function to push an arbitrary R object to queue. \code{message} is a string
#' giving notes to the pushed item. Usually message is stored with header,
#' separated from values. The goal is to describe the value. \code{...} is
#' passed to \code{@@append_header}
#' }
#' \item{\code{pop(n = 1, preserve = FALSE)}}{
#' Pop \code{n} items from the queue. \code{preserve} indicates whether not to
#' free up the resources, though not always guaranteed.
#' }
#' \item{\code{print_item(item)}, \code{print_items(items)}}{
#' To decode matrix returned by \code{log()}, returning named list or data frame
#' with four heads: `time`, `key`, `hash`, and `message`.
#' }
#' \item{\code{list(n=-1)}}{
#' List items in the queue, decoded. If \code{n} is less equal than 0, then
#' list all results. The result is equivalent to
#' \code{self$print_items(self$log(n))}
#' }
#' \item{\code{log(n=-1,all=FALSE)}}{
#' List items in the queue, encoded. This is used with \code{self$print_items}.
#' When \code{all=TRUE}, result will list the  records ever pushed to the queue
#' since the last time queue is cleaned. When \code{all=FALSE}, results will be
#' items in the queue. \code{n} is the number of items.
#' }
#' }
#'
#' @section Public Active Bindings:
#'
#' \describe{
#' \item{\code{id}}{
#' Read-only property. Returns unique ID of current queue.
#' }
#' \item{\code{lockfile}}{
#' The lock file.
#' }
#' \item{\code{head}}{
#' Integer, total number of items popped, i.e. inactive items.
#' }
#' \item{\code{total}}{
#' Total number of items ever pushed to the queue since last cleaned, integer.
#' }
#' \item{\code{count}}{
#' Integer, read-only, equals to total - head, number of active items in the
#' queue
#' }
#' }
#'
#' @section Private Methods or properties:
#'
#' \describe{
#' \item{\code{.id}}{
#' Don't use directly. Used to store queue ID.
#' }
#' \item{\code{.lockfile}}{
#' Location of lock file.
#' }
#' \item{\code{lock}}{
#' Preserve the file lock.
#' }
#' \item{\code{exclusive(expr,...)}}{
#' Function to make sure the methods are thread-safe
#' }
#' \item{\code{default_get_locker()}}{
#' Default method to lock a queue
#' }
#' \item{\code{default_free_locker}}{
#' Default method to free a queue
#' }
#' }
NULL


not_implemented <- function(msg = 'Not yet implemented', default = 0){
  warning(msg)
  default
}
rand_string <- function(length = 50){
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = '')
}
null_item <- data.frame(
  time = character(0),
  key = character(0),
  hash = character(0),
  message = character(0)
)


#' @rdname AbstractQueue
#' @export
AbstractQueue <- R6::R6Class(
  classname = "AbstractQueue",
  portable = TRUE,
  cloneable = TRUE,
  private = list(
    .id = character(0),

    # Lock file that each queue should have
    # If lock file is locked, then we should wait till the next transaction period
    .lockfile = character(0),
    lock = NULL,

    # Run expr making sure that locker is locked to be exclusive (for write-only)
    exclusive = function(expr, ...) {
      on.exit({
        if(is.function(self$free_locker)){
          self$free_locker()
        }else{
          private$default_free_locker()
        }
      })
      if(is.function(self$get_locker)){
        self$get_locker(...)
      }else{
        private$default_get_locker(...)
      }
      force(expr)
    },

    default_get_locker = function(timeout = 5){
      dipsaus_lock(self$lockfile, timeout = timeout)
    },
    default_free_locker = function(){
      dipsaus_unlock(self$lockfile)
    }

  ),
  public = list(

    # By default, queue uses file locker, if you have customized locker, please
    # implement these two methods as functions:
    #   get_locker obtain and lock access (exclusive)
    #   free_locker free the lock
    # private$exclusive will take care the rest
    get_locker = NULL,
    free_locker = NULL,

    # Get head so that we know where we are in the queue
    #   @get_head should return a integer indicating where we are at the queue
    #   @set_head stores that integer
    # param `v` is always non-negative, this is guaranteed
    # Users are not supposed to call these methods directly,
    # they use self$head and self$head<-
    `@get_head` = function(){ not_implemented() },
    `@set_head` = function(v){ not_implemented() },

    # Get total number of items in the queue, similar to @get_head and @set_head
    `@get_total` = function(){ not_implemented() },
    `@set_total` = function(v){ not_implemented() },

    # Increase total, usually this doesn't need to be override, unless you are
    # using files to store total and want to decrease number of file connections
    `@inc_total` = function(n=1){
      self$total <- self$total + n
    },

    # msg will be vector of strings, separated by "|", containing encoded headers
    # 1. time, key, hash, and message, to view what's inside, you can use
    #   self$print_items(stringr::str_split_fixed(msg, '\\|', 4))
    # to decode
    #
    # Make **sure** to return a number as $push() function uses the returned
    # value as indicator of how many items are stored
    # Unless handled elsewhere, usually return length of msg
    `@append_header` = function(msg, ...){
      not_implemented()
      return(length(msg))
    },

    # Defines how to store value. `key` is unique identifier generated from
    # time, queue ID, and value, you can use it as file name
    # value is an arbitrary R object to store. you need to store value somewhere
    # and return a string (hash, or key or whatever) that will be used in
    # restore_value
    # For example,  in rds_queue, I use key as file name and saveRDS(value) to
    # that file. and in `restore_value` I use `hash` to retrive the file name
    # and read the value
    #
    # Make sure return a string, it'll be encoded and stored as `hash`
    `@store_value` = function(value, key){
      not_implemented()
    },

    # hash is the string returned by `@store_value`, and
    # key is the same as key in `@store_value`
    # preserve is a indicator of whether to preserve the value for future use
    # or remove the value to free memory/disk space
    restore_value = function(hash, key, preserve = FALSE){
      not_implemented()
    },

    # Fixed usage, don't override unless you know what's inside
    push = function(value, message = '', ...){
      time <- safe_urlencode(microtime())

      digest_val <- digest::digest(message)

      key <- digest::digest(list(self$id, time, digest_val))

      hash <- safe_urlencode(self$`@store_value`(value, key))

      message <- safe_urlencode(message)
      if(length(hash) != 1){
        cat2('store_value returns hash value that has length != 1', level = 'FATAL')
      }
      out <- paste( time, key, hash, message, sep = "|" )
      private$exclusive({
        n <- self$`@append_header`(msg = out, ...)
        if( n > 0 ){
          self$`@inc_total`( n )
        }
      })
    },

    # decode headers and return a data.frame
    # items should be a nx4 matrix. Easiest example is the matrix returned by
    # `log()`
    print_items = function(items){
      # Take the results from log() and translate into a data.frame with time, key, hash, and message
      do.call('rbind', apply(items, 1, function(item){
        as.data.frame(self$print_item(item), stringsAsFactors = FALSE)
      }))
    },

    # Print single item, similar to `print_items`, returns a list
    print_item = function(item){
      list(
        time = safe_urldecode(item[[1]]),
        key = item[[2]],
        hash = safe_urldecode(item[[3]]),
        message = safe_urldecode(item[[4]])
      )
    },

    # List n items in the queue. if n <= 0, then list all
    # value will not be obtained during the process,
    # only time, key, hash, and message will be returned, as obtaining value
    # is usually much heavier. However, you can use
    # self$restore_value(hash, key, preserve=TRUE) to
    # obtain the value. The value is not always available though.
    list = function(n = -1){
      out <- self$log(n=n, all=FALSE)
      if( !length(out) ){ return(null_item) }
      if( !is.matrix(out) && !is.data.frame(out) ){
        cat2('list must return a matrix or a data.frame', level = 'FATAL')
      }
      nrows <- nrow(out)
      if(!nrows){ return( null_item ) }
      out <- lapply(seq_len(nrows), function(ii){
        re <- self$print_item(out[ii, ])
        as.data.frame(re, stringsAsFactors=FALSE)
      })
      do.call('rbind', out)
    },

    # pop first n items from queue, `preserve` will be passed to `restore_value`
    # Don't override unless you know what's inside
    pop = function(n = 1, preserve = FALSE) {
      private$exclusive({
        # Check count first, in this case, we don't read header file
        count <- self$count
        if(count < 0.5){ return(list()) }
        out <- self$`@log`(n = n)
        if( !length(out) ){ return(list()) }
        if( !is.matrix(out) && !is.data.frame(out) ){
          cat2('list must return a matrix or a data.frame', level = 'FATAL')
        }
        nrows <- nrow(out)
        if(!nrows){ return( list() ) }
        # parse time, key, hash
        out <- lapply(seq_len(nrows), function(ii){
          re <- self$print_item(out[ii, ])
          re$value <- self$restore_value( re$hash, re$key, preserve = preserve )
          re
        })

        self$head <- self$head + nrows
        out
      })
    },

    # get n items from what you saved to during `@append_header`. n<=0 means
    # list all possible items.
    # If all=TRUE, return all items (#items=self$total), including popped items
    # If all=FALSE, only return items in the queue
    # The returned value should be a nx4 matrix
    # I use stringr::str_split_fixed(..., '\\|', 4) in all queues implemented
    `@log` = function(n = -1, all = FALSE){
      not_implemented()
    },
    # log with locks (thread-safe)
    log = function(n=-1, all=FALSE){
      private$exclusive({
        self$`@log`(n=n, all=all)
      })
    },

    # Remove all items and reset head=total=0
    `@reset` = function(...) {
      not_implemented()
    },
    # thread-safe version
    reset = function(...) {
      private$exclusive({
        self$`@reset`(...)
      })
    },


    # clean all popped items. Usually you don't have to do this manually as
    # pop(..., preserve=FALSE) will clean automatically (except for `text_queue`)
    `@clean` = function(...) {
      not_implemented()
    },
    # thread-safe version
    clean = function(...){
      private$exclusive({
        self$`@clean`(...)
      })
    },

    # check the validity of queue. Usually the followings need to be checked
    # 1. head<=total, and non-negative
    # 2. all the necessary files exist
    # 3. all the connections exist
    `@validate` = function(...) {
      not_implemented()
    },
    validate = function(...){
      private$exclusive({
        self$`@validate`(...)
      })
    },

    # Usually should be called at the end of `initialization` to connect to
    # a database, a folder, or an existing queue
    # you should do checks whether the connection is new or it's an existing
    # queue
    `@connect` = function(con = NULL, ...){
      not_implemented()
    },
    # thread-safe version. sometimes you need to override this function instead
    # of `@connect`, because `private$exclusive` requires lockfile to be locked
    # If you don't have lockers ready, or need to set lockers during the
    # connection, override this one
    connect = function(...){
      private$exclusive({
        self$`@connect`(...)
      })
    },

    # will be called during Class$new(...), three tasks,
    # 1. set `get_locker` `free_locker` if lock type is not a file
    # 2. set lockfile (if using default lockers)
    # 3. call self$connect
    initialize = function(con = NULL, lockfile, ...){
      self$lockfile <- lockfile
      self$connect(con, ...)
    },

    # destroy a queue, free up space
    # and call `delayedAssign('.lockfile', {stop(...)}, assign.env=private)`
    # to raise error if a destroyed queue is called again later.
    destroy = function(){
      private$default_free_locker()
      delayedAssign('.lockfile', {
        cat2("Queue is destroyed", level = 'FATAL')
      }, assign.env=private)
    }
  ),
  active = list(

    # read-only version of self$id. It's safer than private$.id as the latter
    # one does not always exist
    id = function(){
      if(length(private$.id) != 1){
        private$.id <- rand_string()
      }
      private$.id
    },

    # set/get lock file. Don't call private$.lockfile directly
    lockfile = function(v){
      if(!missing(v)){
        private$default_free_locker()
        private$.lockfile <- v
      }else if(!length(private$.lockfile)){
        private$.lockfile <- rand_string()
      }
      private$.lockfile
    },

    # a safe wrapper for `@get_head` and `@set_head`
    head = function(v) {
      if(missing(v)){ return(as.integer(self$`@get_head`())) }
      if( length(v) != 1 ){ cat2('head must be a number',level = 'FATAL') }
      if( !is.numeric(v) || v < 0 ){ cat2('head must be a non-negative integer',
                                          level = 'FATAL') }
      if( v > self$total ){ cat2('head must not exceed total',
                                 level = 'FATAL') }
      self$`@set_head`( v )
    },

    # a safe wrapper for `@get_total` and `@set_total`
    total = function(v){
      if(missing(v)){ return(as.integer(self$`@get_total`())) }
      if( length(v) != 1 ){ cat2('total must be a number', level = 'FATAL') }
      if( !is.numeric(v) || v < 0 ){ cat2('total must be a non-negative integer',
                                          level = 'FATAL') }
      self$`@set_total`( v )
    },

    # How many items in the queue right now, = total - head
    count = function(){
      tryCatch({
        self$total - self$head
      }, error = function(e){
        warning('Cannot get count, return 0')
        0
      })
    }
  )
)



