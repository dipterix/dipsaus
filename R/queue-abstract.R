not_implemented <- function(msg = 'Not yet implemented', default = 0){
  warning(msg)
  default
}
rand_string = function(length = 50){
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = '')
}
null_item = data.frame(
  time = character(0),
  key = character(0),
  hash = character(0)
)

#' Defines abstract queue class
#'
#' This class is inspired by \url{https://cran.r-project.org/package=txtq}. The
#' difference is \code{AbstractQueue} introduce an abstract class that can be
#' extended.
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

    default_get_locker = function(time_out = Inf, intervals = 10){
      if( time_out <= 0 ){
        stop('Cannot get locker, timeout!', call. = FALSE)
      }
      # Locker always fails in mac, so lock the file is not enough
      locker_owner = readLines(self$lockfile)
      if(length(locker_owner) == 1 && locker_owner != '' && !isTRUE(locker_owner == self$id)){
        Sys.sleep(intervals / 1000)
        return(private$default_get_locker(time_out - intervals, intervals))
      }
      # Lock the file, exclude all others
      private$lock <- filelock::lock(self$lockfile, timeout = time_out)

      # write ID
      write(self$id, self$lockfile, append = FALSE)
    },
    default_free_locker = function(){
      on.exit({
        if( !is.null(private$lock) ){
          filelock::unlock(private$lock)
          private$lock = NULL
        }
      })
      if( !is.null(private$lock) ){
        write('', self$lockfile, append = FALSE)
      }
    }

  ),
  public = list(

    get_locker = NULL,
    free_locker = NULL,

    # Get head so that we know where we are in the queue
    `@get_head` = function(){ not_implemented() },
    `@set_head` = function(v){ not_implemented() },

    # Get total number of items in the queue
    `@get_total` = function(){ not_implemented() },
    `@set_total` = function(v){ not_implemented() },

    `@inc_total` = function(n=1){
      self$total = self$total + n
    },

    `@append_header` = function(msg, ...){
      not_implemented()
    },

    `@store_value` = function(value, key){
      as.character(value)
    },
    restore_value = function(hash, key, preserve = FALSE){
      value
    },


    push = function(value, ...){
      time <- base64url::base64_urlencode(microtime())

      digest_val = digest::digest(value)

      key = digest::digest(list(self$id, time, digest_val))

      hash = base64url::base64_urlencode(self$`@store_value`(value, key))
      if(length(hash) != 1){
        stop('store_value returns hash value that has length != 1')
      }
      out <- paste( time, key, hash, sep = "|" )
      private$exclusive({
        n = self$`@append_header`(msg = out, ...)
        if( n > 0 ){
          self$`@inc_total`( n )
        }
      })
    },

    print_item = function(item){
      list(
        time = base64url::base64_urldecode(item[[1]]),
        key = item[[2]],
        hash = base64url::base64_urldecode(item[[3]])
      )
    },

    list = function(n = -1){
      out = self$log(n=n)
      if( !length(out) ){ return(null_item) }
      if( !is.matrix(out) && !is.data.frame(out) ){
        stop('list must return a matrix or a data.frame')
      }
      nrows = nrow(out)
      if(!nrows){ return( null_item ) }
      out = lapply(seq_len(nrows), function(ii){
        re = self$print_item(out[ii, ])
        as.data.frame(re, stringsAsFactors=FALSE)
      })
      do.call('rbind', out)
    },

    # pop first n items from queue, if preserve, don't remove the cache file for
    pop = function(n = 1, preserve = FALSE) {
      private$exclusive({
        out <- self$log(n = n)
        if( !length(out) ){ return(list()) }
        if( !is.matrix(out) && !is.data.frame(out) ){
          stop('list must return a matrix or a data.frame')
        }
        nrows = nrow(out)
        if(!nrows){ return( list() ) }
        # parse time, key, hash
        out = lapply(seq_len(nrows), function(ii){
          re = self$print_item(out[ii, ])
          re$value = self$restore_value( re$hash, re$key, preserve = preserve )
          re
        })

        self$head <- self$head + nrows
        out
      })
    },

    `@log` = function(n = -1){
      not_implemented()
    },
    log = function(n=-1){
      private$exclusive({
        self$`@log`(n=n)
      })
    },

    `@reset` = function(...) {
      not_implemented()
    },
    reset = function(...) {
      private$exclusive({
        self$`@reset`(...)
      })
    },


    `@clean` = function(...) {
      not_implemented()
    },
    clean = function(...){
      private$exclusive({
        self$`@clean`(...)
      })
    },


    `@validate` = function(...) {
      not_implemented()
    },
    validate = function(...){
      private$exclusive({
        self$`@validate`(...)
      })
    },

    `@connect` = function(con = NULL, ...){
      not_implemented()
    },
    connect = function(...){
      private$exclusive({
        self$`@connect`(...)
      })
    },

    initialize = function(con = NULL, lockfile, ...){
      self$lockfile = lockfile
      self$connect(con, ...)
    },

    destroy = function(){
      not_implemented()
    }
  ),
  active = list(
    id = function(){
      if(length(private$.id) != 1){
        private$.id = rand_string()
      }
      private$.id
    },
    lockfile = function(v){
      if(!missing(v)){
        private$default_free_locker()
        private$.lockfile = v
      }else if(!length(private$.lockfile)){
        private$.lockfile = tempfile(pattern = 'locker')
      }
      file_create(private$.lockfile)
      private$.lockfile
    },
    head = function(v) {
      if(missing(v)){ return(as.integer(self$`@get_head`())) }
      if( length(v) != 1 ){ stop('head must be a number') }
      if( !is.numeric(v) || v < 0 ){ stop('head must be a non-negative integer') }
      if( v > self$total ){ stop('head must not exceed total') }
      private$exclusive({
        self$`@set_head`( v )
      })
    },
    total = function(v){
      if(missing(v)){ return(as.integer(self$`@get_total`())) }
      if( length(v) != 1 ){ stop('total must be a number') }
      if( !is.numeric(v) || v < 0 ){ stop('total must be a non-negative integer') }
      private$exclusive({
        self$`@set_total`( v )
      })
    },
    count = function(){
      tryCatch({
        private$exclusive({
          self$total - self$head
        })
      }, error = function(e){
        warning('Cannot get count, return 0')
        0
      })
    }
  )
)





