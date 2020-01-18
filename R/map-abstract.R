
#' Abstract Map to store key-value pairs
AbstractMap <- R6::R6Class(
  classname = 'AbstractMap',
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
      stopifnot2(private$valid, msg = 'Map is not valid')
      custom_locker <- is.function(self$get_locker) && is.function(self$free_locker)
      if(self$has_locker){
        on.exit({
          if(custom_locker){
            self$free_locker()
          }else{
            private$default_free_locker()
          }
        })
        if(custom_locker){
          self$get_locker(...)
        }else{
          private$default_get_locker(...)
        }
      }

      force(expr)
    },

    default_get_locker = function(timeout = 10){

      dipsaus_lock(self$lockfile, timeout = timeout)

    },
    default_free_locker = function(){
      dipsaus_unlock(self$lockfile)
    },

    map = NULL,
    valid = TRUE

  ),
  public = list(

    # By default, queue uses file locker, if you have customized locker, please
    # implement these two methods as functions:
    #   get_locker obtain and lock access (exclusive)
    #   free_locker free the lock
    # private$exclusive will take care the rest
    get_locker = NULL,
    free_locker = NULL,
    has_locker = TRUE,
    missing_default = NULL,



    `@remove` = function(keys){
      not_implemented()
      private$map$remove(keys)
    },
    remove = function(keys){
      private$exclusive({
        self$`@remove`( keys )
      })
    },
    reset = function(...){
      keys <- self$keys(include_signatures = FALSE)
      self$remove( keys )
    },



    keys = function(include_signatures = FALSE){
      not_implemented()

      keys <- private$map$keys()
      if( include_signatures ){
        # Returns two columns: key digest

        keys <- t(sapply(keys, function(k){
          c(k, private$map$get(k)$signature)
        }))
      }

      keys
    },

    size = function(){
      length(self$keys( include_signatures = FALSE ))
    },


    digest = function(signature){
      digest::digest(signature)
    },



    has = function(keys, signature, sig_encoded = FALSE){
      stopifnot2(is.character(keys) || is.null(keys), msg = '`keys` must be a character vector or NULL')
      all_keys <- self$keys(include_signatures = TRUE)

      if(!length(all_keys)){ return(rep(FALSE, length(keys))) }

      has_sig <- !missing(signature)

      if( !sig_encoded && has_sig ){
        signature <- self$digest(signature)
      }

      vapply(keys, function(k){
        sel <- all_keys[,1] == k
        has_key <- any(sel)
        if( has_sig && has_key ){ has_key <- all_keys[sel, 2] == signature }
        has_key
      }, FUN.VALUE = FALSE)
    },




    `@set` = function(key, value, signature){
      not_implemented()
      private$map$set(key = key, value = list(
        signature = signature,
        value = value
      ))
      return( signature )
    },
    set = function(key, value, signature){
      force(value)
      if( missing(signature) ){
        signature <- self$digest( value )
      }else{
        signature <- self$digest( signature )
      }
      private$exclusive({
        self$`@set`(key, value, signature = signature)
      })
      invisible(signature)
    },

    mset = function(..., .list = NULL){
      .list <- c(list(...), .list)
      nms <- names(.list)
      lapply(nms, function(nm){
        self$set(nm, .list[[nm]])
      })
    },



    `@get` = function(key){
      not_implemented()
      return( private$map$get(key)$value )
    },
    get = function(key, missing_default){
      if(missing(missing_default)){ missing_default <- self$missing_default }
      if( self$has( key ) ){
        self$`@get`(key)
      }else{
        missing_default
      }
    },

    mget = function(keys, missing_default){
      if(missing(missing_default)){ missing_default <- self$missing_default }

      has_keys <- self$has( keys )

      re <- lapply(seq_along( keys ), function(ii){
        if( has_keys[[ii]] ){
          self$`@get`(keys[[ ii ]])
        }else{
          missing_default
        }
      })
      names(re) <- keys
      re
    },



    as_list = function(sort = FALSE){
      keys <- self$keys(include_signatures = FALSE)
      if(!length(keys)){
        return(list())
      }
      if( sort ){
        keys <- sort(keys)
      }

      self$mget(keys)
    },

    `@validate` = function(...){
      not_implemented()
    },
    validate = function(...){
      stopifnot2(private$valid, msg = 'Map is invalid/destroyed!')
      private$exclusive({
        self$`@validate`(...)
      })
    },

    # Usually should be called at the end of `initialization` to connect to
    # a database, a folder, or an existing queue
    # you should do checks whether the connection is new or it's an existing
    # queue
    `@connect` = function(...){
      not_implemented()
      private$map <- fastmap::fastmap()
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
    initialize = function(has_locker = TRUE, lockfile, ...){
      if( has_locker ){
        self$lockfile <- lockfile
      }
      self$connect(...)
    },

    # destroy a queue, free up space
    # and call `delayedAssign('.lockfile', {stop(...)}, assign.env=private)`
    # to raise error if a destroyed queue is called again later.
    destroy = function(){
      private$default_free_locker()
      private$valid <- FALSE
      delayedAssign('.lockfile', { cat2("Map is destroyed", level = 'FATAL') }, assign.env=private)
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

    is_valid = function(){
      private$valid
    }

  )
)
