
SessionMap <- R6::R6Class(
  classname = 'SessionMap',
  inherit = AbstractMap,
  portable = TRUE,
  cloneable = TRUE,
  public = list(

    has_locker = FALSE,

    `@remove` = function(keys){
      private$map$remove(keys)
    },
    reset = function(...){
      private$map$reset()
    },
    keys = function(include_signatures = FALSE){
      keys <- private$map$keys()
      if( include_signatures ){
        keys <- t(sapply(keys, function(k){
          c(k, private$map$get(k)$signature)
        }))
      }
      keys
    },
    size = function(){
      private$map$size()
    },

    `@set` = function(key, value, signature){
      private$map$set(key = key, value = list(
        signature = signature,
        value = value
      ))
      return( signature )
    },

    `@get` = function(key){
      return( private$map$get(key)$value )
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
    validate = function(...){
      stopifnot2(private$valid, msg = 'Map is invalid/destroyed!')
    },

    initialize = function( map = fastmap::fastmap() ){
      private$map <- map
    },

    destroy = function(){
      private$valid <- FALSE
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
      NULL
    },

    is_valid = function(){
      private$valid
    }

  )
)
