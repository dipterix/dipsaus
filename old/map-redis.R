
RedisMap <- R6::R6Class(
  classname = 'RedisMap',
  inherit = AbstractMap,
  portable = TRUE,
  cloneable = TRUE,
  private = list(

    redis_id = character(0),
    redis = NULL

  ),
  public = list(
    has_locker = FALSE,
    missing_default = NULL,

    `@remove` = function(keys){
      not_implemented()
    },
    remove = function(keys){
      lapply(keys, function(key){
        enkey <- safe_urlencode(key)
        private$redis$hdel(sprintf('%s-signature', private$redis_id), enkey)
        private$redis$hdel(private$redis_id, enkey)
      })
    },
    reset = function(...){
      private$redis$exec(sprintf('DEL %s', private$redis_id))
      private$redis$exec(sprintf('DEL %s-signature', private$redis_id))
    },

    keys = function(include_signatures = FALSE){

      if( !include_signatures ){
        keys <- private$redis$hkeys(sprintf('%s-signature', private$redis_id))
        if(!length(keys)){ return(NULL) }
        keys <- sapply(keys, safe_urldecode)
      }else{
        keys <- private$redis$hgetall(sprintf('%s-signature', private$redis_id))
        if(!length(keys)){ return(NULL) }
        nms <- names(keys)
        keys <- t(sapply(seq_along(keys), function(ii){
          c(safe_urldecode(nms[[ii]]), keys[[ii]][[1]])
        }))
      }

      keys
    },

    size = function(){
      private$redis$hlen(sprintf('%s-signature', private$redis_id))
    },

    digest = function(signature){
      digest::digest(signature)
    },



    has = function(keys, signature, sig_encoded = FALSE){
      stopifnot2(is.character(keys) || is.null(keys), msg = '`keys` must be a character vector or NULL')
      if( self$size() == 0 ){
        return(rep(FALSE, length(keys)))
      }
      has_sig <- !missing(signature)

      if( !sig_encoded && has_sig ){
        signature <- self$digest(signature)
      }

      sig_key <- sprintf('%s-signature', private$redis_id)

      vapply(keys, function(k){
        enkey <- safe_urlencode(k)
        has_key <- private$redis$hexists(sig_key, enkey) > 0
        if( has_key && has_sig ){
          sig <- private$redis$hget(sig_key, enkey)
          has_key <- isTRUE(sig[[1]] == signature)
        }
        has_key
      }, FUN.VALUE = FALSE)
    },


    `@set` = function(key, value, signature){
      not_implemented()
    },
    set = function(key, value, signature){
      force(value)
      if( missing(signature) ){
        signature <- self$digest( value )
      }else{
        signature <- self$digest( signature )
      }

      key <- safe_urlencode(key)

      private$redis$hset(private$redis_id, key, value)
      private$redis$hset(sprintf('%s-signature', private$redis_id), key, signature)

      invisible(signature)
    },


    get = function(key, missing_default){
      if(missing(missing_default)){ missing_default <- self$missing_default }

      enkey <- safe_urlencode(key)

      if(private$redis$hexists(private$redis_id, enkey)){
        private$redis$hget(private$redis_id, enkey)
      }else{
        missing_default
      }
    },

    mget = function(keys, missing_default){
      if(missing(missing_default)){ missing_default <- self$missing_default }

      sapply(keys, self$get, simplify = FALSE, USE.NAMES = TRUE)
    },



    as_list = function(sort = FALSE){
      if(self$size() == 0){
        return(list())
      }
      re <- private$redis$hgetall(private$redis_id)
      names(re) <- sapply(names(re), safe_urldecode)
      if(sort){
        ord <- order(names(re))
        re <- re[ord]
      }
      re
    },

    validate = function(...){
      stopifnot2(private$valid, msg = 'Map is invalid/destroyed!')
    },

    initialize = function(map_id){
      if( !requireNamespace('RcppRedis') ){
        cat2('RcppRedis is not installed. Please download, install, and launch Redis, then\n  ',
             'install.packages("RcppRedis")', level = 'FATAL')
      }
      map_id <- paste0('MAP', map_id)
      tryCatch({
        private$redis <- new( RcppRedis::Redis )
      }, error = function(e){
        cat2('Cannot connect to Redis. Please make sure Redis is installed. \n',
             '  MacOS:\n', '\tInstall: \tbrew install redis\n', '\tTo Start: \tbrew services start redis\n',
             '  Linux:\n', '\tInstall: \tsudo apt-get install redis-server\n',
             '\tTo Start: \tsudo systemctl enable redis-server.service\n',
             '  Windows:\n', '\tCheck: https://github.com/dmajkic/redis/downloads', level = 'FATAL')
      })


      private$redis_id <- map_id
    },

    # destroy a queue, free up space
    # and call `delayedAssign('.lockfile', {stop(...)}, assign.env=private)`
    # to raise error if a destroyed queue is called again later.
    destroy = function(){
      private$valid <- FALSE
      delayedAssign('redis', { cat2("Map is destroyed", level = 'FATAL') }, assign.env=private)
      delayedAssign('redis_id', { cat2("Map is destroyed", level = 'FATAL') }, assign.env=private)
    }
  )
)
