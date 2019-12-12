RedisQueue <- R6::R6Class(
  classname = 'RedisQueue',
  inherit = AbstractQueue,
  private = list(
    redis = NULL,
    redis_id = 'dipsaus'

  ),
  public = list(

    # Get head so that we know where we are in the queue
    `@get_head` = function(){
      private$redis$hget(private$redis_id, 'HEAD')
    },
    `@set_head` = function(v){
      private$redis$hset(private$redis_id, 'HEAD', v)
    },

    # Get total number of items in the queue
    `@get_total` = function(){
      private$redis$hget(private$redis_id, 'TOTAL')
    },
    `@set_total` = function(v){
      private$redis$hset(private$redis_id, 'TOTAL', v)
    },

    `@append_header` = function(msg, ...){
      header_key <- sprintf('%s__HEADERS', private$redis_id)
      lapply(msg, function(m){
        private$redis$exec(sprintf('RPUSH %s "%s"', header_key, as.character(m)))
      })
      return(length(msg))
    },

    `@store_value` = function(value, key){
      private$redis$hset(private$redis_id, key, value)
      key
    },
    restore_value = function(hash, key, preserve = FALSE){
      re <- private$redis$hget(private$redis_id, key)
      if( !preserve ){
        private$redis$hdel(private$redis_id, key)
      }
      re
    },

    `@log` = function(n = -1, all = FALSE){
      header_key <- sprintf('%s__HEADERS', private$redis_id)
      if( all ){
        head <- 0
      }else{
        head <- self$head
      }
      total <- self$total
      count <- total - head
      if( n <= 0 ){ n <- count }else{ n <- min(n, count) }
      if( n == 0 ){ return() }

      header <- private$redis$exec(sprintf('LRANGE %s %d %d', header_key, head, head + n-1))
      header <- unlist(header)
      header <- stringr::str_remove_all(header, '(^")|("$)')
      stringr::str_split_fixed(header, '\\|', 4)
    },

    `@reset` = function() {
      private$redis$exec(sprintf('DEL %s', private$redis_id))
      private$redis$exec(sprintf('DEL %s__HEADERS', private$redis_id))
      self$`@set_head`(0)
      self$`@set_total`(0)
    },

    `@clean` = function(...) {
      head <- self$head
      total <- self$total
      if( total - head < 1 ){
        self$`@reset`()
      }else{
        header_key <- sprintf('%s__HEADERS', private$redis_id)
        if( head > 0 ){
          lapply(seq_len(head), function(ii){
            header <- private$redis$exec(sprintf('LPOP %s', header_key))
            header <- stringr::str_remove_all(header, '(^")|("$)')
            header <- self$print_item(stringr::str_split_fixed(header, '\\|', n = 4))
            private$redis$hdel(private$redis_id, header$hash)
          })
        }
        self$head <- 0
        self$total <- total - head
      }
    },
    `@validate` = function() {

    },

    connect = function(queue_id){

      # Check if total and head exists
      if( private$redis$hexists(private$redis_id, 'HEAD') == 0 ){
        self$`@set_head`(0)
      }

      if( private$redis$hexists(private$redis_id, 'TOTAL') == 0 ){
        self$`@set_total`(0)
      }

    },

    initialize = function(queue_id = rand_string()){
      if( !requireNamespace('RcppRedis') ){
        cat2('RcppRedis is not installed. Please download, install, ',
             'and launch Redis, then\n  ',
             'install.packages("RcppRedis")', level = 'FATAL')
      }
      queue_id <- paste0('QUEUE', queue_id)
      tryCatch({
        private$redis <- new( RcppRedis::Redis )
      }, error = function(e){
        cat2('Cannot connect to Redis. Please make sure Redis is installed. \n',
             '  MacOS:\n', '\tInstall: \tbrew install redis\n', '\tTo Start: \tbrew services start redis\n',
             '  Linux:\n', '\tInstall: \tsudo apt-get install redis-server\n',
             '\tTo Start: \tsudo systemctl enable redis-server.service\n',
             '  Windows:\n', '\tCheck: https://github.com/dmajkic/redis/downloads')
      }, level = 'FATAL')


      private$redis_id <- queue_id
      self$get_locker <- function(time_out = Inf, intervals = 10){
        if( time_out <= 0 ){
          cat2('Cannot get locker, timeout!', level = 'FATAL')
        }
        # Locker always fails in mac, so lock the file is not enough
        if( private$redis$hexists(private$redis_id, 'LOCK') != 0 ){
          locker_owner <- private$redis$hget(private$redis_id, 'LOCK')
          if(length(locker_owner) == 1 && locker_owner != '' && !isTRUE(locker_owner == self$id)){
            Sys.sleep(intervals / 1000)
            return(self$get_locker(time_out - intervals, intervals))
          }
        }
        # Lock the file, exclude all others
        # write ID
        private$redis$hset(private$redis_id, 'LOCK', self$id)
      }
      self$free_locker <- function(){
        private$redis$hset(private$redis_id, 'LOCK', NULL)
      }
      self$connect(queue_id)
    },

    destroy = function(){
      private$redis$exec(sprintf('DEL %s__HEADERS', private$redis_id))
      private$redis$exec(sprintf('DEL %s', private$redis_id))
      delayedAssign('redis', {cat2('Queue destroyed.', level = 'FATAL')}, assign.env = private)
      invisible()
    }
  ),
  active = list(
    lockfile = function(v){
      cat2("Using Redis backend, in-memory lock", level = 'FATAL')
    }
  )
)
