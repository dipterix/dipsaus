SessionQueue <- R6::R6Class(
  classname = 'SessionQueue',
  inherit = AbstractQueue,
  private = list(
    .map = NULL
  ),
  public = list(
    # Get head so that we know where we are in the queue
    `@get_head` = function(){ private$.map$get('head') },
    `@set_head` = function(v){
      private$.map$set('head', v)
    },

    # Get total number of items in the queue
    `@get_total` = function(){ private$.map$get('total') },
    `@set_total` = function(v){
      private$.map$set('total', v)
    },

    `@append_header` = function(msg, ...){
      msg <- as.list(msg)
      if(!length(msg)){ return(0) }
      total <- self$total
      names(msg) <- paste0('HEADER', total + seq_along(msg))
      private$.map$mset(.list = msg)
      return(length(msg))
    },

    `@store_value` = function(value, key){
      private$.map$set(key, value)
      key
    },
    restore_value = function(hash, key, preserve = FALSE){
      # in this case, hash = key
      re <- private$.map$get(hash)
      if(!preserve){
        private$.map$remove(hash)
      }
      re
    },

    `@log` = function(n = -1, all = FALSE){
      if( all ){ head <- 0 }else{ head <- self$head }
      total <- self$total
      count <- total - head
      if( n <= 0 ){ n <- count }else{ n <- min(n, count) }
      if( n == 0 ){ return() }
      re <- unlist(lapply(head + seq_len(n), function(ii){
        private$.map$get(paste0('HEADER', ii))
      }))
      stringr::str_split_fixed(re, '\\|', 4)
    },

    `@reset` = function() {
      total <- private$.map$get('total', 0)
      if(total > 0){
        lapply(seq_len(total), function(ii){
          msg <- private$.map$get(paste0('HEADER', ii))
          re <- self$print_item(stringr::str_split_fixed(msg, '\\|', n = 4))
          private$.map$remove(re$hash)
          private$.map$remove(paste0('HEADER', ii))
        })
      }
      private$.map$set('head', 0)
      private$.map$set('total', 0)
    },

    `@clean` = function(...) {
      head <- self$head
      total <- self$total
      if( total - head < 1 ){
        self$`@reset`()
      }else{
        if( head > 0 ){
          lapply(seq_len(head), function(ii){
            msg <- private$.map$get(paste0('HEADER', ii))
            re <- self$print_item(stringr::str_split_fixed(msg, '\\|', n = 4))
            private$.map$remove(re$hash)
            private$.map$remove(paste0('HEADER', ii))
          })
          if( head < total ){
            lapply(seq(1, total - head), function(ii){
              msg <- private$.map$get(paste0('HEADER', ii + head))
              private$.map$set(paste0('HEADER', ii), msg)
            })
          }
        }

        private$.map$set('head', 0)
        private$.map$set('total', total - head)
      }
    },

    `@validate` = function() {
      if( self$count < 0 ){
        cat2('Negative items in the queue', level = 'FATAL')
      }

      total <- private$.map$get('total')
      head <- private$.map$get('head')
      if( total > head ){
        lapply(seq(head+1, total), function(ii){
          stopifnot(private$.map$has(paste0('HEADER', ii)))
        })
      }
    },

    connect = function(map, ...){

      # check head
      has_head <- map$has('head')
      head <- map$get('head')
      if( length(head) != 1 || !is.numeric(head) || head <= 0 ){
        head <- 0
        has_head <- FALSE
      }

      # check total
      has_total <- map$has('total')
      total <- map$get('total')
      if( length(total) != 1 || !is.numeric(total) || head > total ){
        total <- 0
        has_total <- FALSE
      }

      # check lockfile
      has_lockfile <- map$has('lockfile')
      if( has_lockfile ){
        lockfile <- map$get('lockfile')
        if( !(length( lockfile ) == 1 && is.character( lockfile ) ) ){
          has_lockfile <- FALSE
        }
      }
      if( !has_lockfile ){
        lockfile <- rand_string()
      }

      private$.map <- map
      self$lockfile <- lockfile

      if(!all(has_lockfile, has_head, has_total)){
        map$set(key = 'lockfile', lockfile)
        self$reset()
      }else{
        self$validate()
      }
    },

    initialize = function(map){
      nms <- c('reset','set','mset','get','mget','has','remove','keys','size','as_list')
      if(!is.list(map) || !all(nms %in% names(map))){
        cat2('Please use fastmap::fastmap() as input to avoid memory leak', level = 'FATAL')
      }
      self$connect(map)
    },
    destroy = function(){
      private$.map$reset()
      if(!is.null(private$lock)){
        synchronicity::unlock(private$lock)
      }
      delayedAssign('.lockfile', {cat2('Queue destroyed', level = 'FATAL')}, assign.env = private)
      delayedAssign('.map', {cat2('Queue destroyed', level = 'FATAL')}, assign.env = private)
    }
  )
)
