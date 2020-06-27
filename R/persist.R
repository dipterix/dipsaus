#' @title Wrapper to cache key-value pairs and persist across sessions
#'
#' @description This class is designed to persist arbitrary R objects locally
#' and share across different sessions. The container consists two-level caches.
#' The first one is session-based, meaning it's only valid under current R
#' session and will be cleared once the session is shut down. The second is
#' the persist-level map, which will persist to hard drive and shared across
#' sessions. See \code{cache} method in 'details'.
#'
#' @seealso \code{\link[dipsaus]{map}}
#'
#' @name PersistContainer
#'
#' @section Public Methods:
#'
#' \describe{
#' \item{\code{initialize(..., backend = rds_map)}}{
#' The constructor. backend must inherit \code{AbstractMap}, \code{...} will
#' be passed to \code{backend$new(...)}. To check available back-ends and their
#' use cases, see \code{\link[dipsaus]{map}}.
#' }
#' \item{\code{reset(all = FALSE)}}{
#' Reset container. If all is set to be true, then reset session-based and
#' hard-drive-based, otherwise only reset session-based container.
#' }
#' \item{\code{destroy(all = FALSE)}}{
#' destroy the container. Only use it when you want to finalize the container in
#' \code{\link{reg.finalizer}}.
#' }
#' \item{\code{has(key, signature = NULL)}}{
#' returns a list of true/false (logical) vectors indicating whether keys exist
#' in the container, if signature is used when caching the key-value pairs, then
#' it also checks whether signature matches. This is very important as even if
#' the keys match but signature is wrong, the results will be false.
#' }
#' \item{\code{remove(keys, all = TRUE)}}{
#' Remove keys in the container. Default is to remove the keys in both levels.
#' If \code{all=FALSE}, then only remove the key in current session
#' }
#' \item{\code{cache(key, value, signature = NULL, replace = FALSE, persist = FALSE)}}{
#' \code{key} and \code{signature} together form the unique identifier for the
#' value. By default \code{signature} is none, but it's very useful when value
#' if large, or \code{key} is not a string. \code{replace} indicates whether
#' to force replace the key-value pairs even if the entry exists. If
#' \code{persist} is true, then the value is stored in hard-disks, otherwise
#' the value will be deleted once the session is closed.
#' }
#' }
#' @examples
#'
#' container = PersistContainer$new(tempfile())
#'
#' # Reset the container so that values are cleared
#' container$reset(all = TRUE)
#'
#' # Store `1` to 'a' with signature 111 to a non-persist map
#' # returns 1
#' container$cache(key = 'a', value = 1, signature = 111, persist = FALSE)
#'
#' # Replace 'a' with 3
#' # returns 3
#' container$cache(key = 'a', value = 3, signature = 111,
#'                 persist = TRUE, replace = TRUE)
#'
#' # check if 'a' exists with signature 111
#' container$has('a', signature = 111)    # TRUE
#' # When you only have 'a' but no signature
#' container$has('a')                     # TRUE
#' # check if 'a' exists with wrong signature 222
#' container$has('a', signature = 222)    # FALSE
#'
#'
#' # Store 'a' with 2 with same signature
#' # will fail and ignore the value (value will not be evaluated if signatured)
#' # Return 2 (Important! use cached values)
#' container$cache(key = 'a', value = {
#'   print(123)
#'   return(2)
#' }, signature = 111, replace = FALSE)
#'
#' # When no signature is present
#' # If the key exists (no signature provided), return stored value
#' # returns 3
#' container$cache(key = 'a', value = 4)
#'
#' # replace is TRUE (no signature provided), signature will be some default value
#' container$cache(key = 'a', value = 2, replace = TRUE)
#'
#' # destroy the container to free disk space
#' container$destroy()
NULL

#' @rdname PersistContainer
#' @export
PersistContainer <- R6::R6Class(
  classname = 'PersistContainer',
  private = list(
    map = NULL,
    local_map = NULL
  ),
  public = list(

    initialize = function(..., backend = rds_map){
      args <- list(...)
      m <- tryCatch({
        do.call(backend, args)
      }, error = function(e){
        do.call(qs_map, args)
      })

      private$map <- m
      private$local_map <- session_map()
    },

    reset = function(all = FALSE){
      try({ private$local_map$reset() })
      if(all){
        try({ private$map$reset() })
      }
      invisible()
    },
    destroy = function(all = FALSE){
      try({ private$local_map$destroy() })
      if(all){
        try({ private$map$destroy() })
      }
      invisible()
    },

    has = function(key, signature, sig_encoded = FALSE){
      if(!length(key)){ return(NULL) }

      mis_sig <- missing(signature)

      re <- tryCatch({
        if( mis_sig ){
          private$local_map$has(key) | private$map$has(key)
        }else{
          private$local_map$has(key, signature, sig_encoded) | private$map$has(key, signature, sig_encoded)
        }
      }, error = function(e){
        rep(FALSE, length(key))
      })
      return( re )
    },

    remove = function(keys, all = TRUE){
      try({ private$local_map$remove(keys) })
      if( all ){
        try({ private$map$remove(keys) })
      }
      invisible()
    },

    cache = function(key, value, signature, replace = FALSE, persist = FALSE){

      map <- private$map
      local_map <- private$local_map
      has_sig <- !missing(signature)

      save_item <- function(force_value = TRUE){
        try({

          self$remove(key, all = FALSE)

          if(force_value) force(value)

          if( persist ){
            mp <- map

            if( inherits(map, 'TextMap' ) ){
              # check value is basic types
              if(!(is.atomic( value ) || is.list( value ) || is.data.frame( value ))){
                cat2('value must be atomic, list, or data.frame if you want to persist while using TextMap.\n  ',
                     'Switch to non-persist mode', level = 'WARNING')
                mp <- local_map
              }
            }

          }else{
            mp <- local_map
          }

          if(has_sig){
            mp$set(key, value, signature = signature)
          }else{
            mp$set(key, value, signature = .missing_arg[[1]])
          }

        })


        return(value)

      }
      if( replace ){
        return(save_item())
      }

      # In case the map is corrupted
      try({
        # Search for local_map
        if(has_sig){
          if( local_map$has(keys = key, signature = signature, sig_encoded = FALSE) ){
            return(local_map$get(key = key))
          }else if( map$has(keys = key, signature = signature, sig_encoded = FALSE) ){
            return(map$get(key = key))
          }else{
            return(save_item())
          }
        }else{

          if( local_map$has(keys = key) ){
            return(local_map$get(key = key))
          }else if( map$has(keys = key) ){
            return(map$get(key = key))
          }else{
            return(save_item())
          }

        }

      })


      return(value)
    }

  )
)
