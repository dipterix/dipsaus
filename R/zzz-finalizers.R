memory_address <- function(x) {
  .Call("_dipsaus_object_address", x)
}

DipsausSessionFinalizer <- R6::R6Class(
  classname = 'DipsausSessionFinalizer',
  portable = FALSE,
  parent_env = asNamespace('dipsaus'),
  cloneable = FALSE,
  private = list(
    counts = NULL,
    finalizers = NULL,
    signatures = NULL
  ),
  public = list(
    initialize = function(){
      private$counts = fastmap2()
      private$finalizers = fastmap2()
      private$signatures = fastmap2()
    },
    finalize = function(){
      .subset2(private$counts, 'reset')
      .subset2(private$signatures, 'reset')
      .subset2(private$finalizers, 'reset')
    },
    register = function(key, object, finalizer, onexit = FALSE,
                        replace_if_exists = c('ignore', 'flag', 'finalizer', 'both')){
      stopifnot(is.function(finalizer))
      stopifnot(is.environment(object) || inherits(object, c('R6', 'fastmap', 'fastmap2')))
      replace_if_exists = match.arg(replace_if_exists)

      object_signature <- memory_address(object)
      # have to remove the object so that there won't
      # be any reference to the object in this function
      rm(object)

      if(!.subset2(private$counts, 'has')(key)){
        private$counts[[key]] <- 1L
        private$finalizers[[key]] <- list( finalizer = finalizer, onexit = onexit )
        private$signatures[[object_signature]] <- key
      } else {
        registered <- .subset2(private$signatures, 'has')(object_signature)
        if(!registered){
          private$counts[[key]] <- private$counts[[key]] + 1L
          private$signatures[[object_signature]] <- key
        }

        switch (
          replace_if_exists,
          'flag' = {
            private$finalizers[[key]]$onexit = onexit
          },
          'finalizer' = {
            private$finalizers[[key]]$finalizer = finalizer
          },
          'both' = {
            private$finalizers[[key]]$onexit = onexit
            private$finalizers[[key]]$finalizer = finalizer
          }
        )

      }
      rm(finalizer)
      return(self$.generate_finalizer(key, object_signature))

    },
    .generate_finalizer = function(key, object_signature){
      function(e){

        if(!.subset2(private$signatures, 'has')(object_signature)){
          return()
        }

        # this object is no longer used
        .subset2(private$signatures, 'remove')(object_signature)

        # counting reference - 1
        new_count <- private$counts[[key]] - 1L
        if(new_count > 0){
          private$counts[[key]] <- new_count
          return()
        }

        # No reference, the key
        .subset2(private$counts, 'remove')(key)

        # run finalizer
        tryCatch({
          private$finalizers[[key]]$finalizer(e)
        }, error = function(e){
          cat2('Error occurs during finalizing an object with key ', key, '\nReasons: ',
               e$message, level = 'DEFAULT')
        })

        if(!isFALSE(private$finalizers[[key]]$onexit)){
          .subset2(private$finalizers, 'remove')(key)
        }

      }
    }
  )
)


dipsaus_sessionfinalizer <- DipsausSessionFinalizer$new()


#' @title Create Shared Finalizers to Avoid Over Garbage Collection
#' @description Generates a function to be passed to
#' \code{\link{reg.finalizer}}
#' @param key characters that should be identical to shared finalizers
#' @param env object to finalize. Must be an environment
#' @param fin Shared finalizer: function to call on finalization;
#' see \code{\link{reg.finalizer}}
#' @param onexit logical: should the finalizer be run if the object is
#' still uncollected at the end of the R session?
#' See \code{\link{reg.finalizer}}
#' @details The main purpose of this function is to allow multiple
#' objects that point to a same source (say a temporary file) to
#' perform clean up when all the objects are garbage collected.
#'
#' Base function \code{\link{reg.finalizer}} provides finalization
#' to to garbage collect single R environment. However, when multiple
#' environments share the same file, finalizing one single environment
#' will result in removing the file so that all the other environment
#' lose the reference. (See example "Native \code{reg.finalizer}
#' fails example")
#'
#' @examples
#' #'
#' file_exists <- TRUE
#' clear_files <- function(e){
#'   print('Clean some shared files')
#'   # do something to remove files
#'   file_exists <<- FALSE
#' }
#'
#' # e1, e2 both require file existence
#' e1 <- new.env()
#' e1$valid <- function(){ file_exists }
#' e2 <- new.env()
#' e2$valid <- function(){ file_exists }
#'
#' e1$valid(); e2$valid()
#'
#' # we don't want to remove files when either e1,e2 gets
#' # garbage collected, however, we want to run `clear_files`
#' # when system garbage collecting *both* e1 and e2
#'
#' # Make sure `key`s are identical
#' e1_fin <- shared_finalizer('cleanXXXfiles', e1, clear_files)
#' e2_fin <- shared_finalizer('cleanXXXfiles', e2, clear_files)
#'
#' reg.finalizer(e1, e1_fin)
#' reg.finalizer(e2, e2_fin)
#'
#' # Now remove e1, files are not cleaned, and e2 is still valid
#' rm(e1); gc()
#' e2$valid()  # TRUE
#' file_exists # TRUE
#'
#' # remove both e1 and e2, and file gets removed
#' rm(e2); gc()
#' file_exists  # FALSE
#'
#'
#' # ------------ Native reg.finalizer fails example ------------
#' file_exists <- TRUE
#' clear_files <- function(e){
#'   print('Clean some shared files')
#'   # do something to remove files
#'   file_exists <<- FALSE
#' }
#'
#' # e1, e2 both require file existence
#' e1 <- new.env()
#' e1$valid <- function(){ file_exists }
#' e2 <- new.env()
#' e2$valid <- function(){ file_exists }
#'
#' reg.finalizer(e1, clear_files)
#' reg.finalizer(e2, clear_files)
#' gc()
#' file_exists
#'
#' # removing e1 will invalidate e2
#' rm(e1); gc()
#' e2$valid()    # FALSE
#'
#' @export
shared_finalizer <- function(key, env, fin, onexit = FALSE){
  re <- dipsaus_sessionfinalizer$register(
    key = key, object = env, finalizer = fin, onexit = onexit,
    replace_if_exists = 'both')
  rm(key, env, fin, onexit)
  re
}

