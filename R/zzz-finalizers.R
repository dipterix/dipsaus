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
      private$counts <- fastmap2()
      private$finalizers <- fastmap2()
      private$signatures <- fastmap2()
    },
    finalize = function(){
      .subset2(private$counts, 'reset')()
      .subset2(private$signatures, 'reset')()
      .subset2(private$finalizers, 'reset')()
    },
    register = function(key, object, finalizer, onexit = FALSE,
                        replace_if_exists = c('ignore', 'flag', 'finalizer', 'both')){
      stopifnot(is.function(finalizer))
      stopifnot(is.environment(object) || inherits(object, c('externalptr')))
      replace_if_exists <- match.arg(replace_if_exists)

      object_signature <- memory_address(object)
      # have to remove the object so that there won't
      # be any reference to the object in this function
      rm(object)

      if(!.subset2(private$counts, 'has')(key)){
        private$counts[[key]] <- 1L
        private$finalizers[[key]] <- list( finalizer = finalizer, onexit = onexit )
        private$signatures[[object_signature]] <- c(private$signatures[[object_signature]], key)
      } else {
        registered_keys <- private$signatures[[object_signature]]
        registered <- key %in% registered_keys
        if(!registered){
          private$counts[[key]] <- private$counts[[key]] + 1L
          private$signatures[[object_signature]] <- c(private$signatures[[object_signature]], key)
        }

        switch (
          replace_if_exists,
          'flag' = {
            private$finalizers[[key]]$onexit <- onexit
          },
          'finalizer' = {
            private$finalizers[[key]]$finalizer <- finalizer
          },
          'both' = {
            private$finalizers[[key]]$onexit <- onexit
            private$finalizers[[key]]$finalizer <- finalizer
          }
        )

      }
      rm(finalizer)
      return(self$.generate_finalizer(key, object_signature))

    },
    .generate_finalizer = function(key, object_signature){
      new_function2(alist(e=), {
        object_signature <- !!object_signature
        key <- !!key
        registered <- private$signatures[[object_signature]]
        if(!key %in% registered){
          return()
        }
        registered <- registered[!registered %in% key]
        if(length(registered)){
          private$signatures[[object_signature]] <- registered
        } else {
          # this object is no longer used
          .subset2(private$signatures, 'remove')(object_signature)
        }
        # counting reference - 1
        new_count <- private$counts[[!!key]] - 1L
        if(new_count > 0){
          private$counts[[!!key]] <- new_count
          return()
        }

        # No reference, the key
        .subset2(private$counts, 'remove')(!!key)

        # run finalizer
        tryCatch({
          private$finalizers[[!!key]]$finalizer(e)
        }, error = function(e){
          cat2('Error occurs during finalizing an object with key ', !!key, '\nReasons: ',
               e$message, level = 'DEFAULT')
        })

        if(!isFALSE(private$finalizers[[!!key]]$onexit)){
          .subset2(private$finalizers, 'remove')(!!key)
        }
      }, env = self)
    }
  )
)


dipsaus_sessionfinalizer <- DipsausSessionFinalizer$new()


#' @title Create Shared Finalization to Avoid Over Garbage Collection
#' @description Generates a function to be passed to
#' \code{\link{reg.finalizer}}
#' @param key characters that should be identical if
#' finalization method is to be shared
#' @param x object to finalize
#' @param fin Shared finalization: function to call on finalization;
#' see \code{\link{reg.finalizer}}. See details.
#' @param onexit logical: should the finalization be run if the
#' object is still uncollected at the end of the R session?
#' See \code{\link{reg.finalizer}}
#' @param ... passed to other methods
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
#' The argument of \code{fin} varies according to different types of
#' \code{x}. For environments, \code{fin} contains and only contains
#' one parameter, which is the environment itself. This is the same
#' as \code{reg.finalizer}. For \code{R6} classes, \code{fin} is
#' ignored if class has \code{"shared_finalize"} method defined.
#' For \code{\link[fastmap]{fastmap}} or \code{\link[dipsaus]{fastmap2}}
#' instances, \code{fin} accepts no argument.
#'
#' @examples
#'
#' # ------------ Environment example ------------
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
#' shared_finalizer(e1, 'cleanXXXfiles', clear_files)
#' shared_finalizer(e2, 'cleanXXXfiles', clear_files)
#'
#' # Now remove e1, files are not cleaned, and e2 is still valid
#' rm(e1); invisible(gc(verbose = FALSE))
#' e2$valid()  # TRUE
#' file_exists # TRUE
#'
#' # remove both e1 and e2, and file gets removed
#' rm(e2); invisible(gc(verbose = FALSE))
#' file_exists  # FALSE
#'
#' # ------------ R6 example ------------
#'
#' cls <- R6::R6Class(
#'   classname = '...demo...',
#'   cloneable = TRUE,
#'   public = list(
#'     file_path = character(0),
#'     shared_finalize = function(){
#'       cat('Finalize shared resource - ', self$file_path, '\n')
#'     },
#'     finalize = function(){
#'       cat('Finalize private resource\n')
#'     },
#'     initialize = function(file_path){
#'       self$file_path = file_path
#'       shared_finalizer(self, key = self$file_path)
#'     }
#'   )
#' )
#' e1 <- cls$new('file1')
#' rm(e1); invisible(gc(verbose = FALSE))
#'
#' e1 <- cls$new('file2')
#'
#' # A copy of e1
#' e2 <- e1$clone()
#' # unfortunately, we have to manually register
#' shared_finalizer(e2, key = e2$file_path)
#'
#' # Remove e1, gc only free private resource
#' rm(e1); invisible(gc(verbose = FALSE))
#'
#' # remove e1 and e2, run shared finalize
#' rm(e2); invisible(gc(verbose = FALSE))
#'
#' # ------------ fastmap/fastmap2 example -----------
#'
#' # No formals needed for fastmap/fastmap2
#' fin <- function(){
#'   cat('Finalizer is called\n')
#' }
#' # single reference case
#' e1 <- dipsaus::fastmap2()
#' shared_finalizer(e1, 'fin-fastmap2', fin = fin)
#' invisible(gc(verbose = FALSE)) # Not triggered
#' rm(e1); invisible(gc(verbose = FALSE)) # triggered
#'
#' # multiple reference case
#' e1 <- dipsaus::fastmap2()
#' e2 <- dipsaus::fastmap2()
#' shared_finalizer(e1, 'fin-fastmap2', fin = fin)
#' shared_finalizer(e2, 'fin-fastmap2', fin = fin)
#'
#' rm(e1); invisible(gc(verbose = FALSE)) # Not triggered
#' rm(e2); invisible(gc(verbose = FALSE)) # triggered
#'
#' # ------------ Native reg.finalizer fails example ------------
#'
#' # This example shows a failure case using base::reg.finalizer
#'
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
#' # Clean-ups
#' rm(e2); gc()
#'
#' @export
shared_finalizer <- function(x, key, fin, onexit = FALSE, ...){
  UseMethod('shared_finalizer')
}

#' @rdname shared_finalizer
#' @export
shared_finalizer.default <- function(x, key, fin, onexit = FALSE, ...){
  re <- dipsaus_sessionfinalizer$register(
    key = key, object = x, finalizer = fin, onexit = onexit,
    replace_if_exists = 'both')
  assign('@@finalize@@', re, envir = x)

  finalizer_wrapper <- function(e) {
    .subset2(e, '@@finalize@@')(e)
    rm(e)
  }
  environment(finalizer_wrapper) <- baseenv()
  reg.finalizer(x, finalizer_wrapper, onexit = onexit)

  rm(key, x, fin, re, onexit)
  invisible()
}

#' @rdname shared_finalizer
#' @export
shared_finalizer.R6 <- function(x, key, fin, onexit = TRUE, ...){
  if(is.function(.subset2(x, 'shared_finalize'))){
    r6_finalize <- function(e) {
      .subset2(e, 'shared_finalize')()
    }
    environment(r6_finalize) <- baseenv()
  } else {
    r6_finalize <- fin
  }

  re <- dipsaus_sessionfinalizer$register(
    key = key, object = x, finalizer = r6_finalize, onexit = onexit,
    replace_if_exists = 'both')

  reg.finalizer(x, re, onexit = onexit)

  rm(key, x, re, onexit, r6_finalize)
  invisible()
}

#' @rdname shared_finalizer
#' @export
shared_finalizer.fastmap <- function(x, key, fin, onexit = FALSE, ...){
  cenv <- environment(.subset2(x, 'reset'))
  # create an wrapper
  if(!length(formals(fin))){
    formals(fin) <- alist(...=)
  }

  re <- dipsaus_sessionfinalizer$register(
    key = key, object = cenv$key_idx_map, finalizer = fin, onexit = onexit,
    replace_if_exists = 'both')

  reg.finalizer(cenv$key_idx_map, re, onexit = onexit)

  rm(key, x, re, onexit, cenv, fin)
  invisible()
}

#' @rdname shared_finalizer
#' @export
shared_finalizer.fastmap2 <- function(x, key, fin, onexit = FALSE, ...){
  shared_finalizer.fastmap(x, key, fin, onexit, ...)
  rm(key, x, onexit, fin)
  invisible()
}

