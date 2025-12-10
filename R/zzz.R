# add_js_script <- function(){
#   v <- utils::packageVersion('dipsaus')
#   print(v)
#   shiny::addResourcePath(
#     prefix = sprintf('dipsaus', as.character(v)),
#     directoryPath = system.file('shiny-addons/dipsaus', package='dipsaus')
#   )
# }

# Internal operator for default values
`%||%` <- function(x, y) {
  if(is.null(x)) y else x
}

#' @name sumsquared
#' @title Fast Calculation of Sum-squared for Large Matrices/Vectors
#' @description Calculate \code{sum(x^2)}, but faster when the number of
#' elements exceeds 1000.
#' @param x double, integer, or logical vector/matrix
#' @return A numerical scalar
#' @examples
#'
#' x <- rnorm(10000)
#' sumsquared(x)
#'
#' # Compare speed
#' microbenchmark::microbenchmark(
#'   cpp = {sumsquared(x)},
#'   r = {sum(x^2)}
#' )
#'
#' @export
NULL

register_shiny <- function(){

  # register CompoundInput2
  registerCompoundInput2()

  registerSetInputs()

  # Register directory input handlers
  registerDirectoryInputHandlers()

}

.master_session_id <- local({
  master_id <- NULL
  function(uuid){
    if(!missing(uuid)){
      master_id <<- uuid
    }
    master_id
  }
})

is_master <- function(){
  identical(.master_session_id(), session_uuid())
}

dipsaus_init <- local({
  env <- NULL

  function() {
    if(is.null(env)) {
      env <<- new.env(parent = emptyenv())
      env$cache_path <- R_user_dir("dipsaus", which = "cache")
      env$remove_empty_dir <- remove_empty_dir
      environment(env$remove_empty_dir) <- new.env(parent = baseenv())

      reg.finalizer(env, function(e) {
        tryCatch({
          if(file.exists(e$cache_path)) {
            e$remove_empty_dir(e$cache_path, recursive = TRUE)
          }
        }, error = function(e) {})
      })

    }

  }
})

# Session-based storage for directory upload state
.directory_upload_state <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname){

  ns <- asNamespace(pkgname)
  assign(".locker_keys", fastmap::fastmap(), envir = ns)
  assign(".shiny_input_bindings", fastmap::fastmap(), envir = ns)
  ns$.master_session_id( session_uuid() )

  dipsaus_init()

  register_shiny()
  options("dipsaus.shortcuts" = fastmap2())

}


.onUnload <- function(libpath){

  dipsaus_sessionfinalizer$do_finalize()
  if('dipsaus' %in% names(shiny::resourcePaths())){
    shiny::removeResourcePath('dipsaus')
  }
}

#' @name dipsaus-defunct
#' @title Defunct Functions in Package \pkg{dipsaus}
#' The functions or variables listed here are no longer part of the package.
NULL
