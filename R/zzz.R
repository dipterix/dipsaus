# add_js_script <- function(){
#   v <- utils::packageVersion('dipsaus')
#   print(v)
#   shiny::addResourcePath(
#     prefix = sprintf('dipsaus', as.character(v)),
#     directoryPath = system.file('shiny-addons/dipsaus', package='dipsaus')
#   )
# }

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

.onLoad <- function(libname, pkgname){

  # add_js_script()

  register_shiny()

  options("dipsaus.shortcuts" = fastmap2())

  ns <- asNamespace(pkgname)

  ns$.master_session_id( session_uuid() )


  # reg.finalizer(session_log, function(x){
  #   x$finalize()
  # }, onexit = TRUE)
}


.onUnload <- function(libpath){

  dipsaus_sessionfinalizer$finalize()
  if('dipsaus' %in% names(shiny::resourcePaths())){
    shiny::removeResourcePath('dipsaus')
  }
}



#' @name dipsaus-defunct
#' @title Defunct Functions in Package \pkg{dipsaus}
#' The functions or variables listed here are no longer part of the package.
NULL
