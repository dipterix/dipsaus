# add_js_script <- function(){
#   v <- utils::packageVersion('dipsaus')
#   print(v)
#   shiny::addResourcePath(
#     prefix = sprintf('dipsaus', as.character(v)),
#     directoryPath = system.file('shiny-addons/dipsaus', package='dipsaus')
#   )
# }

register_shiny <- function(){

  # register CompoundInput2
  registerCompoundInput2()

  registerSetInputs()

}

.onLoad <- function(libname, pkgname){

  # add_js_script()

  register_shiny()

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
