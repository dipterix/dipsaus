add_js_script <- function(){
  shiny::addResourcePath(
    prefix = 'dipsaus', directoryPath = system.file('shiny-addons/dipsaus', package='dipsaus')
  )
}

.onLoad <- function(libname, pkgname){

  add_js_script()

  registerCompoundInput2()

}


