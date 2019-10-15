#' Check if a package is installed
#' @param pkgs vector of package names
#' @param all only returns TRUE if all packages are installed. Default is FALSE.
#' @export
package_installed <- function(pkgs, all = FALSE){
  re = sapply(pkgs, function(p){
    system.file('', package = p) != ''
  })
  if(all){
    re = all(re)
  }
  re
}


#' @title Check If Packages Are Installed, Returns Missing Packages
#' @param pkgs vector of packages to install
#' @param libs paths of libraries
#' @param auto_install automatically install packages if missing
#' @param ... other parameters for \code{install.packages}
#' @export
check_installed_packages <- function(pkgs, libs = base::.libPaths(), auto_install = FALSE, ...){
  installed = sapply(pkgs, package_installed)
  pkgs = pkgs[!installed]
  if(auto_install && length(pkgs)){
    cat2('Installing packages:', paste0('[', pkgs, ']', collapse = ', '), level = 'INFO')
    do.call(utils::install.packages, c(
      list(pkgs = pkgs),
      list(...)
    ))
  }
  return(pkgs)
}
