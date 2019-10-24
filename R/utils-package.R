#' Check if a package is installed
#' @param pkgs vector of package names
#' @param all only returns TRUE if all packages are installed. Default is FALSE.
#' @examples
#'
#' # Check if package base and dipsaus are installed
#' package_installed(c('base', 'dipsaus'))
#'
#' # Check if all required packages are installed
#' package_installed(c('base', 'dipsaus'), all = TRUE)
#'
#' @return logical, if packages are installed or not. If \code{all=TRUE}, return
#' a logical value of whether all packages a re installed.
#' @export
package_installed <- function(pkgs, all = FALSE){
  re <- sapply(pkgs, function(p){
    system.file('', package = p) != ''
  })
  if(all){
    re <- all(re)
  }
  re
}


#' @title Check If Packages Are Installed, Returns Missing Packages
#' @param pkgs vector of packages to install
#' @param libs paths of libraries
#' @param auto_install automatically install packages if missing
#' @param ... other parameters for \code{install.packages}
#'
#' @return package names that are not installed
#'
#' @export
check_installed_packages <- function(pkgs, libs = base::.libPaths(), auto_install = FALSE, ...){
  installed <- sapply(pkgs, package_installed)
  pkgs <- pkgs[!installed]
  if(auto_install && length(pkgs)){
    cat2('Installing packages:', paste0('[', pkgs, ']', collapse = ', '), level = 'INFO')
    do.call(utils::install.packages, c(
      list(pkgs = pkgs),
      list(...)
    ))
  }
  return(pkgs)
}
