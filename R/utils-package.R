#' Get attached package names in current session (Internally used)
#' @param include_base whether to include base packages
#' @return characters, package names that are attached in current session
attached_packages <- function(include_base = FALSE){
  info <- utils::sessionInfo()
  bk <- rev(info$basePkgs)
  pk <- vapply(info$otherPkgs, '[[', 'Package', 'Package', USE.NAMES = FALSE)
  pk <- rev(pk)

  if(include_base){
    pk <- c(bk, pk)
  }
  pk
}

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

#' Restart R Session
#' @description Utilize 'RStudio' functions to restart, if running without
#' 'RStudio', use package \code{startup} (not included in this library) instead.
#' @export
restart_session <- function(){
  f <- get0('.rs.restartR')
  if(is.function(f)){
    message('Restarting RStudio rsession. Might take a while. Please wait...')
    f()
    return(invisible())
  }
  # Not in rstudio session
  warning('`restart_session` only works in RStudio. Please consider using startup::restart()')
  return(invisible())
}

