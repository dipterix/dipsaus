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


#' Install Packages at Next Startup
#' @description Register temporary code that will install packages at
#' next session. The code will be automatically removed once executed.
#' @param packages characters, vector of package names
#' installation; default is false
#' @param restart whether to restart session automatically
#' @param repos repositories to search for packages
#' @param ... internal arguments
#' @return None
#' @details
#' \code{prepare_install} is soft-deprecated, use \code{prepare_install2}
#' instead.
#'
#' Installing packages in R session could require restarts if
#' a package to be updated has been loaded. Normally restarting R
#' fixes the problem. However, under some circumstances, such as with a
#' startup code in profile, restarting R might still fail the
#' installation. \code{prepare_install2} starts a new session with clean
#' environments for installation.
#'
#' @name prepare_install
NULL

#' Restart R Session
#' @description Utilize 'RStudio' functions to restart, if running without
#' 'RStudio', use \code{startup}{restart} instead.
#' @export
restart_session <- function(){
  f <- get0('.rs.restartR')
  if(is.function(f)){
    message('Restarting RStudio rsession. Might take a while. Please wait...')
    f()
    return(invisible())
  }
  # Not in rstudio session
  warning('From next version, `restart_session` only works in RStudio. Please consider using startup::restart() in the future')
  startup::restart()
  return(invisible())
}

#' @rdname prepare_install
#' @export
prepare_install2 <- function(
  packages, restart = FALSE, repos = getOption('repos'), ...){

  warning("`prepare_install2` will be deprecated in the future. Please consider seeking for alternatives. For RAVE users, please check the new installation instructions.")

  github_packages <- str_detect(packages, '/')
  cran_packages <- unique(packages[!github_packages])
  github_packages <- unique(packages[github_packages])

  if(length(github_packages) && !package_installed('remotes')){
    cran_packages <- c(cran_packages, 'remotes')
  }
  if(length(cran_packages)){
    rs_install_r(cran_packages, repos = repos, rs = FALSE, ...)
  }
  if(length(github_packages)){
    rs_install_github(github_packages, repos = repos, rs = FALSE, ...)
  }
  if( restart ){
    restart_session()
  }
  invisible()
}
