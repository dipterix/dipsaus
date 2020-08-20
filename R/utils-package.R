#' Get attached package names in current session
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
#' @param update_all whether to update all installed packages before
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

#' @rdname prepare_install
#' @export
prepare_install <- function(packages, update_all = FALSE,
                            restart = FALSE,
                            repos = getOption('repos')){
  warning('prepare_install is soft-deprecated and may cause R failures, use `prepare_install2` instead')
  profile <- startup::find_rprofile()
  if(!length(profile)){
    startup::install()
  }
  profile <- startup::find_rprofile()

  s <- readLines(profile)

  lines <- grep('^#\\ \\-\\-\\-\\ dipsaus\\ temporary', s)
  if(length(lines) >= 2) {
    message('Previous installation code found. Remove and replace with new one...')
    s <- s[-(seq(min(lines), max(lines)))]
  }

  if(!length(repos)){
    repos <- c()
  }
  if(!'CRAN' %in% names(repos) || repos[['CRAN']] == '@CRAN@'){
    repos[['CRAN']] <- 'https://cran.rstudio.com/'
  }
  repos <- c(list('dipterix' = 'https://dipterix.github.io/drat/'), as.list(repos))
  # Add two alternative repositories that provide patches
  repos <- unlist(repos, use.names = TRUE)

  # prepend lines to s

  pre <- paste("# --- dipsaus temporary startup (BEGIN)---
# This is one-time startup code REMOVE the block once finished
message('Execute temporary startup code to install packages...')
tryCatch({
  # Add repository
  repos <- %s
  if(%s){
    utils::update.packages(ask = FALSE, repos = repos)
  }
  packages <- %s
  installed <- utils::installed.packages()
  for(p in unique(packages)){
    if(system.file('', package = p) != '' && p %%in%% installed[,1]){
      pver <- utils::packageVersion(p)[[1]]
      sver <- installed[installed[,1] == p, 3][[1]]
      if(utils::compareVersion(as.character(pver), sver) > 0){
        # newly installed
        next()
      }
    }

    tryCatch({
      utils::install.packages(p, repos = repos, type = 'binary')
    }, warning = function(e){
      if(grepl('^package .*is not available \\\\(as a binary package', e$message)){
        utils::install.packages(p, repos = repos, type = 'source')
      }
    }, error = function(e){})
  }
}, error = function(e){
  message('Error found during installation procedure')
  print(traceback(e))
}, finally = {
  message('Removing temporary installation scripts.')
  profile <- '%s'
  s <- readLines(profile)
  lines <- grep('^#\\\\ \\\\-\\\\-\\\\-\\\\ dipsaus\\\\ temporary', s)
  if(length(lines) >= 2) {
    s <- s[-(seq(min(lines), max(lines)))]
    writeLines(s, con = profile)
  }
})
message('Done.')
# --- dipsaus temporary startup (END)---", collapse = '\n')

  packages <- unique(packages)
  if(length(packages)){
    pre <- sprintf(pre, paste(deparse(repos), collapse = ''),
                   paste(deparse(update_all), collapse = ''),
                   paste(deparse(packages), collapse = ''),
                   startup::find_rprofile())
  } else {
    pre <- NULL
  }

  writeLines(c(pre, s), con = profile)

  if(restart){
    f <- get0('.rs.restartR')
    if(is.function(f)){
      message('Restarting RStudio rsession. Might take a while. Please wait...')
      f()
      return(invisible())
    }
    # Not in rstudio session
    message('Using startup::restart()')
    startup::restart()
    return(invisible())
  }
  message('Please restart ALL R session now. Next startup might take a while. Please wait until finished')
  return(invisible())
}

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
  message('Using startup::restart()')
  startup::restart()
  return(invisible())
}

#' @rdname prepare_install
#' @export
prepare_install2 <- function(
  packages, restart = FALSE, repos = getOption('repos'), ...){

  github_packages <- stringr::str_detect(packages, '/')
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
