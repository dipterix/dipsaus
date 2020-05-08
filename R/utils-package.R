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
#' @return None
#' @details Installing packages in R session could require restarts if
#' a package to be updated has been loaded. Normally restarting R
#' fixes the problem. However, under some circumstances, such as with a
#' startup code in profile, restarting R might still fail the
#' installation. \code{prepare_install} inserts the installation
#' code prior to the startup code so that next time the code will get
#' executed before any other packages are loaded.
#' Once the temporary code get executed, no matter succeeded or not,
#' it will be removed from startup profile.
#' @export
prepare_install <- function(packages, update_all = FALSE,
                            restart = FALSE,
                            repos = getOption('repos')){
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
  # Add two alternative repositories that provide patches
  repos[['RcppCore']] <- 'https://RcppCore.github.io/drat/'
  repos[['dipterix']] <- 'https://dipterix.github.io/drat/'

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
      sver <- installed[installed[,1] == p, 3]
      if(utils::compareVersion(as.character(pver), sver) > 0){
        # newly installed
        next()
      }
    }
    tryCatch({
      utils::install.packages(p, repos = repos, type = 'binary')
    }, error = function(e){
      utils::install.packages(p, repos = repos, type = 'source')
    })

  }
}, error = function(e){
  message('Error found during installation procedure')
  print(traceback(e))
}, finally = {
  message('Removing temporary installation scripts.')
  profile <- startup::find_rprofile()
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
                   paste(deparse(packages), collapse = ''))
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
