# RStudio functions

rs_avail <- function(){
  rstudioapi::isAvailable(version_needed = '1.3', child_ok = TRUE)
}

#' Focus on 'RStudio' Console
#' @description Safe wrap of \code{\link[rstudioapi]{sendToConsole}}
#' @return None
#' @export
rs_focus_console <- function(){
  if(rs_avail()){
    rstudioapi::sendToConsole('', focus = TRUE, execute = FALSE, echo = TRUE)
  }
  return()
}

rs_runjob <- function(script, name){
  rstudioapi::jobRunScript(path = script, name = name,
                           workingDir = tempdir(),
                           importEnv = NULL, exportEnv = "")
  return()
}

future_is_sequential <- function(){
  inherits(future::plan(), 'sequential')
}

rs_runjob_alt <- function(script, name, wait = TRUE){
  # use RScript
  script <- normalizePath(script)
  if(!file.exists(script)){
    stop("script is missing")
  }
  rscript <- R.home('bin')

  rscript <- list.files(rscript, '^rscript', full.names = TRUE, ignore.case = TRUE)

  if(length(rscript) != 1){
    stop("Cannot find Rscript or Rscript.exe... You might want to use RStudio?")
  }

  cmd <- sprintf("%s --vanilla %s", rscript, script)
  system(cmd, wait = wait)
  return()
}

#' Schedule a Background Job
#' @description Utilizes 'RStudio' job scheduler if correct environment is
#' detected, otherwise call system command via \code{Rscript}
#' @param expr R expression
#' @param name used by 'RStudio' as name of the job
#' @param quoted is \code{expr} quoted
#' @param rs whether to use 'RStudio' by default
#' @param wait whether to wait for the result. Only useful when using
#' \code{Rscript}
#'
#' @details
#' 'RStudio' provides interfaces \code{\link[rstudioapi]{jobRunScript}} to
#' schedule background jobs. However, this
#' functionality only applies using 'RStudio' IDE. When launching R from
#' other places such as terminals, the job scheduler usually result in
#' errors. In this case, the alternative is to call system command via
#' \code{Rscript}
#'
#' The expression \code{expr} will run a clean environment. Therefore R objects
#' created outside of the context will be inaccessible from within the child
#' environment, and packages except for base packages will not be loaded.
#'
#' There is a small difference when running within and without 'RStudio'.
#' When running via \code{Rscript}, the environment will run under
#' \code{vanilla} argument, which means no load, no start-up code. If you
#' have start-up code stored at \code{~/.Rprofile}, the start-up code will be
#' ignored. When running within 'RStudio', the start-up code will be executed.
#' As of \code{rstudioapi} version 0.11, there is no 'vanilla' option. This
#' feature is subject to change in the future.
#'
#' @export
rs_exec <- function(expr, name = 'Untitled', quoted = FALSE, rs = TRUE, wait = FALSE){
  if(!quoted){
    expr <- substitute(expr)
  }
  script <- tempfile()
  writeLines(deparse(expr), script, sep = '\n')

  if(rs && rs_avail()){
    rs_runjob(script, name)
  } else {
    rs_runjob_alt(script, name, wait = wait)
  }
}

rs_install_r <- function(packages, repos = getOption('repos'),
                        check_libpaths = TRUE, unload = NULL, rs = TRUE){
  packages <- unique(packages)
  unload <- unique(c('startup', unload))
  unload <- unload[!unload %in% c('base', 'utils')]

  repos <- unlist(repos, use.names = TRUE)
  repos[['dipterix']] <- "https://dipterix.github.io/drat/"

  if(isTRUE(repos[['CRAN']] == "@CRAN@")){
    repos[['CRAN']] <- "https://cran.rstudio.com/"
  }

  if(rs){
    rprof <- startup::find_rprofile()
    if(length(rprof)){
      for(f in rprof){
        file.rename(f, paste0(f, strftime(Sys.time(), '.bak.%Y%m%d-%H%M%S')))
      }
    }
  }

  quo <- rlang::quo({

    if(!!check_libpaths){
      # ---- Check whether user library is created
      vars <- c("R_LIBS", "R_LIBS_SITE", "R_LIBS_USER")
      for (var in vars) {
        path <- Sys.getenv(var)
        if (!nzchar(path)) next
        is_dummy <- grepl("^[.]", path) && !grepl("[/\\]", path)
        if (is_dummy) next
        paths <- unlist(strsplit(path, split = .Platform$path.sep, fixed = TRUE))
        paths <- unique(paths)
        paths <- paths[!vapply(paths, FUN = dir.exists, FUN.VALUE = FALSE)]
        if ( length(paths) ) {
          tryCatch({
            for(p in paths){
              dir.create(p, recursive = TRUE, showWarnings = FALSE)
              if(dir.exists(p)){
                .libPaths(p)
              }
            }
          })
        }
      }
    }

    repos = !!repos

    installr <- function(...){
      pkgs = c(...)
      cat('Installing ', pkgs, '\n')
      try({
        do.call('install.packages', list(
          pkgs = pkgs,
          repos = repos
        ))
      })
    }
    detect_package <- function(p){
      system.file('', package = p) != ''
    }

    ns_reg <- function(){
      eval(parse(text='(get(".Internal", envir = baseenv(), mode = "function"))(getNamespaceRegistry())'))
    }
    unregister_ns <- function (name = NULL) {
      if (!is.character(name) || name == "" || length(name) != 1)
        return()
      if (!(name %in% loadedNamespaces()))
        return()
      do.call(rm, args = list(name, envir = ns_reg()))
      return()
    }

    if(!!rs){
      # Remove startup scripts
      # install startup to avoid previous installation warnings
      if(!detect_package('startup')){
        # this is required by dipsaus
        installr("startup")
      }

      tryCatch({
        # remove startup scripts
        startup::uninstall()
      }, error = function(e){}, warning = function(e){})
    }



    # If reticulate is installed, Rcpp won't upgrade
    for(ns in !!c(unload, packages)){
      try({
        do.call('unloadNamespace', list(ns))
      })
      if(ns %in% loadedNamespaces()){
        unregister_ns(ns)
      }
    }

    for(p in !!packages){
      installr(p)
    }

    if(!!rs){
      if(detect_package('startup')){
        startup::install(overwrite = FALSE)
      }
    }

    cat('Done\n')
  })

  rs_exec(rlang::quo_squash(quo), quoted = TRUE, name = 'Install packages', rs = rs, wait = TRUE)

  invisible()
}


rs_install_github <- function(packages, repos = getOption('repos'),
                        check_libpaths = TRUE, unload = NULL, rs = TRUE){

  stopifnot2(package_installed('remotes'), msg = 'package `remotes` is missing. Please install it.')

  packages <- unique(packages)
  unload <- unique(c('startup', unload))
  unload <- unload[!unload %in% c('base', 'utils')]

  repos <- unlist(repos, use.names = TRUE)
  repos[['dipterix']] <- "https://dipterix.github.io/drat/"

  if(isTRUE(repos[['CRAN']] == "@CRAN@")){
    repos[['CRAN']] <- "https://cran.rstudio.com/"
  }

  if(rs){
    rprof <- startup::find_rprofile()
    if(length(rprof)){
      for(f in rprof){
        file.rename(f, paste0(f, strftime(Sys.time(), '.bak.%Y%m%d-%H%M%S')))
      }
    }
  }


  quo <- rlang::quo({

    if(!!check_libpaths){
      # ---- Check whether user library is created
      vars <- c("R_LIBS", "R_LIBS_SITE", "R_LIBS_USER")
      for (var in vars) {
        path <- Sys.getenv(var)
        if (!nzchar(path)) next
        is_dummy <- grepl("^[.]", path) && !grepl("[/\\]", path)
        if (is_dummy) next
        paths <- unlist(strsplit(path, split = .Platform$path.sep, fixed = TRUE))
        paths <- unique(paths)
        paths <- paths[!vapply(paths, FUN = dir.exists, FUN.VALUE = FALSE)]
        if ( length(paths) ) {
          tryCatch({
            for(p in paths){
              dir.create(p, recursive = TRUE, showWarnings = FALSE)
              if(dir.exists(p)){
                .libPaths(p)
              }
            }
          })
        }
      }
    }

    repos = !!repos

    installr <- function(...){
      pkgs = c(...)
      cat('Installing ', pkgs, '\n')
      remotes::install_github(pkgs, upgrade = 'never', force = TRUE, repos = repos)
    }
    detect_package <- function(p){
      system.file('', package = p) != ''
    }

    ns_reg <- function(){
      eval(parse(text='(get(".Internal", envir = baseenv(), mode = "function"))(getNamespaceRegistry())'))
    }
    unregister_ns <- function (name = NULL) {
      if (!is.character(name) || name == "" || length(name) != 1)
        return()
      if (!(name %in% loadedNamespaces()))
        return()
      do.call(rm, args = list(name, envir = ns_reg()))
      return()
    }

    if(!!rs){
      # Remove startup scripts
      # install startup to avoid previous installation warnings
      if(!detect_package('startup')){
        # this is required by dipsaus
        installr("startup")
      }

      tryCatch({
        # remove startup scripts
        startup::uninstall()
      }, error = function(e){}, warning = function(e){})
    }




    # If reticulate is installed, Rcpp won't upgrade
    for(ns in !!c(unload, packages)){
      try({
        do.call('unloadNamespace', list(ns))
      })
      if(ns %in% loadedNamespaces()){
        unregister_ns(ns)
      }
    }

    for(p in !!packages){
      installr(p)
    }

    if(!!rs){
      if(detect_package('startup')){
        startup::install(overwrite = FALSE)
      }
    }

    cat('Done\n')
  })

  rs_exec(rlang::quo_squash(quo), quoted = TRUE, name = 'Install packages', rs = rs)

  invisible()
}
