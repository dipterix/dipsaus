# RStudio functions

#' @title Verify 'RStudio' version
#' @param version_needed minimum version required
#' @param child_ok check if the current R process is a child process of the
#' main RStudio session.
#' @param shiny_ok if set false, then check if 'Shiny' is running, return false
#' if shiny reactive domain is not \code{NULL}
#' @seealso \code{\link[rstudioapi]{isAvailable}}
#' @return whether 'RStudio' is running and its version is above the
#' required
#' @export
rs_avail <- function(version_needed = '1.3', child_ok = FALSE, shiny_ok = FALSE){

  if(!shiny_ok && !is.null(shiny::getDefaultReactiveDomain())){
    return(FALSE)
  }
  if(!requireNamespace('rstudioapi')){
    return(FALSE)
  }
  rstudioapi::isAvailable(version_needed = version_needed, child_ok = child_ok)
}

#' Focus on 'RStudio' Console
#' @description Focus on coding; works with 'RStudio' (\code{>=1.4})
#' @param wait wait in seconds before sending command; if too soon, then
#' 'RStudio' might not be able to react.
#' @return None
#' @export
rs_focus_console <- function(wait = 0.5){
  if(rs_avail(version_needed = '1.4')){
    if(wait > 0){
      Sys.sleep(wait)
    }
    try({
      rstudioapi::executeCommand("activateConsole", quiet = TRUE)
    }, silent = TRUE)
  }
  return(invisible())
}

rs_runjob <- function(script, name, focus_on_console = FALSE, ...){
  rstudioapi::jobRunScript(path = script, name = name,
                           workingDir = tempdir(),
                           importEnv = NULL, exportEnv = "")
  if(focus_on_console){
    rs_focus_console()
  }
  return()
}

future_is_sequential <- function(){
  inherits(future::plan(), 'sequential')
}

rs_runjob_alt <- function(script, name, wait = TRUE,
                          args = "--vanilla", ...){
  # use RScript
  if(!file.exists(script)){
    stop("script is missing")
  }
  script <- normalizePath(script, mustWork = TRUE, winslash = "\\")
  rscript <- R.home('bin')

  rscript <- list.files(rscript, '^rscript', full.names = TRUE, ignore.case = TRUE)
  rscript <- normalizePath(rscript, mustWork = TRUE, winslash = "\\")[[1]]

  # inject to load base packages
  sinfo <- utils::sessionInfo()
  s <- readLines(script)
  s <- c(
    paste0('library(', rev(sinfo$basePkgs), ')'),
    s
  )

  cmd <- sprintf('"%s" %s "%s"', rscript, paste(args, collapse = " "), script)
  if(get_os() == "windows"){
    system(cmd, wait = wait, show.output.on.console = FALSE, invisible = TRUE, minimized = TRUE, intern = FALSE, ...)
  } else {
    system(cmd, wait = wait, intern = FALSE, ...)
  }

  return()
}

#' Schedule a Background Job
#' @description Utilizes 'RStudio' job scheduler if correct environment is
#' detected, otherwise call system command via \code{Rscript}
#' @param expr R expression
#' @param name used by 'RStudio' as name of the job
#' @param quoted is \code{expr} quoted
#' @param rs whether to use 'RStudio' by default
#' @param wait whether to wait for the result.
#' @param packages packages to load in the sub-sessions
#' @param focus_on_console whether to return back to console after creating
#' jobs; useful when users want to focus on writing code; default is false.
#' This feature works with 'RStudio' (\code{>=1.4})
#' @param nested_ok whether nested \code{rs_exec} is allowed; default is false;
#' Set to true to allow nested parallel code, but use at your own risk.
#' @param ... internally used
#' @return If \code{wait=TRUE}, returns evaluation results of \code{expr},
#' otherwise a function that can track the state of job.
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
#' @examples
#'
#' if(interactive()){
#'   h <- rs_exec(
#'     {
#'       Sys.sleep(2)
#'       print(Sys.getpid())
#'     },
#'     wait = FALSE, name = 'Test',
#'     focus_on_console = TRUE
#'   )
#'   code <- h()
#'   print(code)
#'
#'   # wait 3 seconds
#'   Sys.sleep(3)
#'   code <- h()
#'   attributes(code)
#' }
#'
#' @export
rs_exec <- function(expr, name = 'Untitled', quoted = FALSE, rs = TRUE,
                    wait = FALSE, packages = NULL, focus_on_console = FALSE,
                    ..., nested_ok = FALSE){
  if(!nested_ok && !is_master()) {
    stop("Function `rs_exec`, `lapply_callr` should not be nested.")
  }
  if(!quoted){
    expr <- substitute(expr)
  }
  tempdir(check = TRUE)
  script <- tempfile()
  state_file <- paste0(script, '.dstate')
  res_file <- paste0(script, '.res')

  # 1: initializing
  writeLines('1', state_file)
  state_file <- normalizePath(state_file)

  session_id <- .master_session_id()

  use_rs <- rs && rs_avail(child_ok = TRUE, shiny_ok = TRUE)

  sys_env <- Sys.getenv()

  expr <- rlang::quo({
    # 2: started
    writeLines('2', !!state_file)

    local({
      ...msg... <- new.env(parent = emptyenv())
      reg.finalizer(...msg..., function(e){
        grDevices::graphics.off()
        if(length(e$error)){
          writeLines(c('-1', e$error), !!state_file)
        } else {
          writeLines('0', !!state_file)
        }
      }, onexit = TRUE)

      ...msg...$fun <- function(){
        !!expr
      }

      tryCatch({
        options("raveio.settings_readonly" = TRUE)
        if(!!use_rs){
          options("crayon.enabled" = TRUE)
          options("crayon.colors" = 256)
        }

        lapply(!!packages, function(p){
          suppressMessages({
            do.call('require', list(package = p, character.only = TRUE))
          })
        })

        local({
          ns <- asNamespace('dipsaus')
          ns$.master_session_id(!!session_id)
          sys_env <- !!sys_env
          do.call(Sys.setenv, as.list(sys_env))
        })

        res <- ...msg...$fun()

        if(!is.null(res)){
          saveRDS(res, file = !!res_file)
        }

        writeLines('0', !!state_file)

      }, error = function(e){
        ...msg...$error <- e$message
        writeLines(c('-1', ...msg...$error), !!state_file)
      }, finally = {
        rm(...msg...)
        gc()
      })

    })
  })
  writeLines(deparse(rlang::quo_squash(expr)), script, sep = '\n')

  if(use_rs){
    if(wait){
      rs_runjob(script, name, focus_on_console = FALSE, ...)
    } else {
      rs_runjob(script, name, focus_on_console = focus_on_console, ...)
    }
  } else {
    rs_runjob_alt(script, name, wait = wait, ...)
  }

  # returns a function checking states
  state <- 0
  res <- NULL

  check_f <- function(){
    # This function can track the rs_exec process
    if(file.exists(state_file)){
      s <- readLines(state_file)
      s <- stringr::str_trim(s)
      st <- as.integer(s[[1]])
      if(is.na(st)){
        # unknown results
        st <- -2
      } else {
        s <- s[-1]
      }
      if(st < 0){
        unlink(script)
        unlink(state_file)
        attr(st, 'rs_exec_error') <- s
        attr(st, 'rs_exec_state') <- 'Error'
      } else if(st == 0){
        unlink(script)
        unlink(state_file)
        if(file.exists(res_file)){
          res <<- readRDS(res_file)
        }
        unlink(res_file)
        attr(st, 'rs_exec_state') <- 'Success'
        attr(st, 'rs_exec_result') <- res
        # return to console
        if(focus_on_console){
          rs_focus_console(wait = 0)
        }
      } else if(st > 0){
        attr(st, 'rs_exec_state') <- 'Running'
      }
      state <<- st
    }
    return(structure(state, class = 'dipsaus_rs_exec_res'))
  }

  if(wait){
    check_f()
    while(isTRUE(state > 0)){
      check_f()
      Sys.sleep(0.5)
    }
    check_f <- check_f()
  }
  invisible(check_f)
}

#' @export
print.dipsaus_rs_exec_res <- function(x, ...){
  cat('Code :', as.numeric(x), '\n')
  cat('State:', attr(x, 'rs_exec_state'), '\n')
  if(x < 0){
    cat('Error:\n')
    print(attr(x, 'rs_exec_error'))
  } else if(x==0){
    cat('Please use `attr(x, "rs_exec_result")` to get the results.')
  }
  invisible(x)
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

    repos <- !!repos

    installr <- function(...){
      pkgs <- c(...)
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

    repos <- !!repos

    installr <- function(...){
      pkgs <- c(...)
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

  rs_exec(rlang::quo_squash(quo), quoted = TRUE, name = 'Install packages', rs = rs, wait = TRUE)

  invisible()
}



#' Get 'RStudio' active project
#' @param ... passed to \code{\link{rs_avail}}
#' @return If 'RStudio' is running and current project is not none, return
#' project name, otherwise return \code{NA}
#' @export
rs_active_project <- function(...){
  if( rs_avail(...) ){
    return(rstudioapi::getActiveProject())
  }
  NA
}


#' @title Get 'RStudio' Viewer, or Return Default
#' @param ... passed to \code{\link[rstudioapi]{viewer}}
#' @param default if \code{\link{rs_avail}} fails, the
#' value to return. Default is \code{TRUE}
#' @param version_needed,child_ok,shiny_ok passed to \code{\link{rs_avail}}
#' @return If \code{\link[rstudioapi]{viewer}} can be called and
#' 'RStudio' is running, then launch 'RStudio' internal viewer.
#' Otherwise if \code{default} is a function such as
#' \code{\link[utils]{browseURL}}, then call the function with given
#' arguments. If \code{default} is not a function, return \code{default}
#' @export
rs_viewer <- function(..., default = TRUE, version_needed = '1.3',
                      child_ok = FALSE, shiny_ok = FALSE){
  if(rs_avail(version_needed = version_needed, child_ok = child_ok,
              shiny_ok = shiny_ok)){
    rstudioapi::viewer(...)
  }else{
    if(is.function(default)){
      default(...)
    }else{
      return(default)
    }
  }
}


#' @title Use 'RStudio' to Select a Path on the Server
#' @param is_directory whether the path should be a directory
#' @return Raise error if \code{\link{rs_avail}} fails,
#' otherwise returns the selected path
#' @export
rs_select_path <- function(is_directory = TRUE){
  if(dipsaus::rs_avail()){
    if(is_directory){
      path <- rstudioapi::selectDirectory()
    }else{
      path <- rstudioapi::selectFile()
    }
    # warning("Please fix the path in your script!!!\n\t{path}")
    return(path)
  }else{
    stop("Cannot find file path. Please contact package owner to fix it.")
  }
}



#' @title Save all documents in 'RStudio'
#' @description Perform "safe" save-all action with backward
#' compatibility: check whether 'RStudio' is running and whether
#' \code{rstudioapi} has function \code{documentSaveAll}.
#' @export
rs_save_all <- function(){
  if(rs_avail(version_needed = '1.1.287')){
    if (rstudioapi::hasFun("documentSaveAll")) {
      rstudioapi::documentSaveAll()
      return(invisible())
    }
  }
  warning('RStudio version too low, please update RStudio')
}

#' @title Use 'RStudio' to open and edit files
#' @param path path to file
#' @param create whether to create if path is not found; default is true
#' @return Opens the file pointing to \code{path} to edit, and returns the
#' path
#' @export
rs_edit_file <- function(path, create = TRUE) {
  if(!interactive()) {
    warning("`rs_edit_file`: must run in interactive mode")
    return(path)
  }
  if(!file.exists(path)) {
    if(!create) {
      stop("`rs_edit_file`: File path not exists, cannot open: ", path)
    }
    root <- dirname(path)
    if(!dir.exists(root)) {
      dir.create(root, showWarnings = FALSE, recursive = TRUE)
    }
    file.create(path)
  }

  path <- normalizePath(path, mustWork = TRUE)

  if(rs_avail() && rstudioapi::hasFun("navigateToFile")) {
    rstudioapi::navigateToFile(path)
  } else {
    utils::file.edit(path)
  }
  invisible(path)
}


#' @title Add secondary 'CRAN'-like repository to the 'RStudio' settings
#' @description Add self-hosted repository, such as 'drat', 'r-universe' to
#' 'RStudio' preference. Please restart 'RStudio' to take changes into effect.
#' @param name repository name, must be unique and readable
#' @param url the website address of the repository, starting with schemes
#' such as \code{'https'}.
#' @param add whether to add to existing repository; default is true
#' @returns a list of settings.
#' @details 'RStudio' allows to add secondary 'CRAN'-like repository to its
#' preference, such that users can add on-going self-hosted developing
#' repositories (such as package \code{'drat'}, or 'r-universe'). These
#' repositories will be set automatically when running
#' \code{\link[utils]{install.packages}}.
#' @export
rs_set_repos <- function(name, url, add = TRUE) {

  stopifnot2(rs_avail(),
             msg = "Please use the latest RStudio to call this function API.")
  if(length(name) != 1 || !is.character(name) || !nzchar(name) || name == "CRAN") {
    stop("`rs_set_repos`: name must be non-empty string and cannot be 'CRAN'")
  }
  if(length(url) != 1 || !is.character(url) || !(
    startsWith(tolower(url), "https://") ||
    startsWith(tolower(url), "http://")
  )) {
    stop("`rs_set_repos`: url must start with http:// or https://")
  }
  mirror <- rstudioapi::readRStudioPreference(name = "cran_mirror", default = NULL)
  if(!is.list(mirror)) {
    mirror <- list(
      name = "0-Cloud",
      host = "cloud.r-project.org",
      url = "https://cloud.r-project.org/",
      country = ""
    )
  }
  repos <- list()
  if(add) {
    sec_repo <- unlist(strsplit(as.character(mirror$secondary), "\\|"))
    if(length(sec_repo)) {
      sec_repo <- matrix(sec_repo, ncol = 2, byrow = TRUE)
      for(ii in seq_len(nrow(sec_repo))) {
        repos[[ sec_repo[ii, 1] ]] <- sec_repo[ii, 2]
      }
    } else {
      sec_repo <- NULL
    }
  } else {
    sec_repo <- NULL
  }
  repos[[name]] <- url


  mirror$secondary <- paste(sprintf("%s|%s", names(repos), unlist(repos)),
                            collapse = "|")
  rstudioapi::writeRStudioPreference("cran_mirror", mirror)
  invisible(mirror)
}
