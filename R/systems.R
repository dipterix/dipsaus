# System utilities

#' Detect the type of operating system
#' @return The type of current operating system: \code{'windows'},
#' \code{'darwin'}, \code{'linux'}, \code{'solaris'}, or otherwise
#' \code{'unknown'}.
#' @examples
#'
#' get_os()
#'
#' @export
get_os <- function(){
  if("windows" %in% stringr::str_to_lower(.Platform$OS.type)){
    return("windows")
  }
  os <- stringr::str_to_lower(R.version$os)
  if(stringr::str_detect(os, '^darwin')){
    return('darwin')
  }
  if(stringr::str_detect(os, '^linux')){
    return('linux')
  }
  if(stringr::str_detect(os, '^solaris')){
    return('solaris')
  }
  if(stringr::str_detect(os, '^win')){
    return('windows')
  }
  return('unknown')
}

safe_system <- function(cmd, ..., intern = TRUE, ignore.stderr = TRUE,
                        minimized = TRUE, invisible = TRUE, show.output.on.console = TRUE){
  suppressWarnings({
    if(get_os() == 'windows'){
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr,
                    minimized = minimized, invisible = invisible,
                    show.output.on.console = show.output.on.console, ...)
    } else {
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr, ...)
    }
  })
  ret
}

safe_system2 <- function(cmd, args, ..., stdout = TRUE, stderr = FALSE, onFound = NULL, onNotFound = NA){

  if(Sys.which(cmd) == ""){
    return(onNotFound)
  }

  suppressWarnings({
    ret <- system2(cmd, args, ..., stdout = stdout, stderr = stderr)
  })
  if(is.function(onFound)){
    ret <- onFound(ret)
  }
  ret
}


get_ram_osx <- function(){
  # try to locate sysctl
  cmd <- Sys.which("sysctl")
  if(cmd == ""){
    cmd <- "/usr/sbin/sysctl"
  }
  if(!file.exists(cmd)){
    cmd <- '/sbin/sysctl'
  }
  if(!file.exists(cmd)){
    return(NA)
  }
  # sysctl exists, use cmd
  ram <- safe_system2("sysctl", "hw.memsize", stdout = TRUE,
    onFound = function(ram) {
      substring(ram, 13)
    }
  )
  structure(
    as.numeric(ram),
    class = "dipsaus_bytes",
    unit = "B"
  )
}

get_ram_windows <- function(){
  # check if windir is defined
  cmd <- Sys.which("wmic")
  if(cmd == ""){
    # wmic might not exists, look for it in %WINDIR%\System32\wbem\wmic.exe
    windir <- Sys.getenv('windir')
    if(windir == ""){
      windir <- Sys.getenv('SystemRoot')
    }
    if(windir == ""){
      windir <- file.path(Sys.getenv('SystemDrive'), "WINDOWS", fsep = "\\")
    }
    if(!dir.exists(windir)){
      windir <- "C:\\WINDOWS"
    }
    cmd <- file.path(windir, "System32", "wbem", "wmic.exe", fsep = "\\")
  }
  if(!file.exists(cmd)){
    # cannot find wmic
    return(NA)
  }
  ram <- safe_system2(cmd, c("MemoryChip", "get", "Capacity"), stdout = TRUE)
  structure(
    as.numeric(ram[[2]]),
    class = "dipsaus_bytes",
    unit = "B"
  )
}

get_ram_linux <- function(){
  # need to check "/proc/meminfo"
  if(!file.exists("/proc/meminfo")){
    return(NA)
  }
  s <- readLines("/proc/meminfo", n = 100)
  # get memtotal
  s <- s[startsWith(s, "MemTotal")]
  if(!length(s)){
    return(NA)
  }
  s <- stringr::str_match(s[[1]], "([0-9]+)([ kKmMgGtT]+)([bB])")
  unit <- stringr::str_to_lower(stringr::str_trim(s[[3]]))
  units <- c('', 'k', 'm', 'g', 't')
  ram <- as.numeric(s[[2]]) * 1024^(which(units == unit) - 1)
  structure(
    as.numeric(ram),
    class = "dipsaus_bytes",
    unit = "B"
  )
}


#' Get Memory Size
#' @return System RAM in bytes, or \code{NA} if not supported.
#' @details The function \code{get_ram} only supports 'MacOS', 'Windows', and 'Linux'. 'Solaris' or other platforms will return \code{NA}.
#' Here are the system commands used to detect memory limits:
#' \describe{
#' \item{'Windows'}{Uses command \code{'wmic.exe'} in the 'Windows' system folder. Notice this command-line tool might not exist on all 'Windows' machines. \code{get_ram} will return \code{NA} if it cannot locate the command-line tool.}
#' \item{'MacOS'}{Uses command \code{'sysctl'} located at \code{'/usr/sbin/'} or \code{'/sbin/'}. Alternatively, you can edit the environment variable \code{'PATH'} to include the command-line tools if \code{'sysctl'} is missing. \code{get_ram} will return \code{NA} if it cannot locate \code{'sysctl'}.}
#' \item{'Linux'}{Uses the file \code{'/proc/meminfo'}, possibly the first entry \code{'MemTotal'}. If the file is missing or entry \code{'MemTotal'} cannot be located, \code{get_ram} will return \code{NA}.}
#' }
#'
#' @examples
#'
#' get_ram()
#'
#' @export
get_ram <- function(){
  # .Defunct("memory.size", "utils")
  os <- get_os()
  if(os == 'windows'){
    return(get_ram_windows())
  }
  if(os == 'darwin'){
    return(get_ram_osx())
  }
  if(os == 'linux'){
    return(get_ram_linux())
  }
  return(NA)
}

#' @rdname dipsaus-defunct
#' @export
get_cpu <- function(){
  os <- get_os()

  if(os == "darwin"){
    .Defunct(msg = paste(
      "'get_cpu' is defunct due to its inconsistent results. Please use the following system commands to get CPU information:",
      "Windows: wmic cpu get name",
      "macOS  : sysctl -n machdep.cpu.brand_string",
      "Linux  : awk '/model name/' /proc/cpuinfo",
      "Solaris: psrinfo -vp",
      sep = "\n"
    ))
  }
}




#' @title Provides Unique Session ID According to Current R Session
#' @param pid R session process ID, default is \code{Sys.getpid()}
#' @param attributes whether to append data used to calculate
#' ID as attributes, default is false
#' @return Character string
#' @export
session_uuid <- local({
  uuids <- list()

  function (pid = Sys.getpid(), attributes = FALSE) {
    pidstr <- as.character(pid)
    uuid <- uuids[[pidstr]]
    if (!is.null(uuid)) {
      if (!attributes)
        attr(uuid, "source") <- NULL
      return(uuid)
    }
    info <- Sys.info()
    host <- Sys.getenv(c("HOST", "HOSTNAME", "COMPUTERNAME"))
    host <- host[nzchar(host)]
    host <- if (length(host) == 0L)
      info[["nodename"]]
    else host[1L]
    oseed <- .GlobalEnv$.Random.seed
    on.exit({
      if (is.null(oseed)) {
        rm(list = ".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      } else {
        .GlobalEnv$.Random.seed <- oseed
      }
    })
    info <- list(host = host, info = info, pid = pid, time = Sys.time(),
                 random = sample(.Machine$integer.max, size = 1L, replace = FALSE))
    uuid <- digest::digest(info)
    attr(uuid, "source") <- info
    uuids[[pidstr]] <<- uuid
    if (!attributes)
      attr(uuid, "source") <- NULL
    uuid
  }
})



