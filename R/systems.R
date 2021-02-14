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

#' Get Memory Size
#' @return System RAM in bytes, or \code{NA} is error occurs.
#'
#' @examples
#'
#' get_ram()
#'
#' @export
get_ram <- function(){
  os <- get_os()
  ram <- 128*1024^3
  safe_ram <- function(e){
    NA
  }

  ram <- tryCatch({
    switch (
      os,
      'darwin' = {
        ram <- safe_system2(
          "sysctl",
          "hw.memsize",
          stdout = TRUE,
          onFound = function(ram) {
            substring(ram, 13)
          }
        )
        if(is.na(ram)){
          ram <- safe_system2("top", c("-l", "1", "-s", "0"), stdout = TRUE, onFound = function(s){
            s <- s[stringr::str_detect(s, "PhysMem")][[1]]
            m <- stringr::str_match(s, "PhysMem[: ]+([0-9]+)([gGtTmM])")
            s <- as.numeric(m[[2]])
            u <- stringr::str_to_lower(m[[3]])
            if(u == 'm'){ s <- s * 1024^2 }
            if(u == 'g'){ s <- s * 1024^3 }
            if(u == 't'){ s <- s * 1024^4 }
            s
          })
        }
        ram
      },
      'linux' = {
        if(Sys.which("awk") == ""){
          ram <- NA
        } else {
          ram <- safe_system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE)
          ram <- as.numeric(ram) * 1024
        }
      },
      'solaris' = {
        if(Sys.which("prtconf") == ""){
          ram <- NA
        } else {
          ram <- safe_system("prtconf | grep Memory", intern = TRUE)
          ram <- stringr::str_trim(ram)
          ram <- stringr::str_split(ram, '[ ]+')[[1]][3:4]

          power <- match(ram[2], c("kB", "MB", "GB", "TB", "Kilobytes", "Megabytes", "Gigabytes", "Terabytes"))
          ram <- as.numeric(ram[1]) * 1024^(1 + (power-1) %% 4)
        }
      },
      'windows' = {
        if(Sys.which("wmic") == ""){
          ram <- NA
        } else {
          ram <- safe_system("wmic MemoryChip get Capacity", intern = TRUE)[-1]
          ram <- stringr::str_trim(ram)
          ram <- ram[nchar(ram) > 0]
          ram <- sum(as.numeric(ram))
        }
      }, {
        ram <- NA
      }
    )
    ram
  }, error = safe_ram, warning = safe_ram)
  ram <- as.numeric(ram)
  ram
}


#' Get CPU Chip-set Information
#' @return a list of vendor ID and CPU model name
#' @examples
#'
#' get_cpu()
#'
#' @export
get_cpu <- function(){
  os <- get_os()

  safe_cpu <- function(...){
    list(
      vendor_id = NA,
      model_name = NA
    )
  }
  cpu <- tryCatch({
    switch (
      os,
      'darwin' = list(
        vendor_id = safe_system2(
          "sysctl",
          c("-n", "machdep.cpu.vendor"),
          stdout = TRUE
        ),
        model_name = safe_system2("sysctl", c("-n", "machdep.cpu.brand_string"), stdout = TRUE)
      ),
      'linux' = list(
        vendor_id = safe_system2(
          "awk", c("'/vendor_id/'", "/proc/cpuinfo"),
          stdout = TRUE,
          onFound = function(s){
            gsub("vendor_id\t: ", "", unique(s))
        }),
        model_name = safe_system2(
          "awk", c("'/model name/'", "/proc/cpuinfo"),
          stdout = TRUE,
          onFound = function(s){
            gsub("model name\t: ", "", unique(s))
          })
      ),
      'windows' = list(
        model_name = safe_system2("wmic", "cpu get name", stdout = TRUE, onFound = function(s){ s[[2]] }),
        vendor_id = safe_system2("wmic", "cpu get manufacturer", stdout = TRUE, onFound = function(s){ s[[2]] })
      ),
      list(
        vendor_id = NA,
        model_name = NA
      )
    )
  }, error = safe_cpu, warning = safe_cpu)
  cpu
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



