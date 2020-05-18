# System utilities


get_os <- function(){
  os <- R.version$os
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

#' Get Memory Size
#' @return numeric in Bytes how big your system RAM is
#' @export
get_ram <- function(){
  os <- get_os()
  ram <- 128*1024^3
  safe_ram <- function(e){
    suppressWarnings({
      min(utils::memory.limit(), 128*1024^3)
    })
  }

  ram <- tryCatch({
    switch (
      os,
      'darwin' = {
        ram <- substring(system("sysctl hw.memsize", intern = TRUE), 13)
      },
      'linux' = {
        ram <- system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE)
        ram <- as.numeric(ram) * 1024
      },
      'solaris' = {
        ram <- system("prtconf | grep Memory", intern = TRUE)
        ram <- stringr::str_trim(ram)
        ram <- stringr::str_split(ram, '[ ]+')[[1]][3:4]

        power <- match(ram[2], c("kB", "MB", "GB", "TB", "Kilobytes", "Megabytes", "Gigabytes", "Terabytes"))
        ram <- as.numeric(ram[1]) * 1024^(1 + (power-1) %% 4)
      },
      'windows' = {
        ram <- system("wmic MemoryChip get Capacity", intern = TRUE)[-1]
        ram <- stringr::str_trim(ram)
        ram <- ram[nchar(ram) > 0]
        ram <- sum(as.numeric(ram))
      }, {
        ram <- min(utils::memory.limit(), 128*1024^3)
      }
    )
    ram
  }, error = safe_ram, warning = safe_ram)
  ram <- as.numeric(ram)
  ram
}


#' Get CPU Chip-set Information
#' @return a list of vendor ID and CPU model name
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
        vendor_id = system("sysctl -n machdep.cpu.vendor", intern = TRUE),
        model_name = system("sysctl -n machdep.cpu.brand_string", intern = TRUE)
      ),
      'linux' = list(
        vendor_id = gsub("vendor_id\t: ", "", unique(system("awk '/vendor_id/' /proc/cpuinfo", intern = TRUE))),
        model_name = gsub("model name\t: ", "", unique(system("awk '/model name/' /proc/cpuinfo", intern = TRUE)))
      ),
      'windows' = list(
        model_name = system("wmic cpu get name", intern = TRUE)[2],
        vendor_id = system("wmic cpu get manufacturer", intern = TRUE)[2]
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



