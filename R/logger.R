session_log <- local({
  env <- environment()
  masterpid <- NULL
  uuid <- NULL
  log <- NULL
  log_dir <- getOption('dipsaus.logdir', '~/rave_data/log')
  i <- 0
  n <- 0
  part <- 0


  finalize <- function(){
    if(is.null(masterpid)){ return() }
    if(running_master_session()){
      sess_log_dir <- file.path(log_dir, masterpid)
      if(dir.exists(sess_log_dir)){
        unlink(sess_log_dir, recursive = TRUE, force = TRUE)
      }
    }else{
      write_log()
    }
  }

  initialize <- function(){
    if(!is.null(masterpid)){ return(FALSE) }
    masterpid <<- Sys.getpid()
    sess_log_dir <- file.path(log_dir, masterpid)
    dir_create(sess_log_dir)
    return(TRUE)
  }

  write_log <- function(...){
    if(is.null(uuid)){ return() }
    # No log, no save
    if(n == 0 || i >= n){ return() }

    # Master session exit, nosave
    if(!dir.exists(file.path(log_dir, masterpid))){ return() }

    # Write log to log file
    fpath <- file.path(log_dir, masterpid, paste0(uuid, '_part', part, '.log'))

    s <- unlist(as.list(log))
    writeLines(s, con = fpath)

    i <<- n

  }

  ensure <- function(){
    initialize()
    sid <- session_uuid()
    if(!isTRUE(uuid == sid)){
      uuid <<- sid
      log <<- new.env(parent = emptyenv())
      n <<- 0
      i <<- 0
      part <<- 0
    }
  }

  logger <- function(msg){
    ensure()

    n <<- n + 1
    if(n > getOption('dipsaus.maxlogline', 1000)){
      write_log()
      n <<- 1
      part <<- part + 1
      i <<- 0
      log <<- new.env(parent = emptyenv())
    }
    log[[as.character(n)]] <- sprintf('[%s] %s',
                                      strftime(Sys.time(), usetz = TRUE),
                                      msg)

  }

  get_dir <- function(){
    if(is.null(masterpid)){ return(NULL) }
    file.path(log_dir, masterpid)
  }

  return(env)
})



logger <- function(msg, flush = FALSE){
  session_log$logger(msg)
  if(flush){
    session_log$write_log()
  }
}

packup_logger <- function(path = tempfile(fileext = '.zip')){
  session_log$write_log()
  dir <- session_log$get_dir()
  if(is.null(dir) || !dir.exists(dir)){ return() }
  path <- file_create(path)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(dir)
  if(file.exists(path)){
    unlink(path)
  }
  utils::zip(path, files = list.files('.', pattern = '\\.log$',
                                      full.names = FALSE, recursive = TRUE,
                                      all.files = FALSE, ignore.case = TRUE))
  return(path)
}




