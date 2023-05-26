#' @title Create or Unlock a Lock
#' @description A wrapper for 'synchronicity' package, but user can
#' interrupt the lock procedure anytime, and don't have to worry about
#' whether the lock exists or not.
#' @param name character, the locker's name, must be only letters and digits
#' @param timeout numeric, seconds to wait for the locker to lock or unlock
#' @return Logical, whether the operation succeed.
#' @param exclusive ignored
#' @name lock
#' @examples
#'
#' # Clear existing locks
#' dipsaus::dipsaus_resetlocks()
#'
#' # unlock to prepare for the example
#' dipsaus_unlock('testlocker', timeout = 0.01)
#'
#' # Create a locker, return TRUE
#' lock_success = dipsaus_lock('testlocker')
#' if(lock_success){
#'   cat2('testlocker has been locked')
#' }
#'
#' # test whether locker has been locked
#' lock_success = dipsaus_lock('testlocker', timeout = 0.01)
#' if(!lock_success){
#'   cat2('attempt to lock testlocker failed')
#' }
#'
#' # unlock
#' dipsaus_unlock('testlocker', timeout = 0.01)
#'
#' # clean up
#' dipsaus::dipsaus_resetlocks()
#'
#'
NULL


locker_key <- function(name, set_default = FALSE, unset = FALSE) {
  if(length(name) != 1) {
    return()
  }
  .locker_keys <- get0(".locker_keys")
  name <- sprintf("%s-%s", session_uuid(), name)
  lkey <- .locker_keys$get(name)
  if(unset) {
    .locker_keys$remove(name)
  } else if(is.null(lkey) && set_default) {
    lkey <- sprintf("%s%s", session_uuid(), rand_string(20))
    lkey <- sprintf("%s-%s", lkey, digest(lkey))
    .locker_keys$set(name, lkey)
  }
  lkey
}

validate_lock <- function(lock_path) {
  suppressWarnings({
    try({
      lkey <- readLines(lock_path)
      lkey <- strsplit(lkey, "-")[[1]]
      if(length(lkey) == 2 && identical(digest(lkey[[1]]), lkey[[2]])) {
        return(TRUE)
      }
    }, silent = TRUE)
  })
  return(FALSE)
}

validate_key <- function(lock_path, key) {
  suppressWarnings({
    try({
      lkey <- readLines(lock_path)
      if(identical(lkey, key)) { return(TRUE) }
    }, silent = TRUE)
  })
  return(FALSE)
}

#' @rdname lock
#' @export
dipsaus_lock <- function(name, timeout = 10, exclusive = TRUE){
  timeout <- as.numeric(timeout)
  if(is.na(timeout)) {
    timeout <- 0
  }
  name <- gsub('[^a-zA-Z0-9]', "", name)
  lkey <- locker_key(name, set_default = TRUE)

  root_path <- file.path(R_user_dir("dipsaus", which = "cache"), "file_locks")
  lock_path <- file.path(root_path, name)

  if(!dir.exists(root_path)) {
    dir.create(root_path, showWarnings = FALSE, recursive = TRUE)
  }

  lock_valid <- validate_lock(lock_path)
  if(!lock_valid){
    writeLines(lkey, con = lock_path)
    Sys.sleep(0.001)

    lock_valid <- validate_lock(lock_path)

    if(!lock_valid) {
      writeLines(lkey, con = lock_path)
      Sys.sleep(0.001)
      lock_valid <- validate_lock(lock_path)
    }

    if(lock_valid && validate_key(lock_path, lkey)) {
      return(TRUE)
    }
  } else {
    try({
      if(validate_key(lock_path, lkey)) {
        return(TRUE)
      }
    }, silent = TRUE)
  }

  if(timeout <= 0) {
    return(FALSE)
  }

  # try to wait
  while (timeout > 0) {
    wait_time <- stats::runif(1, min = min(timeout, 0.1), max = min(timeout, 0.2))
    Sys.sleep(wait_time)
    timeout <- timeout - wait_time

    lock_valid <- validate_lock(lock_path)
    if(!lock_valid) {
      writeLines(lkey, con = lock_path)
      Sys.sleep(0.001)
      timeout <- timeout - 0.001
      lock_valid <- validate_lock(lock_path)
      if(lock_valid && validate_key(lock_path, lkey)) {
        return(TRUE)
      }
    } else {
      try({
        if(validate_key(lock_path, lkey)) {
          return(TRUE)
        }
      }, silent = TRUE)
    }

  }

  lock_valid <- validate_lock(lock_path)

  return(lock_valid && validate_key(lock_path, lkey))
}

#' @rdname lock
#' @export
dipsaus_unlock <- function(name, timeout = 10, exclusive = TRUE) {
  timeout <- as.numeric(timeout)
  if(is.na(timeout)) {
    timeout <- 0
  }
  name <- gsub('[^a-zA-Z0-9]', "", name)
  cache_dir <- R_user_dir("dipsaus", which = "cache")
  root_path <- file.path(cache_dir, "file_locks")
  lock_path <- file.path(root_path, name)

  unlocked <- function() {
    locker_key(name, set_default = FALSE, unset = TRUE)
    TRUE
  }

  if(!file.exists(lock_path)) { return(unlocked()) }

  locker_valid <- validate_lock(lock_path)
  if(!locker_valid) { return(TRUE) }

  lkey <- locker_key(name, set_default = FALSE)

  try({
    if(validate_key(lock_path, lkey)) {
      unlink(lock_path)
      remove_empty_dir(cache_dir, recursive = TRUE)
      return(unlocked())
    }
  }, silent = TRUE)

  while(timeout > 0) {
    wait_time <- stats::runif(1, min = min(timeout, 0.1), max = min(timeout, 0.2))
    Sys.sleep(wait_time)
    timeout <- timeout - wait_time

    locker_valid <- validate_lock(lock_path)
    if(!locker_valid) { return(unlocked()) }

    try({
      if(validate_key(lock_path, lkey)) {
        unlink(lock_path)
        remove_empty_dir(cache_dir, recursive = TRUE)
        return(unlocked())
      }
    }, silent = TRUE)

  }


  return(FALSE)
}

#' @rdname lock
#' @export
dipsaus_resetlocks <- function(name) {
  cache_dir <- R_user_dir("dipsaus", which = "cache")
  root_path <- file.path(cache_dir, "file_locks")
  if(missing(name)) {
    unlink(root_path, recursive = TRUE, force = TRUE)
  } else {
    name <- gsub('[^a-zA-Z0-9]', "", name)
    lock_path <- file.path(root_path, name)
    lock_path <- lock_path[file.exists(lock_path)]
    for(f in lock_path) {
      unlink(lock_path, force = TRUE)
    }
  }
  remove_empty_dir(cache_dir, recursive = TRUE)
}

# .locks <- local({
#   locks <- NULL
#
#   create_lock <- function(name, create = TRUE){
#     if(is.null(locks)){
#       locks <<- fastmap::fastmap()
#     }
#
#     name <- stringr::str_remove_all(name, '[^a-zA-Z0-9]')
#
#     if( locks$has(name) ){
#       item <- locks$get(name)
#       if(is.list(item)) {
#         return(item$mutex)
#       } else {
#         return(NULL)
#       }
#     } else if( !create ){
#       return(NULL)
#     } else {
#
#       # C++ errors might not be captured?
#       # capture.output({
#         mutex <- tryCatch({
#           cat("boost.mutex create = TRUE")
#           re <- synchronicity::boost.mutex(sharedName = name,
#                                            timeout = 0.01,
#                                            create = TRUE)
#           force(re)
#         }, error = function(e){
#           cat("boost.mutex create = FALSE")
#           synchronicity::boost.mutex(sharedName = name,
#                                      timeout = 0.01,
#                                      create = FALSE)
#           return(mutex)
#         })
#         locks$set(name, list(
#           mutex = mutex,
#           nlocks = 0
#         ))
#       # },type = 'message')
#     }
#
#   }
#
#   lock <- function(name, exclusive = TRUE, timeout = 10){
#     name <- stringr::str_remove_all(name, '[^a-zA-Z0-9]')
#     l <- create_lock(name)
#     if(exclusive){
#       f <- synchronicity::lock
#     }else{
#       f <- synchronicity::lock.shared
#     }
#     timeout <- max(timeout, 0.01)
#
#     # synchronicity lock doesn't allow interrupts
#     while (timeout > 0) {
#       locked <- f(l, timeout = 0.5)
#       if(locked){
#         locks[[name]][['nlocks']] <- locks[[name]][['nlocks']] + 1
#         return(locked)
#       }
#       Sys.sleep(min(timeout, 0.5))
#       timeout <- timeout - 0.5
#     }
#     return(FALSE)
#   }
#
#   unlock <- function(name, exclusive = TRUE, timeout = 10){
#     name <- stringr::str_remove_all(name, '[^a-zA-Z0-9]')
#     l <- create_lock(name, create = FALSE)
#     if(is.null(l)){
#       return(TRUE)
#     }
#     if(exclusive){
#       u <- synchronicity::unlock
#     }else{
#       u <- synchronicity::unlock.shared
#     }
#     unlocked <- u(l, timeout = timeout)
#     if(unlocked){
#       locks[[name]][['nlocks']] <- locks[[name]][['nlocks']] - 1
#       if(locks[[name]][['nlocks']] < 0){
#         # remove lock and free up space
#         rm(list = name, envir = locks)
#         gc()
#       }
#     }
#     unlocked
#   }
#
#   return(list(
#     create_lock = create_lock,
#     lock = lock,
#     unlock = unlock
#   ))
#
# })






# dipsaus_lock <- .locks$lock

# dipsaus_unlock <- .locks$unlock
