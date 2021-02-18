
.locks <- local({
  locks <- NULL

  create_lock <- function(name, create = TRUE){
    if(is.null(locks)){
      locks <<- new.env(parent = emptyenv())
    }

    name <- stringr::str_remove_all(name, '[^a-zA-Z0-9]')


    if(!exists(name, envir = locks)){

      if(!create){
        return(NULL)
      }

      # C++ errors might not be captured?
      capture.output({
        mutex <- tryCatch({
          re <- synchronicity::boost.mutex(sharedName = name,
                                          timeout = 0.01,
                                          create = TRUE)
          force(re)
        }, error = function(e){
          synchronicity::boost.mutex(sharedName = name,
                                     timeout = 0.01,
                                     create = FALSE)
        })
      },type = 'message')



      locks[[name]] <- list(
        mutex = mutex,
        nlocks = 0
      )
      mutex
    }else{
      locks[[name]]$mutex
    }


  }

  lock <- function(name, exclusive = TRUE, timeout = 10){
    name <- stringr::str_remove_all(name, '[^a-zA-Z0-9]')
    l <- create_lock(name)
    if(exclusive){
      f <- synchronicity::lock
    }else{
      f <- synchronicity::lock.shared
    }
    timeout <- max(timeout, 0.01)

    # synchronicity lock doesn't allow interrupts
    while (timeout > 0) {
      locked <- f(l, timeout = 0.5)
      if(locked){
        locks[[name]][['nlocks']] <- locks[[name]][['nlocks']] + 1
        return(locked)
      }
      Sys.sleep(min(timeout, 0.5))
      timeout <- timeout - 0.5
    }
    return(FALSE)
  }

  unlock <- function(name, exclusive = TRUE, timeout = 10){
    name <- stringr::str_remove_all(name, '[^a-zA-Z0-9]')
    l <- create_lock(name, create = FALSE)
    if(is.null(l)){
      return(TRUE)
    }
    if(exclusive){
      u <- synchronicity::unlock
    }else{
      u <- synchronicity::unlock.shared
    }
    unlocked <- u(l, timeout = timeout)
    if(unlocked){
      locks[[name]][['nlocks']] <- locks[[name]][['nlocks']] - 1
      if(locks[[name]][['nlocks']] < 0){
        # remove lock and free up space
        rm(list = name, envir = locks)
        gc()
      }
    }
    unlocked
  }

  return(list(
    create_lock = create_lock,
    lock = lock,
    unlock = unlock
  ))

})


#' @title Create or Unlock a Lock
#' @description A wrapper for 'synchronicity' package, but user can
#' interrupt the lock procedure anytime, and don't have to worry about
#' whether the lock exists or not.
#' @param name character, the locker's name, must be only letters and digits
#' @param exclusive logical whether the locker is exclusive. True for write
#' access, False for read access. Default is true.
#' @param timeout numeric, seconds to wait for the locker to lock or unlock
#' @return Logical, whether the operation succeed.
#' @name lock
#' @examples
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
#'
NULL



#' @rdname lock
#' @export
dipsaus_lock <- .locks$lock

#' @rdname lock
#' @export
dipsaus_unlock <- .locks$unlock
