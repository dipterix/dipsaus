#' @title Create a group of named graphic devices
#' @param ... named expressions to launch devices
#' @param env environment to evaluate expressions
#' @return A list of functions to query, control, and switch between devices
#' @examples
#' \dontrun{ ## Unix-specific example
#'
#' # Create multiple named devices
#' devs <- dev_create(line = X11(), points = x11())
#'
#' # switch to device named "points"
#'
#' devs$dev_which('points')
#' plot(1:10)
#'
#' # switch to "line" device
#' devs$dev_switch('line')
#' plot(1:100, type='l')
#'
#' # Create another group with conflict name
#' dev_another <- dev_create(line = X11())
#'
#' # Query device name with 'line'
#' dev_another$dev_which('line')  # 4
#' devs$dev_which('line')  # 2, doesn't conflict with the new groups
#'
#' dev.list()
#' # close one or more device
#' dev_another$dev_off('line')
#' dev.list()
#'
#' # close all devices
#' devs$dev_off()
#' dev.list()
#'
#' }
#' @export
dev_create <- function(..., env = parent.frame()){
  quos = rlang::quos(..., .ignore_empty = 'all', .named = TRUE)
  devs <- get0(".Devices", envir = baseenv(), ifnotfound = list("null device"), inherits = FALSE)
  n_devs <- unlist(devs)
  n_devs <- sum(n_devs != "")

  private_id = rand_string(6)

  nms = names(quos)

  # create devices
  for(ii in seq_along(quos)){
    quo = quos[[ii]]
    rlang::eval_tidy(quo, env = env)
    devs <- get0(".Devices", envir = baseenv(), ifnotfound = list("null device"), inherits = FALSE)
    n <- sum(unlist(devs) != "")
    if( n > n_devs ){
      attr(devs[[n_devs + 1]], 'dipsaus_dev_name') <- paste0(private_id, nms[[ii]])
    }
    # assign back to baseenv
    assign('.Devices', devs, envir = baseenv())
    n_devs = n
  }

  dev_which <- function(dev_name){
    stopifnot2(length(dev_name) == 1 && is.character(dev_name), msg = 'dev_name must be a string')
    devs <- get0(".Devices", envir = baseenv(), ifnotfound = list("null device"), inherits = FALSE)
    for(ii in seq_along(devs)){
      if(ii == 1) next()
      dev = devs[[ii]]
      if( dev != '' ){
        nm <- attr(dev, 'dipsaus_dev_name')
        if(isTRUE(paste0(private_id, dev_name) == nm)){
          return(ii)
        }
      }
    }
    return(NA)
  }

  dev_switch <- function(dev_name){
    ii = dev_which(dev_name)
    if(!is.na(ii)){
      grDevices::dev.set(ii)
    }

  }

  dev_names <- function(){
    # only returns active device names
    devs <- get0(".Devices", envir = baseenv(), ifnotfound = list("null device"), inherits = FALSE)
    re <- lapply(devs, function(dev){
      dev_name = attr(dev, 'dipsaus_dev_name')
      id = stringr::str_sub(dev_name, end = 6)
      if(length(id) == 1 && isTRUE(id == private_id)){
        return(stringr::str_sub(dev_name, start = 7))
      }else{
        NULL
      }
    })
    unlist(re)
  }

  dev_off <- function(devnames){
    if(missing(devnames)){
      devnames <- dev_names()
    }
    for(nm in devnames){
      ii <- dev_which(nm)
      if(!is.na(ii)){
        grDevices::dev.off(ii)
      }
    }
    invisible()
  }

  return(list(
    dev_which = dev_which,
    dev_switch = dev_switch,
    dev_names = dev_names,
    dev_off = dev_off
  ))

}
