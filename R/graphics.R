#' @title Create a group of named graphic devices
#' @param ... named expressions to launch devices
#' @param env environment to evaluate expressions
#' @param attributes named list; names correspond to device names and values
#' are attributes to set to the devices
#' @param dev which device to search for attributes
#' @param which which attribute to obtain
#' @param ifnotfound value to return if attribute is not found
#' @return A list of functions to query, control, and switch between devices
#' @examples
#' \dontrun{ ## Unix-specific example
#'
#' # Create multiple named devices, setting attributes to the second graph
#' devs <- dev_create(
#'   line = X11(), points = x11(),
#'   attributes = list(points = list(pch = 16))
#' )
#'
#' # switch to device named "points"
#'
#' devs$dev_which('points')
#'
#' # Plot points, with pch given as preset
#' plot(1:10, pch = get_dev_attr(which = 'pch', ifnotfound = 1))
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
#' @name graphic-devices
NULL

#' @rdname graphic-devices
#' @export
dev_create <- function(..., env = parent.frame(), attributes = list()){
  quos <- rlang::quos(..., .ignore_empty = 'all', .named = TRUE)
  devs <- get0(".Devices", envir = baseenv(), ifnotfound = list("null device"), inherits = FALSE)
  n_devs <- unlist(devs)
  n_devs <- sum(n_devs != "")

  private_id <- rand_string(6)

  nms <- names(quos)

  # create devices
  for(ii in seq_along(quos)){
    quo <- quos[[ii]]
    rlang::eval_tidy(quo, env = env)
    devs <- get0(".Devices", envir = baseenv(), ifnotfound = list("null device"), inherits = FALSE)
    n <- sum(unlist(devs) != "")
    if( n > n_devs ){
      attr(devs[[n_devs + 1]], 'dipsaus_dev_name') <- paste0(private_id, nms[[ii]])
      opt <- attributes[[nms[[ii]]]]
      if(is.null(opt)) {
        opt <- list()
      }
      attr(devs[[n_devs + 1]], 'dipsaus_dev_attr') <- opt
    }
    # To CRAN Team who reaches this line
    #
    # The following line violates CRAN policy, as pointed by
    # Kurt Hornik <Kurt.Hornik@wu.ac.at> and
    # Paul Murrell <paul@stat.auckland.ac.nz>
    # on 03/10/2021, that ".Devices" is altered in baseenv()
    #
    # However, currently there is no substitute as the default grDevice package
    # does not allow labeling the devices. One of the issues is that device
    # list will be altered when user close the graphics and create new ones.
    # If some one close one (A) and open another device (B)
    # it is possible that B is mistaken as A, resulting in 2 figures blending
    # into each other. There are other issues, but this one is significant.
    #
    # After discussion, I was allowed to keep my code until native supports are
    # provided
    #
    #
    # Original Email:
    #     >>>>> Paul Murrell writes:
    #
    #     > Hi
    #     > On 15/03/21 10:58 am, Zhengjia Wang wrote:
    #       >> Hi, Paul,
    #       >> Thanks for clarification. Allowing reference by label is all I was
    #       >> asking for. dipsaus only adds one attribute as the label to each device,
    #       >> and the grouping was done using the label prefix. But in general, there
    #       >> are many ways to group the devices as long as I can reference by labels.
    #
    #     > Ok.  That sounds good.  I will look at implementing something for you
    #     > to try out.
    #
    #     Perfect.
    #
    #       >> For now, would you mind if I still keep this function in the package
    #       >> until the new feature is published? There are some projects that use
    #       >> this function to export graphs to multiple files, and I don't want them
    #       >> to get headaches for the sudden changes. However, I can always add a
    #       >> warning to dev_create so that other CRAN packages know to avoid
    #       >> importing this function. Once I can reference devices by labels in the
    #       >> newer versions of R, I'll fix this issue ASAP.
    #
    #     > I'll leave that up to the CRAN guys.
    #
    #     Yep, that should be fine for now.
    #
    #     Best
    #     -k
    #
    assign('.Devices', devs, envir = baseenv())
    n_devs <- n
  }

  dev_which <- function(dev_name){
    stopifnot2(length(dev_name) == 1 && is.character(dev_name), msg = 'dev_name must be a string')
    devs <- get0(".Devices", envir = baseenv(), ifnotfound = list("null device"), inherits = FALSE)
    for(ii in seq_along(devs)){
      if(ii == 1) next()
      dev <- devs[[ii]]
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
    ii <- dev_which(dev_name)
    if(!is.na(ii)){
      grDevices::dev.set(ii)
      return(TRUE)
    }
    return(FALSE)
  }

  dev_names <- function(){
    # only returns active device names
    devs <- get0(".Devices", envir = baseenv(), ifnotfound = list("null device"), inherits = FALSE)
    re <- lapply(devs, function(dev){
      dev_name <- attr(dev, 'dipsaus_dev_name')
      id <- stringr::str_sub(dev_name, end = 6)
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

  dev_attributes <- function(dev_name){
    ii <- dev_which(dev_name)
    if(!is.na(ii)){
      devs <- get0(".Devices", envir = baseenv(), ifnotfound = list("null device"), inherits = FALSE)
      return(attr(devs[[ii]], 'dipsaus_dev_attr'))
    }
    return(NULL)
  }

  re <- fastmap2()
  re$dev_which <- dev_which
  re$dev_switch <- dev_switch
  re$dev_names <- dev_names
  re$dev_off <- dev_off
  re$dev_attributes <- dev_attributes
  return(re)
}

#' @rdname graphic-devices
#' @export
get_dev_attr <- function(which, dev = grDevices::dev.cur(), ifnotfound = NULL){
  devs <- get0(".Devices", envir = baseenv(), ifnotfound = list("null device"), inherits = FALSE)
  if(length(devs) < dev){
    return(ifnotfound)
  }
  attrs <- attr(devs[[dev]], 'dipsaus_dev_attr')

  if(missing(which)){
    # return the whole attributes

    if(is.null(attrs)){
      return(ifnotfound)
    } else {
      return(attrs)
    }

  } else {

    # return the item within attr
    if(which %in% names(attrs)){
      return(attrs[[which]])
    } else {
      return(ifnotfound)
    }
  }
}
