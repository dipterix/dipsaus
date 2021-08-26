#' @name dipsaus-rstudio-shortcuts
#' @title Register customized R code to 'RStudio' shortcuts
#' @description 'RStudio' keyboard shortcuts is handy, however, it is non-trivial
#' to set shortcuts that run customized code. The proposing functions
#' allow 10 customized R expressions to be registered. The first five
#' (1 to 5) are interactive shortcuts, the rest five
#' (6 to 10) are non-interactive.
#' @param which integer from 1 to 10, which keyboard shortcut to edit
#' @param txt an insertion/replacement shortcut to add
#' @param expr expression to run if shortcut is pressed
#' @param force whether to remove existing shortcut if the hot-key has been
#' registered
#' @param quoted whether \code{expr} is quoted, default is false
#' @details There are two steps to register an 'RStudio' keyboard shortcut.
#'
#' 1. Please enable the shortcuts by opening
#' \code{'Tools' > 'Modify Keyboard Shortcuts'} in 'RStudio' menu bar;
#' search and locate add-in items starting with 'Dipsaus'; register hot-keys
#' of your choices, and then save. It is recommended that these
#' keys are \code{'Alt' + 1} to \code{'Alt' + 0}. On Apple, 'Alt' is
#' equivalent to 'option' key.
#'
#' 2. run \code{rs_add_insertion_shortcut} or \code{rs_add_shortcut} to
#' customize the behaviors of each shortcuts; see Examples.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Need to run in RStudio
#' # Please read the Section 'Details' carefully
#'
#' # --------------------------------------------
#'
#' # I assume the shortcuts are Alt+1,2,...,9,0,
#' # corresponding to shortcuts 1 - 10
#'
#' # Adds an insertion to Alt+9
#' rs_add_insertion_shortcut(9, " %?<-% ", force = TRUE)
#' # restart RStudio and try `Alt+9`
#'
#'
#' # Adds an expression to Alt+1
#' rs_add_shortcut(1, {
#'   expr <- sprintf("system.time({\n%s\n})\n",
#'                   rstudioapi::selectionGet()$value)
#'   cat(expr)
#'   eval(parse(text = expr))
#' }, force = TRUE)
#'
#' # Select any valid R code and press Alt+1
#'
#'
#' }
#'
NULL

shortcut_settings_path <- function(
  which = c("01", "02", "03", "04", "05", "06",
            "07", "08", "09", "10"),
  check = FALSE
){
  which <- match.arg(which)
  dir <- file.path(R_user_dir(package = 'dipsaus', which = "config"), "shortcuts")
  fname <- file.path(dir, sprintf("%s.R", which))
  if(!dir.exists(dir)) {
    if(check){
      dir_create(dir)
    }
  }
  fname
}

path_addins_json <- function (check = FALSE) {
  if( get_os() == "windows" ){
    dir <- file.path("~", "AppData", "Roaming", "rstudio", "keybindings")
  } else {
    dir <- file.path("~", ".config", "rstudio", "keybindings")
  }
  re <- file.path(dir, "addins.json")
  if( check ){
    if(!dir.exists(dir)){
      dir_create(dir)
    }
    if(!file.exists(re)){
      writeLines('{}', re)
    }
  }
  normalizePath(re, mustWork = check)
}

#' @rdname dipsaus-rstudio-shortcuts
#' @export
rs_add_insertion_shortcut <- function(which, txt, force = FALSE){
  expr <- bquote({
    cts <- rstudioapi::getActiveDocumentContext()
    rstudioapi::selectionSet(.(txt), cts$id)
    invisible()
  })
  rs_add_shortcut(which, expr, force, quoted = TRUE)
}

#' @rdname dipsaus-rstudio-shortcuts
#' @export
rs_add_shortcut <- function(which, expr, force = FALSE, quoted = FALSE){
  if(!quoted){
    expr <- substitute(expr)
  }
  which <- as.integer(which)
  stopifnot2(isTRUE(which >= 1 && which <= 10),
             msg = "`which` must be an integer from 1 to 10")
  if(which < 10){
    key <- sprintf("Alt+%d", which)
    which <- sprintf("0%d", which)
  } else {
    which <- sprintf("%d", which)
    key <- "Alt+0"
  }
  fname <- shortcut_settings_path(which, check = TRUE)
  # replace fname
  writeLines(deparse(expr), fname)

  # register shortcut
  addin <- path_addins_json(check = TRUE)
  file.copy(addin, sprintf('%s.bak', addin))
  li <- jsonlite::read_json(addin)
  nm <- sprintf("dipsaus::shortcut%s", which)

  li[[nm]] <- NULL
  nms <- names(li)
  for(ii in seq_along(li)){
    if(isTRUE(li[[ii]] == key)){
      if(force) {
        # remove this key
        li[[ii]] <- NULL
        break
      } else {
        warning("A shortcut has already using the key. Please manually edit the key in RStudio or use `force`=TRUE\n  - ", nms[[ii]])
        return(invisible())
      }
    }
  }
  li[[nm]] <- key

  jsonlite::write_json(li, addin, auto_unbox = TRUE, pretty = TRUE)
  if( rs_avail() ){
    try({
      rstudioapi::executeCommand("modifyKeyboardShortcuts", quiet = FALSE)
      rstudioapi::showDialog("Notification", sprintf("Code generated. Please filter and register <p>&nbsp;&nbsp;&nbsp;&nbsp;<strong>Dipsaus shortcut %s</strong></p>in the menu, apply changes, and restart RStudio", which))
    }, silent = TRUE)
  } else {
    message("Shortcut added, please edit the shortcut and restart RStudio")
  }
}

#' @rdname dipsaus-rstudio-shortcuts
#' @export
rs_remove_shortcut <- function(which){
  which <- as.integer(which)
  if(is.na(which)){ return(FALSE) }
  if(which < 1 || which > 10){ return(FALSE) }
  if(which < 9) {
    which <- sprintf("0%d", which)
  } else {
    which <- sprintf("%d", which)
  }
  addin <- path_addins_json(check = FALSE)
  if(!file.exists(addin)){ return(TRUE) }
  file.copy(addin, sprintf('%s.bak', addin))

  li <- jsonlite::read_json(addin)
  nm <- sprintf("dipsaus::shortcut%s", which)
  if(!nm %in% names(li)){ return(TRUE) }
  li[[nm]] <- NULL
  if(!length(li)){
    writeLines("{}", addin)
  } else {
    jsonlite::write_json(li, addin, auto_unbox = TRUE, pretty = TRUE)
  }
  message("Shortcut removed, please restart RStudio")
  return(invisible(TRUE))
}

run_shortcut <- function(which){
  fname <- shortcut_settings_path(which)
  if(!file.exists(fname)){
    warning("Shortcut ", which , " has not been set up yet. Please read ",
            sQuote('help("rs_add_shortcut")'), " to set up shortcuts.")
    return(invisible())
  }
  expr <- parse(file = fname)
  eval(expr, envir = new.env(parent = .GlobalEnv))
}

shortcut01 <- function(){
  run_shortcut("01")
}

shortcut02 <- function(){
  run_shortcut("02")
}

shortcut03 <- function(){
  run_shortcut("03")
}

shortcut04 <- function(){
  run_shortcut("04")
}

shortcut05 <- function(){
  run_shortcut("05")
}

shortcut06 <- function(){
  run_shortcut("06")
}

shortcut07 <- function(){
  run_shortcut("07")
}

shortcut08 <- function(){
  run_shortcut("08")
}

shortcut09 <- function(){
  run_shortcut("09")
}

shortcut10 <- function(){
  run_shortcut("10")
}

