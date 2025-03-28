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
#' @param env environment to debug code; default is global environment
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
#' Function \code{rs_quick_debug} provides quick way to debug a script or
#' function without messing up the code. The script only works in 'RStudio'.
#' When executing the quick-debug function, the cursor context will be
#' automatically resolved and nearest debugging code blocks will be searched
#' and executed.
#' To enable this feature, add a line with \code{"# DIPSAUS: DEBUG START"} in
#' your code, followed by debugging code blocks in comments. The script will
#' figure it out. Since the 'RStudio' context will be obtained when executing
#' the function, it is recommended to add this function to your shortcuts.
#' By default, if the shortcut-1 is unset, this function will be executed.
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
#' # Adds an expression to Alt+2
#' rs_add_shortcut(2, {
#'   expr <- sprintf("system.time({\n%s\n})\n",
#'                   rstudioapi::selectionGet()$value)
#'   cat(expr)
#'   eval(parse(text = expr))
#' }, force = TRUE)
#'
#' # Select any valid R code and press Alt+1
#'
#' # --------------------------------------------
#'
#' # run this to set your shortcut (one-time setup)
#' rs_add_shortcut(1, { dipsaus::rs_quick_debug() })
#'
#' # Add debug feature: insert the following comment anywhere in your code
#' # You may open a new script in the RStudio
#'
#' # DIPSAUS: DEBUG START
#' # message("Debugging...")
#' # a <- 1
#' # print(a)
#' # message("Finished")
#'
#'
#' # Place your cursor here, press the shortcut key
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

  fname <- shortcut_settings_path(which)
  if(file.exists(fname)){
    unlink(fname)
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

#' @rdname dipsaus-rstudio-shortcuts
#' @export
rs_show_shortcut <- function(which) {
  if(!is.character(which)){
    if(which < 10) {
      which <- sprintf("0%s", which)
    } else {
      which <- as.character(which)
    }
  }
  fname <- shortcut_settings_path(which)
  if(!file.exists(fname)){
    warning("Shortcut ", which , " has not been set up yet. Please read ",
            sQuote('help("rs_add_shortcut")'), " to set up shortcuts.")
    return(invisible())
  }
  cat(readLines(fname), "", sep = "\n")
}

run_shortcut <- function(which){
  fname <- shortcut_settings_path(which)
  if(!file.exists(fname)){
    warning("Shortcut ", which , " has not been set up yet. Please read ",
            sQuote('help("rs_add_shortcut")'), " to set up shortcuts.")
    return(invisible())
  }
  expr <- parse(file = fname)
  eval(expr, envir = new.env(parent = globalenv()))
}

shortcut01 <- function(){
  fname <- shortcut_settings_path("01")
  if(!file.exists(fname)){
    rs_quick_debug()
  } else {
    run_shortcut("01")
  }
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

#' @rdname dipsaus-rstudio-shortcuts
#' @export
rs_quick_debug <- function(env = globalenv()) {
  # ----- DIPSAUS: DEBUG START--------
  # ctx <- rstudioapi::getActiveDocumentContext()
  # row <- ctx$selection[[1]]$range$start[1]
  # sub <- ctx$contents[seq_len(row)]
  # sub <- trimws(sub)

  # test
  ctx <- rstudioapi::getActiveDocumentContext()
  row <- ctx$selection[[1]]$range$start[1]
  sub <- ctx$contents[seq_len(row)]
  sub <- trimws(sub)
  # Find latest debug start
  debug_start <- which(grepl("^#[ -]{0, }DIPSAUS[^a-zA-Z0-9]{0,}DEBUG START", sub))
  if(!length(debug_start)) { return() }
  debug_start <- debug_start[[length(debug_start)]]
  # find the end of comment
  sub <- ctx$contents[seq(debug_start, length(ctx$contents))]
  sub <- trimws(sub)

  # check format: is it an R script?
  script_type <- "r"
  if( grepl("\\.(cpp|h|c)$", ctx$path, ignore.case = TRUE) ) {
    script_type <- "cpp"
  }

  remove_comments <- TRUE
  if( script_type %in% c("cpp") ) {

    # find */
    debug_end <- which(grepl("[*]{1,}/", sub))
    if(length(debug_end)) {
      debug_end <- debug_end[[1]]

      idx <- deparse_svec(seq_len(debug_end - 1))
      remove_comments <- FALSE
    }

  }
  if(remove_comments) {
    idx <- deparse_svec(which(startsWith(sub, "#")), concatenate = FALSE)[[1]]
  }

  idx <- parse_svec(idx)
  sub <- sub[idx[-1]]
  if(!length(sub)) { return() }

  # try to run without #
  if( remove_comments ) {
    sub <- gsub("^#", "", sub)
  }


  cat("# Running the following debug code: ", sub, "", sep = "\n")

  eval(parse(text = sub), envir = env)

}
