#' Enables quick debug.
#' Tired of running debugging code? Save this shortcut
#' To set debug code, write down debugging code in the following format
#' To run the code, use shortcut. make sure the cursor pointer is below the
#' flag `DIPSAUS DEBUG START`. Try to click anywhere below line 8 and activate
#' shortcut 1 (alt+1)

# DIPSAUS DEBUG START
# message("This is a debug message")
dipsaus::rs_add_shortcut(1, {
  dipsaus::rs_quick_debug()
})

#' Save current workspace to a temporary place for quick debugging when
#' switching between R sessions
dipsaus::rs_add_shortcut(4, {
  local({
    image_path <- file.path(tools::R_user_dir(package = "dipsaus", which = "cache"), "temporary")
    if(!dir.exists(image_path)) {
      dir.create(image_path, showWarnings = FALSE, recursive = TRUE)
    }
    image_path <- file.path(image_path, "snapshot.RData")
    message("Saving current workspace to: ", image_path)
    save.image(file = image_path)
  })
})

#' Load workspace saved by the previous shortcut
dipsaus::rs_add_shortcut(5, {
  local({
    image_path <- file.path(tools::R_user_dir(package = "dipsaus", which = "cache"), "temporary", "snapshot.RData")
    if(file.exists(image_path)) {
      message("Loading saved workspace from: ", image_path)
      load(image_path)
    }
  })
})

