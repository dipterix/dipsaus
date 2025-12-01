#' @title Shiny drag-and-drop file input
#' @description
#' Fancy drag and drop file upload for \code{shiny} apps.
#' @param inputId the input slot that will be used to access the value
#' @param label display label for the control, or NULL for no label.
#' @param width the width of the input
#' @param after_content tiny content that is to be displayed below the input box
#' @param size height of the widget, choices are \code{'s'}, \code{'m'}, \code{'l'}, and \code{'xl'}
#' @param ... passed to \code{\link[shiny]{fileInput}}
#' @returns See \code{\link[shiny]{fileInput}}
#' @examples
#'
#'
#' library(shiny)
#' library(dipsaus)
#'
#' ui <- basicPage(
#'   fancyFileInput('file_input', "Please upload")
#' )
#'
#' if(interactive()) {
#'   shinyApp(
#'     ui, server = function(input, output, session){},
#'     options = list(launch.browser = TRUE)
#'   )
#' }
#'
#' @export
fancyFileInput <- function( inputId, label, width = NULL,
                            after_content = "Drag & drop, or button",
                            size = c("s", "m", "l", "xl"),
                            ... ) {

  if(missing(label)) {
    label <- NULL
  }
  size <- match.arg(size)

  htmltools <- asNamespace("htmltools")
  max_size <- dipsaus::to_ram_size(getOption("shiny.maxRequestSize", 5*1024^2), 1024)

  shiny::div(
    class = c("dipsaus-fancy-file-input", sprintf("dipsaus-fancy-file-input-%s", size)),
    `dipsaus-after-content` = sprintf('%s (max: %.1f %s)',
                                      after_content,
                                      max_size, attr(max_size, "unit")),
    style = htmltools$css(
      width = htmltools$validateCssUnit(width),
    ),
    use_shiny_dipsaus(),
    shiny::fileInput(inputId = inputId, label = label, width = "100%", ...)
  )

}


#' @title Shiny drag-and-drop directory input
#' @description
#' Fancy drag and drop directory upload for \code{shiny} apps. This function
#' allows users to drag and drop entire directories. Note: This feature requires
#' browser support for the \code{webkitdirectory} attribute (Chrome, Edge, Safari).
#' Firefox has limited support.
#' @param inputId the input slot that will be used to access the value
#' @param label display label for the control, or NULL for no label.
#' @param width the width of the input
#' @param after_content tiny content that is to be displayed below the input box
#' @param size height of the widget, choices are \code{'s'}, \code{'m'}, \code{'l'}, and \code{'xl'}
#' @param maxSize maximum file size per file in bytes (default uses \code{shiny.maxRequestSize} option, typically 5MB)
#' @param ... additional arguments (currently unused)
#' @returns A data frame with components: \code{name} (file name),
#' \code{size} (file size in bytes), \code{type} (MIME type), \code{datapath}
#' (temporary file path on server), and \code{relativePath} (full relative path
#' including subdirectories). The data frame also has an attribute \code{directoryStructure}
#' containing a nested list representing the directory tree.
#' @details
#' The directory input uses the \code{webkitdirectory} HTML attribute which is
#' not part of the HTML5 standard but is widely supported. Browser compatibility:
#' \itemize{
#'   \item Chrome/Edge: Full support
#'   \item Safari: Full support
#'   \item Firefox: Partial support (desktop only, no mobile)
#'   \item Internet Explorer: Not supported
#' }
#'
#' Hidden files (starting with '.') are filtered out by default on the client side.
#'
#' Files are transferred as base64-encoded data, so they use approximately 33\% more
#' bandwidth than their actual size. Files exceeding \code{maxSize} will be skipped
#' with a warning in the browser console.
#'
#' @examples
#'
#'
#' library(shiny)
#' library(dipsaus)
#'
#' ui <- basicPage(
#'   fancyDirectoryInput('dir_input', "Please upload a directory")
#' )
#'
#' if(interactive()) {
#'   shinyApp(
#'     ui,
#'     server = function(input, output, session){
#'       observeEvent(input$dir_input, {
#'         files <- input$dir_input
#'         if(!is.null(files)) {
#'           cat("Uploaded files:\n")
#'           print(files[, c("name", "size", "relativePath")])
#'
#'           # Access directory structure
#'           dir_struct <- attr(files, "directoryStructure")
#'           cat("\nDirectory structure:\n")
#'           print(str(dir_struct))
#'         }
#'       })
#'     },
#'     options = list(launch.browser = TRUE)
#'   )
#' }
#'
#' @export
fancyDirectoryInput <- function( inputId, label, width = NULL,
                                 after_content = "Drag & drop directory, or button",
                                 size = c("s", "m", "l", "xl"),
                                 maxSize = NULL,
                                 ... ) {

  if(missing(label)) {
    label <- NULL
  }
  size <- match.arg(size)

  htmltools <- asNamespace("htmltools")

  # Use provided maxSize or fall back to shiny option
  if(is.null(maxSize)) {
    maxSize <- getOption("shiny.maxRequestSize", 5*1024^2)
  }
  max_size <- dipsaus::to_ram_size(maxSize, 1024)

  # Create the HTML structure manually to avoid Shiny's file input binding
  input_id_directory <- paste0(inputId, "_directory")

  shiny::div(
    class = c("dipsaus-fancy-directory-input", sprintf("dipsaus-fancy-directory-input-%s", size)),
    id = inputId,
    `data-max-file-size` = maxSize,  # Pass max size to JavaScript in bytes
    `dipsaus-after-content` = sprintf('%s (max per file: %.1f %s)',
                                      after_content,
                                      max_size, attr(max_size, "unit")),
    style = htmltools$css(
      width = htmltools$validateCssUnit(width),
    ),
    use_shiny_dipsaus(),

    # Manual HTML structure for file input
    shiny::div(
      class = "shiny-input-container",
      style = "width: 100%;",

      if(!is.null(label)) {
        shiny::tags$label(
          class = "control-label",
          `for` = input_id_directory,
          label
        )
      },

      shiny::div(
        class = "input-group",

        shiny::tags$label(
          class = "input-group-btn",
          shiny::tags$span(
            class = "btn btn-default btn-file",
            "Browse...",
            shiny::tags$input(
              id = input_id_directory,
              name = input_id_directory,
              type = "file",
              class = "dipsaus-directory-file-input",
              style = "display: none;",
              webkitdirectory = NA,
              directory = NA,
              multiple = NA
            )
          )
        ),

        shiny::tags$input(
          type = "text",
          class = "form-control",
          placeholder = "No directory selected",
          readonly = "readonly"
        )
      ),

      shiny::div(
        class = "progress progress-striped active shiny-file-input-progress",
        style = "display: none;",
        shiny::div(class = "progress-bar")
      )
    )
  )

}