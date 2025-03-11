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