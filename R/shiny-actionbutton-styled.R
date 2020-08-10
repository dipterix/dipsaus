#' Action Button but with customized styles
#' @param inputId,label,icon,width,... passed to \code{shiny::actionButton}
#' @param type button type, choices are `default`, `primary`, `info`, `success`,
#' `warning`, and `danger`
#' @param btn_type HTML tag type, either \code{"button"} or \code{"a"}
#' @param class additional classes to be added to the button
#'
#' @return `HTML` tags
#'
#' @examples
#'
#' # demo('example-actionButtonStyled', package='dipsaus')
#'
#' library(shiny)
#' library(dipsaus)
#'
#' ui <- fluidPage(
#'   actionButtonStyled('btn', label = 'Click me', type = 'default'),
#'   actionButtonStyled('btn2', label = 'Click me2', type = 'primary')
#' )
#'
#'
#' server <- function(input, output, session) {
#'   btn_types = c('default', 'primary', 'info', 'success', 'warning', 'danger')
#'   observeEvent(input$btn, {
#'     btype = btn_types[((input$btn-1) %% (length(btn_types)-1)) + 1]
#'     updateActionButtonStyled(session, 'btn2', type = btype)
#'   })
#'   observeEvent(input$btn2, {
#'     updateActionButtonStyled(session, 'btn',
#'                              disabled = c(FALSE,TRUE)[(input$btn2 %% 2) + 1])
#'   })
#' }
#'
#'
#' if( interactive() ){
#'   shinyApp(ui, server, options = list(launch.browser=TRUE))
#' }
#'
#' @seealso \code{\link[dipsaus]{updateActionButtonStyled}} for how to update the button.
#'
#' @export
actionButtonStyled <- function(
  inputId, label, icon = NULL, width = NULL, type = "primary",
  btn_type = "button", class = "", ...
){

  if(length(type) > 1){ type <- type[[1]] }
  stopifnot2(length(type) == 0 || type[[1]] %in% c(
    'default', 'primary', 'info', 'success', 'warning', 'danger'
  ), msg = "type must be in 'default', 'primary', 'info', 'success', 'warning', 'danger'")

  value <- shiny::restoreInput(id = inputId, default = NULL)
  args <- list(...)
  style <- c(args[["style"]], "")[[1]]
  width <- c(width, "auto")[[1]]
  style <- paste0("width: ", shiny::validateCssUnit(width), ";", style)
  args[["style"]] <- style
  args[["id"]] <- inputId
  args[["type"]] <- btn_type
  args[["class"]] <- sprintf("btn btn-%s action-button %s", type, class)
  args[["data-val"]] <- value
  args[["id"]] <- inputId

  use_shiny_dipsaus(do.call(shiny::tags$button, c(list(list(icon, label)), args)))
}

#' Update styled action button
#' @param session,inputId,label,icon passed to \code{shiny::updateActionButton}
#' @param type button type to update
#' @param disabled whether to disable the button
#' @param ... ignored
#' @return none
#'
#' @seealso \code{\link[dipsaus]{actionButtonStyled}} for how to define the button.
#'
#' @export
updateActionButtonStyled <- function(
  session, inputId, label = NULL, icon = NULL, type = NULL, disabled = NULL, ...){
  if(length(type) > 1){ type <- type[[1]] }
  stopifnot2(length(type) == 0 || type[[1]] %in% c(
    'default', 'primary', 'info', 'success', 'warning', 'danger'),
    msg = "type must be in 'default', 'primary', 'info', 'success', 'warning', 'danger'")
  session$sendCustomMessage(
    type = 'dipsaus.updateActionButtonStyled',
    message = list(
      inputId = session$ns( inputId ),
      type = type,
      disabled = isTRUE(disabled)
    )
  )

  shiny::updateActionButton(session, inputId, label, icon)
  invisible()
}
