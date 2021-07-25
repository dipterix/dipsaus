#' @title Simple shiny alert that uses 'JavaScript' promises
#' @param title title of the alert
#' @param text alert body text (pure text)
#' @param icon which icon to display, choices are \code{'info'},
#' \code{'success'} \code{'warning'}, and \code{'error'}
#' @param danger_mode true or false; if true, then the confirm button turns
#' red and the default focus is set on the cancel button instead. To enable
#' danger mode, \code{buttons} must be \code{TRUE} as well
#' @param buttons logical value or a named list, or characters. If logical,
#' it indicates whether buttons should be displayed or not; for named list,
#' the names will be the button text, see example; for characters, the
#' characters will be the button text and value
#' @param auto_close whether to close automatically when clicking outside of
#' the alert
#' @param on_close \code{NULL} or a function that takes in one argument. If
#' function is passed in, then it will be executed when users close the alert
#' @param session shiny session, see \code{\link[shiny]{domains}}
#' @return a temporary input ID, currently not useful
#' @examples
#'
#' library(shiny)
#' library(dipsaus)
#' ui <- fluidPage(
#'   use_shiny_dipsaus(),
#'   actionButtonStyled('btn', 'btn')
#' )
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$btn, {
#'     shiny_alert2(
#'       on_close = function(value) {
#'         cat("Modal closed!\n")
#'         print(value)
#'       },
#'       title = "Title",
#'       text = "message",
#'       icon = "success",
#'       auto_close = FALSE,
#'       buttons = list("cancel" = TRUE,
#'                      "YES!" = list(value = 1))
#'     )
#'   })
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server, options = list(launch.browser = TRUE))
#' }
#'
#' @export
shiny_alert2 <- function(
  title = "Alert",
  text = '',
  icon = c("info", "warning", "success", "error"),
  danger_mode = FALSE,
  auto_close = TRUE,
  buttons = NULL,
  on_close = NULL,
  session = shiny::getDefaultReactiveDomain()
){
  if( is.null(session) ){ return(invisible()) }
  icon <- match.arg(icon)

  # randomly generate id
  inputId <- rand_string(7)
  inputId <- sprintf("swal_%s", inputId)
  session$sendCustomMessage('dipsaus-swal-show', list(
    inputId = inputId,
    title = title,
    text = text,
    icon = icon,
    dangerMode = danger_mode,
    autoClose = auto_close,
    buttons = buttons
  ))
  if(is.function(on_close)){
    env <- ensure_shiny_proxy(session = session)
    env$alert_callbacks[[inputId]] <- on_close
  }
  invisible(inputId)
}


#' @rdname shiny_alert2
#' @export
close_alert2 <- function(){
  session = shiny::getDefaultReactiveDomain()
  session$sendCustomMessage('dipsaus-swal-close', list(""))
  invisible()
}