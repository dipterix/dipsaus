


#' @title Take a screenshot in shiny apps
#' @description Take a screenshot of the whole page and save encoded
#' \code{DataURI} that can be accessed via \code{input[[inputId]]}.
#' @param inputId the input id where the screenshot should be
#' @param session shiny session
#' @return None. However, the screenshot results can be accessed from
#' shiny input
#' @examples
#' library(shiny)
#' library(dipsaus)
#' ui <- fluidPage(
#'   tagList(
#'     shiny::singleton(shiny::tags$head(
#'       shiny::tags$link(rel="stylesheet", type="text/css", href="dipsaus/dipsaus.css"),
#'       shiny::tags$script(src="dipsaus/dipsaus-dipterix-lib.js")
#'     ))
#'   ),
#'   actionButtonStyled('do', 'Take Screenshot'),
#'   compoundInput2('group', label = 'Group', components = list(
#'     textInput('txt', 'Enter something here')
#'   ))
#' )
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$do, {
#'     screenshot('screeshot_result')
#'   })
#'   observeEvent(input$screeshot_result, {
#'     showModal(modalDialog(
#'       tags$img(src = input$screeshot_result, width = '100%')
#'     ))
#'   })
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
#'
#' @export
screenshot <- function(inputId, session = shiny::getDefaultReactiveDomain()){
  if(inherits(session, c('session_proxy', 'ShinySession'))){
    session$sendCustomMessage('dipsaus-screeshot', list(
      inputId = session$ns(inputId)
    ))
  }
  invisible()
}

#' @title Save "Base64" Data to Images
#' @param data characters, encoded "Base64" data for images
#' @param path file path to save to
#' @return Absolute path of the saved file
#' @export
base64_to_image <- function(data, path){

  con <- file(path, 'wb')
  on.exit({
    close(con)
  }, add = TRUE)

  data <- stringr::str_replace(data, '^data:image.{0,10};base64,', '')
  base64enc::base64decode(data, output = con)

  invisible(normalizePath(path))
}

#
# shiny_bugreport_local <- function(title, body, footer = NULL, image = ''){
#   date_time <- blastula::add_readable_time()
#
#   if(length(image) > 1 || image != ''){
#     body <- c(body, image)
#   }
#
#   email <- blastula::compose_email(
#     header = blastula::md(title),
#     body = blastula::md(body),
#     footer = blastula::md(c(
#       footer,
#       '\n', date_time
#     ))
#   )
#
#   # create Zip file of log
#   logfiles <- packup_logger()
#   if(!is.null(logfiles)){
#     email <- blastula::add_attachment(email, logfiles, content_type = "application/zip",
#                              filename = 'logs.zip')
#   }
#   # Add session info
#   sess_info <- utils::capture.output({
#     print('----------- SESS -----------')
#     print(utils::sessionInfo())
#     print('----------- ENV -----------')
#     print(Sys.getenv())
#     print('----------- OPT -----------')
#     print(options())
#   })
#   f <- tempfile()
#   writeLines(sess_info, f)
#   email <- blastula::add_attachment(email, f, content_type = "text/plain",
#                            filename = 'session.txt')
#
#   email
# }
#
#
# shiny_bugreport <- function(title, body, footer = NULL, image,
#                             then = NULL){
#
#   if(missing(image)){
#     image <- ''
#   }else{
#     image <- blastula::add_image(image)
#   }
#
#   session <- shiny::getDefaultReactiveDomain()
#   if(!inherits(session, c('session_proxy', 'ShinySession'))){
#     email <- shiny_bugreport_local(title, body, footer, image)
#     if(is.function(then)){
#       email <- then(email)
#     }
#     return(email)
#   }
#   shiny::observeEvent(session$input[['..dipsaus_screenshot..']], {
#     uri <- session$input[['..dipsaus_screenshot..']]
#     if(!is.null(uri)){
#       ff <- tempfile()
#       ff <- base64_to_image(uri, ff)
#       img <- blastula::add_image(ff)
#     }else{
#       img <- NULL
#     }
#
#     email <- shiny_bugreport_local(title, body, footer, image = c(image, img))
#     if(is.function(then)){
#       email <- then(email)
#     }
#     return(email)
#
#   }, domain = session, once = TRUE, ignoreInit = TRUE)
#
#
#   screenshot('..dipsaus_screenshot..', session)
#
# }



