
#' @title Set Shiny Input
#' @description Shiny `input' object is read-only reactive list. When try to
#' assign values to input, errors usually occur. This method provides several
#' work-around to set values to input. Please use along with
#' \code{\link{use_shiny_dipsaus}}.
#' @param session shiny session, see shiny \code{\link[shiny]{domains}}
#' @param inputId character, input ID
#' @param value the value to assign
#' @param priority characters, options are "event", "deferred", and "immediate".
#' "event" and "immediate" are similar, they always fire changes. "deferred"
#' fire signals to other reactive/observers only when the input value has been
#' changed
#' @param method characters, options are "proxy", "serialize", "value",
#' "expression". "proxy" is recommended, other methods are experimental.
#' @param quoted is value quoted? Only used when method is "expression"
#' @examples
#'
#' library(shiny)
#' library(dipsaus)
#' ui <- fluidPage(
#'   # Register widgets
#'   use_shiny_dipsaus(),
#'   actionButton('run', 'Set Input'),
#'   verbatimTextOutput('input_value')
#' )
#'
#' server <- function(input, output, session) {
#'   start = Sys.time()
#'
#'   output$input_value <- renderPrint({
#'
#'     now <- input$key
#'     now %?<-% start
#'     cat('This app has been opened for ',
#'         difftime(now, start, units = 'sec'), ' seconds')
#'   })
#'
#'   observeEvent(input$run, {
#'     # setting input$key to Sys.time()
#'     set_shiny_input(session, 'key', Sys.time())
#'   })
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
#'
#'
#' @export
set_shiny_input <- function(
  session = shiny::getDefaultReactiveDomain(), inputId, value,
  priority = c('event', 'deferred', 'immediate'),
  method = c('proxy', 'serialize', 'value', 'expression'), quoted = TRUE){

  priority <- match.arg(priority)
  method <- match.arg(method)
  inputId <- session$ns(inputId)
  proxy <- method

  switch (method,
    'serialize' = {
      raw <- serialize(value, ascii = FALSE, connection = NULL)
      raw <- base64enc::base64encode(raw)
    },
    'value' = {
      raw <- jsonlite::toJSON(value, auto_unbox = TRUE, digits = 22)
      raw <- base64url::base64_urlencode(raw)
    },
    'expression' = {
      if(!quoted){
        value <- substitute(value)
      }
      raw <- paste(deparse(value), collapse = '\n')
      raw <- jsonlite::toJSON(raw, auto_unbox = TRUE)
      raw <- base64url::base64_urlencode(raw)
    },
    'proxy' = {
      session$userData$dipsaus_reserved %?<-% new.env(parent = emptyenv())
      session$userData$dipsaus_reserved$proxy_data %?<-% fastmap2()
      session$userData$dipsaus_reserved$proxy_data[[inputId]] <- value
      raw <- inputId
    }
  )

  session$sendCustomMessage('dipsaus-set-input', list(
    inputId = inputId,
    proxy = proxy,
    value = raw,
    priority = priority
  ))
  invisible()
}



#' @title Set up shiny plugins
#' @description This function must be called from a Shiny app's UI in order for
#' some widgets to work.
#' @param x 'HTML' tags
#' @export
use_shiny_dipsaus <- function(x){
  version <- as.character(utils::packageVersion("dipsaus"))
  deps <- structure(list(name = "dipsaus", version = version, src = list(
    file = system.file('shiny-addons/dipsaus', package = 'dipsaus')),
    meta = NULL, script = "dipsaus-dipterix-lib.js", stylesheet = "dipsaus.css",
    head = NULL, attachment = NULL, package = NULL, all_files = TRUE), class = "html_dependency")
  if(missing(x)){
    x <- shiny::tags$div(style = "display: none;")
  }
  attr(x, "html_dependencies") <- deps
  x
  # shiny::singleton(shiny::tags$head(
  #   shiny::tags$link(rel="stylesheet", type="text/css", href="dipsaus/dipsaus.css"),
  #   shiny::tags$script(src="dipsaus/dipsaus-dipterix-lib.js")
  # ))
}


registerSetInputs <- function(){
  # register input
  shiny::registerInputHandler("dipsaus_asis", function(data, session, name) {

    if(!is.list(data)){
      return(NULL)
    }
    proxy <- match.arg(data$proxy, c('serialize', 'value', 'expression', 'proxy'))

    raw <- data$value
    switch (
      proxy,

      'serialize' = {
        unserialize(base64enc::base64decode(raw))
      },

      'value' = {
        jsonlite::fromJSON(base64url::base64_urldecode(raw))
      },

      'expression' = {
        raw <- jsonlite::fromJSON(base64url::base64_urldecode(raw))
        str2lang(raw)
      },

      'proxy' = {
        if(inherits(session$userData$dipsaus_reserved$proxy_data, 'fastmap2')){
          re <- session$userData$dipsaus_reserved$proxy_data[[raw]]
          session$userData$dipsaus_reserved$proxy_data$`@remove`(raw)
          re
        }else{
          NULL
        }

      },

      {
        cat2('set_shiny_input method must be from ',
             sQuote('serialize'), ', ', sQuote('value'),
             level = 'FATAL')
      }
    )

  }, force = TRUE)
}

