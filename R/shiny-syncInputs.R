#' @title Synchronize Shiny Inputs
#' @param input,session shiny reactive objects
#' @param inputIds input ids to be synchronized
#' @param uniform functions, equaling to length of \code{inputIds}, converting inputs
#' to a uniform values
#' @param updates functions, equaling to length of \code{inputIds}, updating input values
#' @param snap numeric, milliseconds to defer the changes
#'
#' @return none.
#'
#' @examples
#'
#' library(shiny)
#'
#' ui <- fluidPage(
#'   textInput('a', 'a', value = 'a'),
#'   sliderInput('b', 'b', value = 1, min = 0, max = 1000)
#' )
#'
#' server <- function(input, output, session) {
#'   sync_shiny_inputs(input, session, inputIds = c('a', 'b'), uniform = list(
#'     function(a){as.numeric(a)},
#'     'I'
#'   ), updates = list(
#'     function(a){updateTextInput(session, 'a', value = a)},
#'     function(b){updateSliderInput(session, 'b', value = b)}
#'   ))
#'
#' }
#'
#' if( interactive() ){
#'   shinyApp(ui, server)
#' }
#'
#' @export
sync_shiny_inputs <- function(input, session, inputIds,
                              uniform = rep('I', length(inputIds)), updates, snap = 250){
  env <- new.env(parent = emptyenv())
  this_env <- environment()

  env$which_changed <- 0
  env$suppress_other <- FALSE
  env$val <- NULL

  local_data <- reactiveValues(
    last_changed = Sys.time(),
    last_updated = Sys.time()
  )
  lapply(seq_along(inputIds), function(ii){
    input_id <- inputIds[[ii]]
    observeEvent(input[[input_id]], {
      if(!env$suppress_other){
        env$which_changed <- ii
      }
      if( env$which_changed == ii ){
        env$val <- do.call(uniform[[ii]], list(input[[input_id]]))
        local_data$last_changed <- Sys.time()
      }
    }, event.env = environment(), handler.env = environment())
  })
  observeEvent(local_data$last_changed, {
    if( env$which_changed == 0 ){ return() }
    # suppress other inputs
    env$suppress_other <- TRUE

    lapply(seq_along(inputIds), function(ii){
      if(ii != env$which_changed){
        updates[[ii]](env$val)
        # updateTextInput(session, env[[paste0('inname', 3 - which_changed)]], value = val)
      }
    })

    local_data$last_updated <- Sys.time()
  }, event.env = this_env, handler.env = this_env)

  observe({
    last_updated <- local_data$last_updated
    if( is.null(last_updated) ){
      env$suppress_other <- FALSE
      return()
    }
    print(last_updated)
    now <- Sys.time()
    dif <- time_delta(last_updated, now) * 1000
    if(dif > snap){
      env$suppress_other <- FALSE
      return()
    }else{
      if( dif < 10 ){
        dif <- snap
      }
      shiny::invalidateLater(dif)
    }
  }, env = this_env)

  invisible()
}






