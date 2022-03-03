#' @title Synchronize Shiny Inputs
#' @param input,session shiny reactive objects
#' @param inputIds input ids to be synchronized
#' @param uniform functions, equaling to length of \code{inputIds}, converting inputs
#' to a uniform values
#' @param updates functions, equaling to length of \code{inputIds}, updating input values
#' @param snap numeric, milliseconds to defer the changes
#' @param ignoreNULL,ignoreInit passed to \code{\link[shiny]{bindEvent}}
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
                              uniform = rep('I', length(inputIds)),
                              updates, snap = 250,
                              ignoreNULL = TRUE, ignoreInit = FALSE){
  env <- fastmap::fastmap()

  env$mset(
    which_changed = 0,
    suppress_other = FALSE,
    val = NULL
  )

  local_data <- reactiveValues(
    last_changed = Sys.time(),
    last_updated = Sys.time()
  )
  lapply(seq_along(inputIds), function(ii){
    input_id <- inputIds[[ii]]
    shiny::bindEvent(
      shiny::observe({
        if(!env$get("suppress_other", missing = FALSE)){
          env$set("which_changed", ii)
        }
        if( env$get("which_changed", missing = 0) == ii ){
          env$set(key = "val", do.call(uniform[[ii]], list(input[[input_id]])))
          local_data$last_changed <- Sys.time()
        }
      }),
      input[[input_id]],
      ignoreNULL = ignoreNULL,
      ignoreInit = ignoreInit
    )
  })

  shiny::bindEvent(
    shiny::observe({
      if( env$get("which_changed", missing = 0) == 0 ){ return() }
      # suppress other inputs
      env$set("suppress_other", value = TRUE)

      lapply(seq_along(inputIds), function(ii){
        if(ii != env$get("which_changed", missing = 0)){
          updates[[ii]](env$get("val"))
          # updateTextInput(session, env[[paste0('inname', 3 - which_changed)]], value = val)
        }
      })

      local_data$last_updated <- Sys.time()
    }),
    local_data$last_changed,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  finished_updates <- shiny::debounce(
    shiny::bindEvent(
      shiny::reactive({
        if(length(local_data$last_changed) && length(local_data$last_updated)) {
          return(Sys.time())
        } else {
          return()
        }
      }),
      local_data$last_changed,
      local_data$last_updated,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    ),
    millis = snap
  )

  shiny::bindEvent(
    shiny::observe({
      local_data$last_updated <- NULL
      local_data$last_changed <- NULL
      env$set("suppress_other", FALSE)
    }),
    finished_updates(),
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  invisible()
}






