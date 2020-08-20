#' 'Shiny' progress bar, but can run without reactive context
#' @param title character, task description
#' @param max maximum number of items in the queue
#' @param ... passed to \code{shiny::Progress$new(...)}
#' @param quiet suppress console output, ignored in shiny context.
#' @param session 'shiny' session, default is current reactive domain
#' @param log function when running locally, default is \code{NULL},
#' which redirects to \code{\link{cat2}}
#' @param shiny_auto_close logical, automatically close 'shiny' progress bar
#' once current observer is over. Default is \code{FALSE}. If setting to
#' \code{TRUE}, then it's equivalent to
#' \code{p <- progress2(...); on.exit({p$close()}, add = TRUE)}.
#'
#' @return A list of functions:
#' \describe{
#' \item{\code{inc(detail, message = NULL, amount = 1, ...)}}{
#' Increase progress bar by \code{amount} (default is 1).
#' }
#' \item{\code{close()}}{
#' Close the progress
#' }
#' \item{\code{reset(detail = '', message = '', value = 0)}}{
#' Reset the progress to \code{value} (default is 0), and reset information
#' }
#' \item{\code{get_value()}}{
#' Get current progress value
#' }
#' \item{\code{is_closed()}}{
#' Returns logical value if the progress is closed or not.
#' }
#' }
#'
#' @examples
#'
#' progress <- progress2('Task A', max = 2)
#' progress$inc('Detail 1')
#' progress$inc('Detail 2')
#' progress$close()
#'
#' # Check if progress is closed
#' progress$is_closed()
#'
#' # ------------------------------ Shiny Example ------------------------------
#' library(shiny)
#' library(dipsaus)
#'
#' ui <- fluidPage(
#'   actionButtonStyled('do', 'Click Here', type = 'primary')
#' )
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$do, {
#'     updateActionButtonStyled(session, 'do', disabled = TRUE)
#'     progress <- progress2('Task A', max = 10, shiny_auto_close = TRUE)
#'     lapply(1:10, function(ii){
#'       progress$inc(sprintf('Detail %d', ii))
#'       Sys.sleep(0.2)
#'     })
#'     updateActionButtonStyled(session, 'do', disabled = FALSE)
#'   })
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
#'
#' @export
progress2 <- function( title, max = 1, ..., quiet = FALSE,
                       session = shiny::getDefaultReactiveDomain(),
                       shiny_auto_close = FALSE, log = NULL){
  if(missing(title) || is.null(title)){ title <- '' }
  if( length(title) > 1 ){ title <- paste(title, collapse = '')}

  if( inherits(session, c('ShinySession', 'session_proxy', 'R6')) ){
    within_shiny <- TRUE
  }else{
    within_shiny <- FALSE
  }

  current <- 0
  closed <- FALSE
  get_value <- function(){ current }
  is_closed <- function(){ closed }
  logger <- function(..., .quiet = quiet, level = 'DEFAULT', bullet = 'play'){
    if(!.quiet){
      if(is.function(log)){
        log(...)
      }else{
        cat2(..., level=level, bullet=bullet)
      }
    }
  }



  if( quiet || !within_shiny ){
    progress <- NULL
    logger(sprintf("[%s]: initializing...", title), level = 'DEFAULT', bullet = 'play')

    inc <- function(detail, message = NULL, amount = 1, ...){
      stopifnot2(!closed, msg = 'progress is closed')
      quiet <- c(list(...)[['quiet']], quiet)[[1]]
      # if message is updated
      if(!is.null(message) && length(message) == 1){ title <<- message }
      current <<- amount + current
      logger(sprintf("[%s]: %s (%d out of %d)", title, detail, current, max),
           level = 'DEFAULT', bullet = 'arrow_right', .quiet = quiet)
    }

    close <- function(message = 'Finished'){
      closed <<- TRUE
      logger(message, level = 'DEFAULT', bullet = 'stop')
    }
    reset <- function(detail = '', message = '', value = 0){
      title <<- message
      current <<- value
    }

  }else{
    progress <- shiny::Progress$new(session = session, max = max, ...)
    inc <- function(detail, message = NULL, amount = 1, ...){
      if(!is.null(message) && length(message) == 1){ title <<- message }
      progress$inc(detail = detail, message = title, amount = amount)
    }
    close <- function(message = 'Finished'){
      if(!closed){
        progress$close()
        closed <<- TRUE
      }
    }
    reset <- function(detail = '', message = '', value = 0){
      title <<- message
      current <<- value
      progress$set(value = value, message = title, detail = detail)
    }
    if(shiny_auto_close){
      parent_frame <- parent.frame()
      do.call(
        on.exit, list(substitute(close()), add = TRUE),
        envir = parent_frame
      )
    }
    inc(detail = 'Initializing...', amount = 0)

  }

  return(list(
    .progress = progress,
    inc = inc,
    close = close,
    reset = reset,
    get_value = get_value,
    is_closed = is_closed
  ))
}

#' @title Progress-bar Handler
#' @description Handler for \code{\link{progress2}} to support
#' \code{progressr::handlers}. See examples for detailed use case
#' @param title default title of \code{\link{progress2}}
#' @param intrusiveness A non-negative scalar on how intrusive
#' (disruptive) the reporter to the user
#' @param target where progression updates are rendered
#' @param ... passed to \code{\link[progressr]{make_progression_handler}}
#' @examples
#'
#'
#' library(progressr)
#' library(shiny)
#' library(future)
#'
#' ## ------------------------------ Setup! -------------------------------
#' handlers(handler_dipsaus_progress())
#'
#' # ------------------------------ A simple usage ------------------------
#' xs <- 1:5
#' handlers(handler_dipsaus_progress())
#' with_progress({
#'   p <- progressor(along = xs)
#'   y <- lapply(xs, function(x) {
#'     p(sprintf("x=%g", x))
#'     Sys.sleep(0.1)
#'     sqrt(x)
#'   })
#' })
#'
#' # ------------------------ A future.apply case -------------------------
#' plan(sequential)
#' # test it yourself with plan(multisession)
#'
#' handlers(handler_dipsaus_progress())
#' with_progress({
#'   p <- progressor(along = xs)
#'   y <- future.apply::future_lapply(xs, function(x) {
#'     p(sprintf("x=%g", x))
#'     Sys.sleep(0.1)
#'     sqrt(x)
#'   })
#' })
#'
#' # ------------------------ A shiny case --------------------------------
#'
#' ui <- fluidPage(
#'   actionButton('ok', 'Run Demo')
#' )
#'
#' server <- function(input, output, session) {
#'   handlers(handler_dipsaus_progress())
#'   make_forked_clusters()
#'
#'   observeEvent(input$ok, {
#'     with_progress({
#'       p <- progressor(along = 1:100)
#'       y <- future.apply::future_lapply(1:100, function(x) {
#'         p(sprintf("Input %d|Result %d", x, x+1))
#'         Sys.sleep(1)
#'         x+1
#'       })
#'     })
#'   })
#' }
#'
#' if(interactive()){
#'   shinyApp(ui, server)
#' }
#'
#'
#'
#' @export
handler_dipsaus_progress <- function (
  title = getOption("dipsaus.progressr.title", "Progress"),
  intrusiveness = getOption("progressr.intrusiveness.gui", 1),
  target = if (is.null(shiny::getDefaultReactiveDomain())) "terminal" else "gui",
  ...)
{
  reporter <- local({
    pb <- NULL
    make_pb <- function(title, max, ...) {
      if (!is.null(pb) && !pb$is_closed())
        return(pb)
      pb <<- progress2(title, max, ..., log = function(...){
        msg <- paste(..., sep = '', collapse = ' ')
        msg <- gsub(pattern = '[\n]+$', replacement = '', msg)
        cw <- getOption('width', 80)
        message('\r', paste(rep(' ', cw), collapse = ''), appendLF = FALSE)
        message('\r', msg, appendLF = FALSE)
        flush.console()
      })
      pb
    }
    list(
      reset = function(...) {
        if(!is.null(pb)){
          pb$close()
        }
        pb <<- NULL
      },
      initiate = function(config, state, progression, ...) {
        if (!state$enabled || config$times == 1L) return()
        make_pb(title = title, max = config$max_steps)
      },
      update = function(config, state, progression, ...) {
        if (!state$enabled || config$times <= 2L) return()
        # make sure pb exists
        make_pb(title = title, max = config$max_steps)
        if( state$delta > 0 ){
          # message|details
          msg <- strsplit(state$message, '|', TRUE)[[1]]
          if(length(msg) > 1){
            message <- msg[[1]]
            detail <- paste(msg[-1], collapse = '|')
          }else{
            message <- NULL
            detail <- state$message
          }
          pb$inc(message = message, detail = detail,
                 amount = state$delta)
        }
      },
      finish = function(config, state, progression, ...) {
        if( is.null(pb) || pb$is_closed() ) return()
        if( pb$get_value() < config$max_steps ){
          state$step <- config$max_steps
          reporter$update(config = config, state = state,
                          progression = progression, ...)
        }
        if (config$clear ) {
          # s <- sprintf('%s - Finished', title)
          # ss <- paste(rep(' ', stringr::str_length(s)), collapse = '')
          # cat(sprintf('%s\r%s\n', s, ss))
          pb$close(sprintf('%s\r', cli::symbol$square_small_filled))

        }
      }
    )
  })
  progressr::make_progression_handler(
    name = "dipsaus_progress", reporter,
    intrusiveness = intrusiveness, ...)
}
