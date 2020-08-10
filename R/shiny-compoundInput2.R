
#' Compound input that combines and extends shiny inputs
#' @name compoundInput2
#' @param inputId character, shiny input ID
#' @param label character, will show on each groups
#' @param components `HTML` tags that defines and combines HTML components within groups
#' @param initial_ncomp numeric initial number of groups to show, non-negative
#' @param min_ncomp minimum number of groups, default is 0, non-negative
#' @param max_ncomp maximum number of groups, default is 10, greater or equal
#' than \code{min_ncomp}
#' @param label_color integer or characters, length of 1 or \code{max_ncomp},
#' assigning colors to each group labels,
#' @param value list of lists, initial values of each inputs, see examples.
#' @param max_height maximum height of the widget
#' @param ... will be ignored
#' @examples
#' library(shiny); library(dipsaus)
#' compoundInput2(
#'   'input_id', 'Group',
#'     div(
#'     textInput('text', 'Text Label'),
#'     sliderInput('sli', 'Slider Selector', value = 0, min = 1, max = 1)
#'   ),
#'   label_color = 1:10,
#'   value = list(
#'     list(text = '1'),  # Set text first group to be "1"
#'     list(),                # no settings for second group
#'     list(sli = 0.2)    # sli = 0.2 for the third group
#'   ))
#'
#' # Source - system.file('demo/example-compountInput2.R', package='dipsaus')
#'
#' # demo('example-compountInput2', package='dipsaus')
#'
#' library(shiny)
#' library(dipsaus)
#' ui <- fluidPage(
#'   fluidRow(
#'     column(
#'       width = 4,
#'       compoundInput2(
#'         'compound', 'Group Label', label_color = 1:10,
#'         components = div(
#'           textInput('txt', 'Text'),
#'           selectInput('sel', 'Select', choices = 1:10, multiple = TRUE),
#'           sliderInput('sli', 'Slider', max=1, min=0, val=0.5)
#'         ),
#'         value = list(
#'           list(txt = '1'),  # Set text first group to be "1"
#'           '',                # no settings for second group
#'           list(sli = 0.2)    # sli = 0.2 for the third group
#'         )
#'       ),
#'       hr(),
#'       actionButton('action', 'Update compound input')
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   observe({
#'     print(input$compound)
#'   })
#'   observe({
#'     # Getting specific input at group 1
#'     print(input$compound_txt_1)
#'   })
#'   observeEvent(input$action, {
#'     updateCompoundInput2(
#'       session, 'compound',
#'       # Update values for each components
#'       value = lapply(1:5, function(ii){
#'         list(
#'           txt = sample(LETTERS, 1),
#'           sel = sample(1:10, 3),
#'           sli = runif(1)
#'         )
#'       }), ncomp = NULL, txt = list(label = as.character(Sys.time())))
#'   })
#' }
#'
#' if( interactive() ){
#'   shinyApp(ui, server, options = list(launch.browser = TRUE))
#' }
#'
#' @seealso \code{\link[dipsaus]{updateCompoundInput2}} for how to update inputs
#' @return `HTML` tags
NULL

# compound_inputs <- fastmap::fastmap()

#' @rdname compoundInput2
#' @export
compoundInput2 <- function(
  inputId, label = 'Group', components = shiny::tagList(),
  initial_ncomp = 1, min_ncomp = 0, max_ncomp = 10,
  value = NULL, label_color = 1, max_height = NULL, ...
){
  # add_js_script()

  if( length(label_color) == 0 ){ label_color <- 1 }
  if( !length(label_color) %in% c(1, max_ncomp)){
    cat2('label_color must be length of 1 or equal to max_ncomp', level = 'FATAL')
  }
  label_color <- col2hexStr( label_color )
  if( length(label_color) == 1 ){
    label_color <- rep(label_color, max_ncomp)
  }
  # Add css, js


  components <- substitute(components)

  parent_env <- parent.frame()
  ...this_env <- new.env(); ...this_env$bind_infos <- list()
  min_ncomp <- max(min_ncomp, 0)
  max_ncomp <- max(min_ncomp, max_ncomp)
  initial_ncomp <- max(initial_ncomp, min_ncomp)

  ...make_ui <- function(ind){
    nest_inputids <- function(id, call){
      fname <- call[[1]]
      bind_info <- getInputBinding(fname)

      id <- eval(id)
      ...this_env$bind_infos[[ id ]] <- bind_info


      inner_id <- paste0(inputId, '_', id, '_', ind)

      inner_id
    }
    comp <- match_calls(components, quoted = TRUE, envir = parent_env,
                        replace_args = list(
                          inputId = nest_inputids,
                          outputId = nest_inputids
                        ))

    as.call(list(
      quote(shiny::div),
      class = 'dipsaus-compound-input-item col-xs-12',
      `data-value` = sprintf('%s', ind),
      as.call(list(
        quote(shiny::tags$fieldset),
        as.call(list(
          quote(shiny::tags$legend),
          style = sprintf(
            'border:none; margin: 0; padding: 0 10px; font-size: 14px; color: ${{label_color}}'),
          sprintf('%s - %s', label, ind)
        )),
        style = 'border: 1px solid #efefef; padding:.35em .625em .75em; margin-bottom: 15px;',
        comp
      ))
    ))
  }

  if(!is.list(value)){
    value <- list()
  }else{
    names(value) <- NULL
  }

  value <- shiny::restoreInput(id = inputId, default = value)

  if(length(max_height)){
    ...overflow_x <- 'hidden'
    ...overflow_y <- 'scroll'
  } else{
    max_height <- 'auto'
    ...overflow_x <- 'hidden'
    ...overflow_y <- 'auto'
  }
  comp_ui <- quote(shiny::div(
    id = inputId,
    class = 'dipsaus-compound-input',
    style = sprintf('max-height:%s;overflow-x:%s;overflow-y:%s',
                    ...max_height, ...overflow_x, ...overflow_y),
    # shiny::singleton(shiny::tags$head(
    #   shiny::tags$link(rel="stylesheet", type="text/css", href="dipsaus/dipsaus.css"),
    #   shiny::tags$script(src="dipsaus/dipsaus-dipterix-lib.js")
    # )),
    shiny::div(
      class = 'dipsaus-compound-input-header force-hidden', style = 'display:none!important;',
      # Input information

      jsonlite::toJSON(list(
        template = as.character(eval(...make_ui('${{ind}}'))),
        initial_ncomp = ...initial_ncomp,
        min_ncomp = ...min_ncomp,
        max_ncomp = ...max_ncomp,
        bind_infos = ...this_env$bind_infos,
        label_color = ...label_color,
        initial_value = ...value
      ))
    ),
    shiny::div(
      class = 'dipsaus-compound-input-body row'
    ),
    shiny::div(
      class = 'dipsaus-compound-input-foot',
      shiny::div(
        class = 'dipsaus-compound-input-foot-ctrl'
      )
    ),
    shiny::div(
      class = 'force-hidden',
      # Make sure some plugins are loaded
      eval(...make_ui('...junk...'))
    )
  ))



  re <- eval(comp_ui, envir = list(
    inputId = inputId, ...make_ui = ...make_ui, ...initial_ncomp = initial_ncomp,
    ...min_ncomp = min_ncomp, ...max_ncomp = max_ncomp, ...label = label,
    ...label_color = label_color, ...value = value, ...this_env = ...this_env,
    ...max_height = max_height, ...overflow_x = ...overflow_x, ...overflow_y = ...overflow_y
  ), enclos = parent_env)


  value <- as.list(value)
  value$meta <- list(
    initial_ncomp = initial_ncomp,
    min_ncomp = min_ncomp,
    max_ncomp = max_ncomp,
    template = '',
    bind_infos = ...this_env$bind_infos,
    label_color = label_color
  )


  session <- shiny::getDefaultReactiveDomain()
  if(!is.null(session)){
    value <- translate_compoundInput(value, session$rootScope(), inputId)
    session$userData$dipsaus_reserved %?<-% new.env(parent = emptyenv())
    session$userData$dipsaus_reserved$compount_inputs %?<-% fastmap::fastmap()
    session$userData$dipsaus_reserved$compount_inputs$set(inputId, value)
  }

  use_shiny_dipsaus(re)
}


translate_compoundInput <- function(data, session, name){
  if (is.null(data)){ return(list()) }

  # restoreInput(id = , NULL)
  meta <- as.list(data$meta)
  data$meta <- NULL
  # shinysession$ns(name)
  mis_sess <- missing(session)

  if(!mis_sess && !length(meta) && is.environment(session$userData$dipsaus_reserved)){
    default_val <- session$userData$dipsaus_reserved$compount_inputs$get(session$ns(name))
    meta <- as.list(default_val$meta)
  }

  # session_scope = character(0)
  # if( !mis_sess ){
  #   session_scope = session$ns(NULL)
  # }

  inner_ids <- names(meta$bind_infos)
  update_functions <- sapply(inner_ids, function(id, ...){
    bind_info <- meta$bind_infos[[id]]
    if(is.list(bind_info) && 'update_function' %in% names(bind_info)){
      update_function <- bind_info$update_function
      if(length(update_function)){
        update_function <- str2lang(update_function[[1]])
        input_names <- names(formals(eval(update_function)))

        # in case R says ii not found when checking
        ii <- NULL
        .session <- NULL
        fbody <- rlang::quo({
          session_scope <- .session$ns(NULL)
          session <- .session
          widget_name <- !!name
          if((length(session_scope) == 1) && session_scope != ''){
            # check whether name starts with session_scope if yes, this means
            # we register submodule in root session,
            # and we need to go back and remove session_scope in name
            slen <- stringr::str_length(session_scope)
            nlen <- stringr::str_length(widget_name)
            if( slen < nlen - 1 ){
              if( stringr::str_sub(widget_name, end = slen + 1) == sprintf('%s-', session_scope) ){
                # need to remove scope from widget_name
                widget_name <- stringr::str_sub(widget_name, start = slen + 2)
              }
            }
          }

          inputId <- sprintf('%s_%s_%s', widget_name, !!id, ii)
          call <- as.call(list(!!update_function, session = quote(session),
                               inputId = inputId, ...))
          if( !'...' %in% !!input_names){
            # Need to match call
            nms <- names(call)
            sel <- nms %in% c('', !!input_names)
            if(!all(sel)){
              call <- call[sel]
            }
          }
          re <- eval(call)
          # clean up in case of large memory leak?
          rm(session)
          re
        })

        f <- function(ii, ..., .session = shiny::getDefaultReactiveDomain()){
          # if(mis_sess){
          #   session <- shiny::getDefaultReactiveDomain()
          # }
          # inputId <- sprintf('%s_%s_%s', name, id, ii)
          # call <- as.call(list(update_function, session = quote(session),
          #                      inputId = inputId, ...))
          # if( !'...' %in% input_names){
          #   # Need to match call
          #   nms <- names(call)
          #   sel <- nms %in% c('', input_names)
          #   if(!all(sel)){
          #     call <- call[sel]
          #   }
          # }
          # eval(call)
        }

        body(f) <- rlang::quo_squash(fbody)
        environment(f) <- baseenv()
        return( f )
      }
    }
    return(NULL)
  }, simplify = FALSE, USE.NAMES = TRUE)
  attr(data, 'update_functions') <- update_functions
  attr(data, 'meta') <- meta
  class(data) <- c('dipsaus_compoundInput_data', 'list')
  return(data)
}

registerCompoundInput2 <- function(){
  # register input
  shiny::registerInputHandler("dipsaus.compoundInput2", function(data, shinysession, name) {
    translate_compoundInput(data, shinysession, name)
    # data
  }, force = TRUE)
}

#' @export
print.dipsaus_compoundInput_data <- function(x, ...){
  local({
    attributes(x) <- NULL
    base::print(x)
  })
  invisible(x)
}

#' Update compound inputs
#' @param session shiny session or session proxy
#' @param inputId character see \code{compoundInput2}
#' @param value list of lists, see \code{compoundInput2} or examples
#' @param ncomp integer, non-negative number of groups to update, \code{NULL} to
#' remain unchanged
#' @param initialization,... named list of other updates
#' @return none
#' @examples
#'
#' \dontrun{
#' library(shiny); library(dipsaus)
#'
#' ## UI side
#' compoundInput2(
#'   'input_id', 'Group',
#'     div(
#'     textInput('text', 'Text Label'),
#'     sliderInput('sli', 'Slider Selector', value = 0, min = 1, max = 1)
#'   ),
#'   label_color = 1:10,
#'   value = list(
#'     list(text = '1'),  # Set text first group to be "1"
#'     '',                # no settings for second group
#'     list(sli = 0.2)    # sli = 0.2 for the third group
#'   ))
#'
#' ## server side:
#' updateCompoundInput2(session, 'inputid',
#'                      # Change the first 3 groups
#'                      value = lapply(1:3, function(ii){
#'                        list(sli = runif(1))
#'                      }),
#'                      # Change text label for all groups
#'                      initialization = list(
#'                        text = list(label = as.character(Sys.time()))
#'                      ))
#' }
#'
#' @seealso \code{\link[dipsaus]{compoundInput2}} for how to define components.
#'
#' @export
updateCompoundInput2 <- function(session, inputId, value = NULL, ncomp = NULL,
                                 initialization = NULL, ...) {
  if(!is.list(value)){
    value <- list()
  }
  value <- lapply(seq_along(value), function(ii){
    g <- value[[ii]]
    if(is.list(g)){
      g$.__item <- ii
    }else{
      g <- NULL
    }
    g
  })
  initialization <- c(initialization, list(...))

  sample <- shiny::isolate(session$input[[ inputId ]])
  if(is.null(sample)){
    if(is.environment(session$userData$dipsaus_reserved)){
      sample <- session$userData$dipsaus_reserved$compount_inputs$get(session$ns(inputId))
    }
  }

  update_functions <- attr(sample, 'update_functions')
  meta <- attr(sample, 'meta')
  max_ncomp <- meta$max_ncomp
  if(!length(max_ncomp) || !is.numeric(max_ncomp) || max_ncomp <= 0){
    max_ncomp <- 100
  }

  if(!is.list(update_functions)){
    update_functions <- list()
  }

  # Make n-components, and subscribe events
  session$sendInputMessage(inputId, list(value = value, ncomp = ncomp))

  for(nm in names(initialization)){
    uf <- update_functions[[ nm ]]
    if(is.function(uf)){
      lapply(seq_len(max_ncomp), function(ii){
        do.call(uf, c(list(ii = ii), initialization[[nm]]))
      })
    }
  }

  session$sendInputMessage(inputId, list(value = value, ncomp = ncomp))
  invisible()
}


