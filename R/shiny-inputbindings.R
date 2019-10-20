# Defines shiny bindings

shiny_input_bindings = new.env(parent = emptyenv())
list2env(list(
  'shiny.textInput' = list(
    binding = "shiny.textInput",
    update_function = "shiny::updateTextInput"
  ),
  'shiny.textAreaInput' = list(
    binding = "shiny.textareaInput",
    update_function = "shiny::updateTextAreaInput"
  ),
  'shiny.passwordInput' = list(
    binding = "shiny.passwordInput",
    update_function = "shiny::updateTextInput"
  ),
  'shiny.numericInput' = list(
    binding = "shiny.numberInput",
    update_function = "shiny::updateNumericInput"
  ),
  'shiny.checkboxInput' = list(
    binding = "shiny.checkboxInput",
    update_function = "shiny::updateCheckboxInput"
  ),
  'shiny.sliderInput' = list(
    binding = "shiny.sliderInput",
    update_function = "shiny::updateSliderInput"
  ),
  'shiny.dateInput' = list(
    binding = "shiny.dateInput",
    update_function = "shiny::updateDateInput"
  ),
  'shiny.dateRangeInput' = list(
    binding = "shiny.dateRangeInput",
    update_function = 'shiny::updateDateRangeInput'
  ),
  'shiny.selectInput' = list(
    binding = "shiny.selectInput",
    update_function = 'shiny::updateSelectInput'
  ),
  'shiny.selectizeInput' = list(
    binding = "shiny.selectInput",
    update_function = 'shiny::updateSelectizeInput'
  ),
  'shiny.varSelectInput' = list(
    binding = "shiny.selectInput",
    update_function = 'shiny::updateVarSelectInput'
  ),
  'shiny.varSelectizeInput' = list(
    binding = "shiny.selectInput",
    update_function = 'shiny::updateVarSelectizeInput'
  ),
  'shiny.radioButtons' = list(
    binding = "shiny.radioInput",
    update_function = 'shiny::updateRadioButtons'
  ),
  'shiny.checkboxGroupInput' = list(
    binding = "shiny.checkboxGroupInput",
    update_function = 'shiny::updateCheckboxGroupInput'
  ),
  'shiny.actionButton' = list(
    binding = "shiny.actionButtonInput",
    update_function = 'shiny::updateActionButton'
  ),
  'shiny.actionLink' = list(
    binding = "shiny.actionButtonInput",
    update_function = 'shiny::updateActionButton'
  ),
  'shiny.fileInput' = list(
    binding = "shiny.fileInputBinding",
    update_function = NULL
  ),
  'dipsaus.compoundInput2' = list(
    binding = "dipsaus.compoundInput2",
    update_function = 'dipsaus.updateCompoundInput2'
  ),
  'dipsaus.actionButtonStyled' = list(
    binding = "shiny.actionButtonInput",
    update_function = 'dipsaus.updateActionButtonStyled'
  )
), envir = shiny_input_bindings)


#' Register customized input to enable support by compound input
#' @param fname character, function name, such as \code{"textInput"}
#' @param pkg character, package name, like \code{"shiny"}
#' @param shiny_binding character, 'JavaScript' binding name.See examples
#' @param update_function character, update function such as \code{"shiny::textInput"}
#' @examples
#'
#' # register shiny textInput
#' registerInputBinding('textInput', 'shiny',
#'                      'shiny.textInput', 'shiny::updateTextInput')
#'
#' # Register shiny actionLink
#' # In "Shiny.inputbindings", the binding name is "shiny.actionButtonInput",
#' # Shiny update function is "shiny::updateActionButton"
#' registerInputBinding('actionLink', 'shiny',
#'                      'shiny.actionButtonInput', 'shiny::updateActionButton')
#'
#' @export
registerInputBinding <- function(fname, pkg, shiny_binding, update_function = NULL){
  ns = asNamespace(pkg)
  if( !is.function(ns[[fname]]) ){
    stop(sprintf('%s::%s is not a function', pkg, fname))
  }
  shiny_input_bindings[[sprintf('%s.%s', pkg, fname)]] = list(
    binding = shiny_binding,
    update_function = update_function
  )
}

#' Obtain registered input bindings
#' @param fname input function name, character or quoted expression
#' such as \code{'shiny::textInput'} or \code{numericInput}.
#' @param pkg (optional), name of package
#' @param envir environment to evaluate \code{fname} if \code{pkg} is not provided
#'
#' @return a list containing: 1. `JavaScript` input binding name; 2. `R` updating function name
#'
#' @examples
#'
#' # Most recommended usage
#' getInputBinding('compoundInput2', pkg = 'dipsaus')
#'
#' # Other usages
#' getInputBinding('shiny::textInput')
#'
#'
#' getInputBinding(shiny::textInput)
#'
#' getInputBinding(compoundInput2, pkg = 'dipsaus')
#'
#' # Bad usage
#' \donttest{
#'
#' #### You need to library(shiny), or set envir=asNamespace('shiny'), or pkg='shiny'
#' # getInputBinding('textInput')
#' # getInputBinding(textInput) also fails
#'
#' #### Always fails
#' # getInputBinding('dipsaus::compoundInput2', pkg = 'dipsaus')
#' }
#'
#' @export
getInputBinding <- function(fname, pkg = NULL, envir = parent.frame()){
  if( length(pkg) != 1 || !is.character(pkg) ){
    # need to get package from fname

    if(is.character(fname)){
      fname = str2lang(fname)
    }

    fname_quoted = substitute(fname)
    if( !is.language(fname) ){
      fname = fname_quoted
    }

    # now we have quoted fname
    if(is.call(fname) && all(as.character(fname[[1]]) %in% c('::', ':::'))){
      pkg = deparse(fname[[2]])
      fname = deparse(fname[[3]])
    }else{
      f = eval(fname, envir = envir)
      fenv = environment(f)
      if( isNamespace(fenv) ){
        pkg = fenv$.__NAMESPACE__.$spec[['name']]
      }
      fname = deparse(fname)
    }
  }else{
    fname_quoted = substitute(fname)
    if(!is.character(fname)){
      fname = deparse(fname_quoted)
    }
  }
  # Check whether fname exists
  if(is.null(pkg)){
    stop(sprintf('Cannot find function %s in any package loaded from envir. Please provide package name', fname))
  }
  ns = asNamespace(pkg)
  if(!is.function(ns[[fname]])){
    stop(sprintf('Cannot find function %s in namespace %s', fname, pkg))
  }
  binding_key = sprintf('%s.%s', pkg, fname)
  binding_re = shiny_input_bindings[[ binding_key ]]
  if(is.null(binding_re)){
    stop(sprintf('Cannot find input binding for %s. Please use\n\tdipsaus::registerInputBinding(%s, %s, shiny_binding, update_function = NULL)\n  to register this input type.', binding_key, fname, pkg))
  }
  binding_re$call_function = sprintf('%s::%s', pkg, fname)
  binding_re
}