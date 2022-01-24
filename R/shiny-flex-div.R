# Combine multiple inputs using flex layout

#' Generate Shiny element with arrangement automatically
#' @param ... shiny UI elements
#' @param ncols number of columns, either \code{"auto"} or
#' vector of positive integers
#' @return HTML objects
#' @details If multiple numbers of columns are specified,
#' \code{flex_div} will guess the best size that will be applied.
#' For button UI, \code{flex_div} automatically add \code{"20px"}
#' on the top margin.
#' @examples
#' ui <- flex_div(
#'   shiny::selectInput('sel', label = 'Select input',
#'                      choices = '', width = '100%'),
#'   shiny::textInput('id2', label = html_asis(' '), width = '100%',
#'                    value = 'Heights aligned'),
#'   actionButtonStyled('ok2', 'Button', width = '100%',),
#'   shiny::sliderInput('sl', 'Item 4', min = 1, max = 2,
#'                      value = 1.5, width = '100%'),
#'   shiny::fileInput('aa', 'item 5', width = '100%'),
#'   ncols = c(2,3) # Try to assign 2 or 3 items per column
#' )
#' if(interactive()){
#'   shiny::shinyApp(ui = shiny::fluidPage(shiny::fluidRow(ui)),
#'                   server = function(input, output, session){})
#' }
#' @export
flex_div <- function(..., ncols = 'auto'){
  stopifnot2(isTRUE(ncols == 'auto') || all(ncols >=1) ,
             msg = 'ncols must be either "auto" or integers greater than 0')

  call <- match.call()
  if('ncols' %in% names(call)){
    call[['ncols']] <- NULL
  }
  elements <- as.list(call)[-1]
  n <- length(elements)
  if(isTRUE(ncols == 'auto')){
    if(n <= 3){
      d <- c(1, n)
    } else {
      d <- grDevices::n2mfrow(n)
    }
    flex_basis <- floor(10000 / d[[2]]) / 100
  } else {
    ncols <- sort(ceiling(ncols))
    resid <- n %% ncols
    if(any(resid == 0)){
      ncols <- ncols[resid == 0][[1]]
      d <- c(n/ncols, ncols)
      flex_basis <- floor(10000 / d[[2]]) / 100
    } else {
      resid1 <- ncols - resid
      ncol1 <- max(ncols[resid1 == min(resid1)])
      flex_basis <- floor(10000 / ncol1) / 100
      d <- c(ceiling(n / ncol1), ncol1)
    }
  }
  els <- lapply(seq_along(elements), function(ii){
    style <- sprintf('flex-basis:%.2f%%;', flex_basis)
    if( ii %% d[[2]] == 0 ){
      bk <- quote(shiny::div(class="dipsaus-flexdiv-break"))
    } else {
      bk <- NULL
    }

    as.call(list(
      quote(shiny::tagList),
      as.call(list(
        quote(shiny::div),
        class="dipsaus-flexdiv-item",
        style = style,
        elements[[ii]]
      )),
      bk
    ))
  })

  new_call <- as.call(c(list(
    quote(shiny::div),
    class = "dipsaus-flexdiv-container",
    shiny::singleton(
      shiny::tags$style(
        '.dipsaus-flexdiv-container { display:flex; flex-wrap: wrap; }',
        '.dipsaus-flexdiv-item { flex: 1; padding: 0 7px; }',
        shiny::HTML('.dipsaus-flexdiv-item>.btn { margin-top: 25px; }'),
        '.dipsaus-flexdiv-break { flex-basis: 100%; height: 0; }'
      )
    )
  ), els))
  eval(new_call, parent.frame())

}

#' Escape HTML strings
#' @description Escape HTML strings so that they will be displayed
#' 'as-is' in websites.
#' @param s characters
#' @param space whether to also escape white space, default is true.
#' @return An R string
#' @examples
#'
#' ui <- flex_div(
#'   shiny::textInput('id', ' ', width = '100%',
#'                    value = 'Height not aligned'),
#'   actionButtonStyled('ok', 'Button1', width = '100%',),
#'   shiny::textInput('id2', html_asis(' '), width = '100%',
#'                    value = 'Heights aligned'),
#'   actionButtonStyled('ok2', 'Button2', width = '100%',),
#'   ncols = 2
#' )
#' if(interactive()){
#'   shiny::shinyApp(ui = shiny::fluidPage(shiny::fluidRow(ui)),
#'                   server = function(input, output, session){})
#' }
#'
#' @export
html_asis <- function(s, space = TRUE){
  if(space){
    pattern <- "&|<|>| |\t" # or "&|<|>|'|\"|\r|\n"
    specials <- list(
      "&" = "&amp;",
      "<" = "&lt;",
      ">" = "&gt;",
      # "'" = "&#39;",
      # '"' = "&quot;",
      # "\r" = "&#13;",
      # "\n" = "&#10;",
      " " = "&nbsp;",
      "\t" = "&nbsp;&nbsp;&nbsp;&nbsp;"
    )
  } else {
    pattern <- "&|<|>|"
    specials <- list(
      "&" = "&amp;",
      "<" = "&lt;",
      ">" = "&gt;"
    )
  }

  s <- enc2utf8(as.character(s))
  if (any(grepl(pattern, s, useBytes = TRUE))) {
    for (chr in names(specials)) {
      s <- gsub(chr, specials[[chr]], s, fixed = TRUE,
                useBytes = TRUE)
    }
  }
  Encoding(s) <- "UTF-8"
  shiny::HTML(s)
}

#' @name html_class
#' @title Combine, add, or remove 'HTML' classes
#' @description Combine 'HTML' classes to produce nice, clean 'HTML' class
#' string via \code{combine_html_class}, or to remove a class via
#' \code{remove_html_class}
#' @param ... one or more characters, classes to combine; duplicated classes
#' will be removed
#' @param target characters, class list
#' @param class one or more characters, classes to be removed from \code{target}
#' @return A character string of new 'HTML' class
#' @examples
#'
#' # Combine classes "a b c d e"
#' combine_html_class("a", "b  a", c("c", " d", "b"), list("e ", "a"))
#'
#' # Remove class
#' remove_html_class("a b   c  e", c("b", "c "))
#'
#' @export
combine_html_class <- function(...){
  s <- paste(c(...), collapse = " ", sep = " ")
  s <- unlist(strsplit(s, " "))
  s <- unique(s)
  s <- s[!s %in% '']
  paste(s, collapse = " ")
}

#' @rdname html_class
#' @export
remove_html_class <- function(target, class){
  if (!length(target)) { return("") }
  s <- unlist(strsplit(target, " "))
  s <- unique(s)
  class <- unlist(strsplit(class, " "))
  s <- s[!s %in% c('', class)]
  paste(s, collapse = " ")
}
