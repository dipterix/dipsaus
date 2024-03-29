% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny-compoundInput2.R
\name{compoundInput2}
\alias{compoundInput2}
\title{Compound input that combines and extends shiny inputs}
\usage{
compoundInput2(
  inputId,
  label = "Group",
  components = shiny::tagList(),
  initial_ncomp = 1,
  min_ncomp = 0,
  max_ncomp = 10,
  value = NULL,
  label_color = NA,
  max_height = NULL,
  ...
)
}
\arguments{
\item{inputId}{character, shiny input ID}

\item{label}{character, will show on each groups}

\item{components}{`HTML` tags that defines and combines HTML components within groups}

\item{initial_ncomp}{numeric initial number of groups to show, non-negative}

\item{min_ncomp}{minimum number of groups, default is 0, non-negative}

\item{max_ncomp}{maximum number of groups, default is 10, greater or equal
than \code{min_ncomp}}

\item{value}{list of lists, initial values of each inputs, see examples.}

\item{label_color}{integer or characters, length of 1 or \code{max_ncomp}, assigning colors to each group labels; default is \code{NA}, and try to get color from foreground \code{par("fg")}}

\item{max_height}{maximum height of the widget}

\item{...}{will be ignored}
}
\value{
`HTML` tags
}
\description{
Compound input that combines and extends shiny inputs
}
\examples{
library(shiny); library(dipsaus)
compoundInput2(
  'input_id', 'Group',
    div(
    textInput('text', 'Text Label'),
    sliderInput('sli', 'Slider Selector', value = 0, min = 1, max = 1)
  ),
  label_color = 1:10,
  value = list(
    list(text = '1'),  # Set text first group to be "1"
    list(),                # no settings for second group
    list(sli = 0.2)    # sli = 0.2 for the third group
  ))

# Source - system.file('demo/example-compountInput2.R', package='dipsaus')

# demo('example-compountInput2', package='dipsaus')

library(shiny)
library(dipsaus)
ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      compoundInput2(
        'compound', 'Group Label', label_color = c(NA,1:9),
        components = div(
          textInput('txt', 'Text'),
          selectInput('sel', 'Select', choices = 1:10, multiple = TRUE),
          sliderInput('sli', 'Slider', max=1, min=0, val=0.5)
        ),
        value = list(
          list(txt = '1'),  # Set text first group to be "1"
          '',                # no settings for second group
          list(sli = 0.2)    # sli = 0.2 for the third group
        )
      ),
      hr(),
      actionButton('action', 'Update compound input')
    )
  )
)

server <- function(input, output, session) {
  observe({
    print(input$compound)
  })
  observe({
    # Getting specific input at group 1
    print(input$compound_txt_1)
  })
  observeEvent(input$action, {
    updateCompoundInput2(
      session, 'compound',
      # Update values for each components
      value = lapply(1:5, function(ii){
        list(
          txt = sample(LETTERS, 1),
          sel = sample(1:10, 3),
          sli = runif(1)
        )
      }), ncomp = NULL, txt = list(label = as.character(Sys.time())))
  })
}

if( interactive() ){
  shinyApp(ui, server, options = list(launch.browser = TRUE))
}

}
\seealso{
\code{\link[dipsaus]{updateCompoundInput2}} for how to update inputs
}
