# Take a screenshot in shiny apps

Take a screenshot of the whole page and save encoded `DataURI` that can
be accessed via `input[[inputId]]`.

## Usage

``` r
screenshot(inputId, session = shiny::getDefaultReactiveDomain())
```

## Arguments

- inputId:

  the input id where the screenshot should be

- session:

  shiny session

## Value

None. However, the screenshot results can be accessed from shiny input

## Examples

``` r
library(shiny)
library(dipsaus)
ui <- fluidPage(
  tagList(
    shiny::singleton(shiny::tags$head(
      shiny::tags$link(rel="stylesheet", type="text/css", href="dipsaus/dipsaus.css"),
      shiny::tags$script(src="dipsaus/dipsaus-dipterix-lib.js")
    ))
  ),
  actionButtonStyled('do', 'Take Screenshot'),
  compoundInput2('group', label = 'Group', components = list(
    textInput('txt', 'Enter something here')
  ))
)

server <- function(input, output, session) {
  observeEvent(input$do, {
    screenshot('screeshot_result')
  })
  observeEvent(input$screeshot_result, {
    showModal(modalDialog(
      tags$img(src = input$screeshot_result, width = '100%')
    ))
  })
}

if(interactive()){
  shinyApp(ui, server)
}
```
