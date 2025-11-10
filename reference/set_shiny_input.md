# Set Shiny Input

Shiny \`input' object is read-only reactive list. When try to assign
values to input, errors usually occur. This method provides several
work-around to set values to input. Please use along with
[`use_shiny_dipsaus`](https://dipterix.org/dipsaus/reference/use_shiny_dipsaus.md).

## Usage

``` r
set_shiny_input(
  session = shiny::getDefaultReactiveDomain(),
  inputId,
  value,
  priority = c("event", "deferred", "immediate"),
  method = c("proxy", "serialize", "value", "expression"),
  quoted = TRUE
)
```

## Arguments

- session:

  shiny session, see shiny
  [`domains`](https://rdrr.io/pkg/shiny/man/domains.html)

- inputId:

  character, input ID

- value:

  the value to assign

- priority:

  characters, options are "event", "deferred", and "immediate". "event"
  and "immediate" are similar, they always fire changes. "deferred" fire
  signals to other reactive/observers only when the input value has been
  changed

- method:

  characters, options are "proxy", "serialize", "value", "expression".
  "proxy" is recommended, other methods are experimental.

- quoted:

  is value quoted? Only used when method is "expression"

## Examples

``` r
library(shiny)
library(dipsaus)
ui <- fluidPage(
  # Register widgets
  use_shiny_dipsaus(),
  actionButton('run', 'Set Input'),
  verbatimTextOutput('input_value')
)

server <- function(input, output, session) {
  start = Sys.time()

  output$input_value <- renderPrint({

    now <- input$key
    now %?<-% start
    cat('This app has been opened for ',
        difftime(now, start, units = 'sec'), ' seconds')
  })

  observeEvent(input$run, {
    # setting input$key to Sys.time()
    set_shiny_input(session, 'key', Sys.time())
  })
}

if(interactive()){
  shinyApp(ui, server)
}

```
