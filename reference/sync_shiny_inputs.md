# Synchronize Shiny Inputs

Synchronize Shiny Inputs

## Usage

``` r
sync_shiny_inputs(
  input,
  session,
  inputIds,
  uniform = rep("I", length(inputIds)),
  updates,
  snap = 250,
  ignoreNULL = TRUE,
  ignoreInit = FALSE
)
```

## Arguments

- input, session:

  shiny reactive objects

- inputIds:

  input ids to be synchronized

- uniform:

  functions, equaling to length of `inputIds`, converting inputs to a
  uniform values

- updates:

  functions, equaling to length of `inputIds`, updating input values

- snap:

  numeric, milliseconds to defer the changes

- ignoreNULL, ignoreInit:

  passed to [`bindEvent`](https://rdrr.io/pkg/shiny/man/bindEvent.html)

## Value

none.

## Examples

``` r
library(shiny)

ui <- fluidPage(
  textInput('a', 'a', value = 'a'),
  sliderInput('b', 'b', value = 1, min = 0, max = 1000)
)

server <- function(input, output, session) {
  sync_shiny_inputs(input, session, inputIds = c('a', 'b'), uniform = list(
    function(a){as.numeric(a)},
    'I'
  ), updates = list(
    function(a){updateTextInput(session, 'a', value = a)},
    function(b){updateSliderInput(session, 'b', value = b)}
  ))

}

if( interactive() ){
  shinyApp(ui, server)
}
```
