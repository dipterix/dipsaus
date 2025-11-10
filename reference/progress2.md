# 'Shiny' progress bar, but can run without reactive context

'Shiny' progress bar, but can run without reactive context

## Usage

``` r
progress2(
  title,
  max = 1,
  ...,
  quiet = FALSE,
  session = shiny::getDefaultReactiveDomain(),
  shiny_auto_close = FALSE,
  log = NULL
)
```

## Arguments

- title:

  character, task description

- max:

  maximum number of items in the queue

- ...:

  passed to `shiny::Progress$new(...)`

- quiet:

  suppress console output, ignored in shiny context.

- session:

  'shiny' session, default is current reactive domain

- shiny_auto_close:

  logical, automatically close 'shiny' progress bar once current
  observer is over. Default is `FALSE`. If setting to `TRUE`, then it's
  equivalent to `p <- progress2(...); on.exit({p$close()}, add = TRUE)`.

- log:

  function when running locally, default is `NULL`, which redirects to
  [`cat2`](https://dipterix.org/dipsaus/reference/cat2.md)

## Value

A list of functions:

- `inc(detail, message = NULL, amount = 1, ...)`:

  Increase progress bar by `amount` (default is 1).

- [`close()`](https://rdrr.io/r/base/connections.html):

  Close the progress

- `reset(detail = '', message = '', value = 0)`:

  Reset the progress to `value` (default is 0), and reset information

- `get_value()`:

  Get current progress value

- `is_closed()`:

  Returns logical value if the progress is closed or not.

## Examples

``` r
progress <- progress2('Task A', max = 2)
#> [Task A]: initializing... 
progress$inc('Detail 1')
#> [Task A]: Detail 1 (1 out of 2) 
progress$inc('Detail 2')
#> [Task A]: Detail 2 (2 out of 2) 
progress$close()
#> Finished 

# Check if progress is closed
progress$is_closed()
#> [1] TRUE

# ------------------------------ Shiny Example ------------------------------
library(shiny)
library(dipsaus)

ui <- fluidPage(
  actionButtonStyled('do', 'Click Here', type = 'primary')
)

server <- function(input, output, session) {
  observeEvent(input$do, {
    updateActionButtonStyled(session, 'do', disabled = TRUE)
    progress <- progress2('Task A', max = 10, shiny_auto_close = TRUE)
    lapply(1:10, function(ii){
      progress$inc(sprintf('Detail %d', ii))
      Sys.sleep(0.2)
    })
    updateActionButtonStyled(session, 'do', disabled = FALSE)
  })
}

if(interactive()){
  shinyApp(ui, server)
}
```
