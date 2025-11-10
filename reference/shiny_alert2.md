# Simple shiny alert that uses 'JavaScript' promises

Simple shiny alert that uses 'JavaScript' promises

## Usage

``` r
shiny_alert2(
  title = "Alert",
  text = "",
  icon = c("info", "warning", "success", "error"),
  danger_mode = FALSE,
  auto_close = TRUE,
  buttons = NULL,
  on_close = NULL,
  session = shiny::getDefaultReactiveDomain()
)

close_alert2(session = shiny::getDefaultReactiveDomain())
```

## Arguments

- title:

  title of the alert

- text:

  alert body text (pure text)

- icon:

  which icon to display, choices are `'info'`, `'success'` `'warning'`,
  and `'error'`

- danger_mode:

  true or false; if true, then the confirm button turns red and the
  default focus is set on the cancel button instead. To enable danger
  mode, `buttons` must be `TRUE` as well

- auto_close:

  whether to close automatically when clicking outside of the alert

- buttons:

  logical value or a named list, or characters. If logical, it indicates
  whether buttons should be displayed or not; for named list, the names
  will be the button text, see example; for characters, the characters
  will be the button text and value

- on_close:

  `NULL` or a function that takes in one argument. If function is passed
  in, then it will be executed when users close the alert

- session:

  shiny session, see
  [`domains`](https://rdrr.io/pkg/shiny/man/domains.html)

## Value

a temporary input ID, currently not useful

## Examples

``` r
library(shiny)
library(dipsaus)
ui <- fluidPage(
  use_shiny_dipsaus(),
  actionButtonStyled('btn', 'btn')
)

server <- function(input, output, session) {
  observeEvent(input$btn, {
    shiny_alert2(
      on_close = function(value) {
        cat("Modal closed!\n")
        print(value)
      },
      title = "Title",
      text = "message",
      icon = "success",
      auto_close = FALSE,
      buttons = list("cancel" = TRUE,
                     "YES!" = list(value = 1))
    )
  })
}

if(interactive()){
  shinyApp(ui, server, options = list(launch.browser = TRUE))
}
```
