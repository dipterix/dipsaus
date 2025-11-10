# Action Button but with customized styles

Action Button but with customized styles

## Usage

``` r
actionButtonStyled(
  inputId,
  label,
  icon = NULL,
  width = NULL,
  type = "primary",
  btn_type = "button",
  class = "",
  ...
)
```

## Arguments

- inputId, label, icon, width, ...:

  passed to
  [`shiny::actionButton`](https://rdrr.io/pkg/shiny/man/actionButton.html)

- type:

  button type, choices are \`default\`, \`primary\`, \`info\`,
  \`success\`, \`warning\`, and \`danger\`

- btn_type:

  HTML tag type, either `"button"` or `"a"`

- class:

  additional classes to be added to the button

## Value

\`HTML\` tags

## See also

[`updateActionButtonStyled`](https://dipterix.org/dipsaus/reference/updateActionButtonStyled.md)
for how to update the button.

## Examples

``` r
# demo('example-actionButtonStyled', package='dipsaus')

library(shiny)
library(dipsaus)

ui <- fluidPage(
  actionButtonStyled('btn', label = 'Click me', type = 'default'),
  actionButtonStyled('btn2', label = 'Click me2', type = 'primary')
)


server <- function(input, output, session) {
  btn_types = c('default', 'primary', 'info', 'success', 'warning', 'danger')
  observeEvent(input$btn, {
    btype = btn_types[((input$btn-1) %% (length(btn_types)-1)) + 1]
    updateActionButtonStyled(session, 'btn2', type = btype)
  })
  observeEvent(input$btn2, {
    updateActionButtonStyled(session, 'btn',
                             disabled = c(FALSE,TRUE)[(input$btn2 %% 2) + 1])
  })
}


if( interactive() ){
  shinyApp(ui, server, options = list(launch.browser=TRUE))
}
```
