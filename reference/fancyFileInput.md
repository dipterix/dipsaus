# Shiny drag-and-drop file input

Fancy drag and drop file upload for `shiny` apps.

## Usage

``` r
fancyFileInput(
  inputId,
  label,
  width = NULL,
  after_content = "Drag & drop, or button",
  size = c("s", "m", "l", "xl"),
  ...
)
```

## Arguments

- inputId:

  the input slot that will be used to access the value

- label:

  display label for the control, or NULL for no label.

- width:

  the width of the input

- after_content:

  tiny content that is to be displayed below the input box

- size:

  height of the widget, choices are `'s'`, `'m'`, `'l'`, and `'xl'`

- ...:

  passed to [`fileInput`](https://rdrr.io/pkg/shiny/man/fileInput.html)

## Value

See [`fileInput`](https://rdrr.io/pkg/shiny/man/fileInput.html)

## Examples

``` r

library(shiny)
library(dipsaus)

ui <- basicPage(
  fancyFileInput('file_input', "Please upload")
)

if(interactive()) {
  shinyApp(
    ui, server = function(input, output, session){},
    options = list(launch.browser = TRUE)
  )
}
```
