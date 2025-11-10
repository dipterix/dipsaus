# Escape HTML strings

Escape HTML strings so that they will be displayed 'as-is' in websites.

## Usage

``` r
html_asis(s, space = TRUE)
```

## Arguments

- s:

  characters

- space:

  whether to also escape white space, default is true.

## Value

An R string

## Examples

``` r
ui <- flex_div(
  shiny::textInput('id', ' ', width = '100%',
                   value = 'Height not aligned'),
  actionButtonStyled('ok', 'Button1', width = '100%',),
  shiny::textInput('id2', html_asis(' '), width = '100%',
                   value = 'Heights aligned'),
  actionButtonStyled('ok2', 'Button2', width = '100%',),
  ncols = 2
)
if(interactive()){
  shiny::shinyApp(ui = shiny::fluidPage(shiny::fluidRow(ui)),
                  server = function(input, output, session){})
}
```
