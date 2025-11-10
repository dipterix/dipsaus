# Generate Shiny element with arrangement automatically

Generate Shiny element with arrangement automatically

## Usage

``` r
flex_div(..., ncols = "auto")
```

## Arguments

- ...:

  shiny UI elements

- ncols:

  number of columns, either `"auto"` or vector of positive integers

## Value

HTML objects

## Details

If multiple numbers of columns are specified, `flex_div` will guess the
best size that will be applied. For button UI, `flex_div` automatically
add `"20px"` on the top margin.

## Examples

``` r
ui <- flex_div(
  shiny::selectInput('sel', label = 'Select input',
                     choices = '', width = '100%'),
  shiny::textInput('id2', label = html_asis(' '), width = '100%',
                   value = 'Heights aligned'),
  actionButtonStyled('ok2', 'Button', width = '100%',),
  shiny::sliderInput('sl', 'Item 4', min = 1, max = 2,
                     value = 1.5, width = '100%'),
  shiny::fileInput('aa', 'item 5', width = '100%'),
  ncols = c(2,3) # Try to assign 2 or 3 items per column
)
if(interactive()){
  shiny::shinyApp(ui = shiny::fluidPage(shiny::fluidRow(ui)),
                  server = function(input, output, session){})
}
```
