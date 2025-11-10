# Register customized input to enable support by compound input

Register customized input to enable support by compound input

## Usage

``` r
registerInputBinding(
  fname,
  pkg,
  shiny_binding,
  update_function = NULL,
  quiet = FALSE
)
```

## Arguments

- fname:

  character, function name, such as `"textInput"`

- pkg:

  character, package name, like `"shiny"`

- shiny_binding:

  character, 'JavaScript' binding name.See examples

- update_function:

  character, update function such as `"shiny::textInput"`

- quiet:

  logical, whether to suppress warnings

## Value

a list of binding functions, one is \`JavaScript\` object key in
`Shiny.inputBindings`, the other is \`shiny\` update function in R end.

## Examples

``` r
# register shiny textInput
registerInputBinding('textInput', 'shiny',
                     'shiny.textInput', 'shiny::updateTextInput')

# Register shiny actionLink
# In "Shiny.inputbindings", the binding name is "shiny.actionButtonInput",
# Shiny update function is "shiny::updateActionButton"
registerInputBinding('actionLink', 'shiny',
                     'shiny.actionButtonInput', 'shiny::updateActionButton')
```
