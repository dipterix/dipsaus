# Shiny Customized Widgets

### 1. Styled Action Button

The default `shiny` `actionButton` cannot fully use `Bootstrap` theme or
the full features defined by `HTML`. For example, the button class is
always `btn btn-default` and we can’t disable/enable the button in an
easy way.

`actionButtonStyled` solves these two problems:

- `type` allows you to add `Bootstrap` styles to the button. The
  supported types are: `default`, `primary`, `info`, `success`,
  `warning`, `danger`
- if you have customized CSS styles, `class` provides more flexible way
  to add classes
- when updating the button, `disabled` allows to disable/enable the
  button

The usage is listed as follows:

> Note the website must include `Bootstrap` library. If you are viewing
> this from default `CRAN` vignette, you might want to [check this
> link](https://dipterix.org/dipsaus/articles/shiny_customized_widgets.html)
> to view the demo.

``` r

# UI function
actionButtonStyled(inputId, label, icon = NULL, width = NULL, 
                   btn_type = "button", type = "primary", class = "", ...)

# Update function
updateActionButtonStyled(session, inputId, label = NULL, icon = NULL,
                         type = NULL, disabled = NULL, ...)
```

Show-case:

[TABLE]

### 2. Compound Inputs (grouped Inputs)

`compoundInput2` provides group inputs where each group contains
multiple shiny inputs. For examples

``` r
compoundInput2(
  'compound', 'Group Label', label_color = 1:10,
  components = div(
    textInput('txt', 'Text'),
    selectInput('sel', 'Select', choices = 1:10, multiple = TRUE),
    sliderInput('sli', 'Slider', max=1, min=0, val=0.5)
  ), max_ncomp = 10, min_ncomp = 0, initial_ncomp = 1
)
```

will create a list of input groups with minimum of 0 but maximum of 10.
User can control the size of groups by pressing `+` and `-` buttons. The
value `input$components` looks like this:

    #> [[1]]
    #> [[1]]$txt
    #> [1] ""
    #> 
    #> [[1]]$sel
    #> [1] "1" "3" 
    #> 
    #> [[1]]$sli
    #> [1] 0.5
    #> 
    #> [[2]]
    ...

I found this input extremely useful in clinic trial when the experiment
condition is grouped and developers don’t know ahead the number of
condition groups.

The further details can be found with
`demo('example-compountInput2', package='dipsaus')`. The source file can
be found using
`system.file('demo/example-compountInput2.R', package='dipsaus')`.

### 3. Synchronize Multiple Inputs

`sync_shiny_inputs` provides a way to update among multiple inputs with
no dead-locks. For example, input A (`textInput`) shares the same
information as input B (`sliderInput`). Updating A would trigger B to
update. What if you want A to be updated when B is changed? The
following code might cause recursive updates:

``` r
# Bad example
observeEvent(input$A, {
  updateSliderInput(session, 'B', value = input$A)
})
observeEvent(input$B, {
  updateTextInput(session, 'A', value = input$B)
})
```

In this case, you can use `sync_shiny_inputs`:

``` r
sync_shiny_inputs(input, session, inputIds = c('A', 'B'), uniform = list(
  function(a){as.numeric(a)},
  function(b){ b }
), updates = list(
  function(a){updateTextInput(session, 'A', value = a)},
  function(b){updateSliderInput(session, 'B', value = b)}
))
```

`inputIds` refers to the input ID to be synchronized. When one or more
of the inputs are changed, the value will be passed to the corresponding
`uniform` functions and stored, then `updates` will notify each inputs
to update the UI values. The input of `updates` is the stored value.

For example, if input `A` is changed from `"0"` to `"1"`, then the first
function of `uniform` is triggered, `function(a){as.numeric(a)}` will be
evaluated with `a="1"`. The result, which is numeric `1` will be stored.
Next, each functions in `updates` will be called, with `1` (stored in
the previous step) as input, results in changing the slider input `B` to
`1`. This whole process will not trigger `A` to re-update.
