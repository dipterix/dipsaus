# Compound input that combines and extends shiny inputs

Compound input that combines and extends shiny inputs

## Usage

``` r
compoundInput2(
  inputId,
  label = "Group",
  components = shiny::tagList(),
  initial_ncomp = 1,
  min_ncomp = 0,
  max_ncomp = 10,
  value = NULL,
  label_color = NA,
  max_height = NULL,
  ...
)
```

## Arguments

- inputId:

  character, shiny input ID

- label:

  character, will show on each groups

- components:

  \`HTML\` tags that defines and combines HTML components within groups

- initial_ncomp:

  numeric initial number of groups to show, non-negative

- min_ncomp:

  minimum number of groups, default is 0, non-negative

- max_ncomp:

  maximum number of groups, default is 10, greater or equal than
  `min_ncomp`

- value:

  list of lists, initial values of each inputs, see examples.

- label_color:

  integer or characters, length of 1 or `max_ncomp`, assigning colors to
  each group labels; default is `NA`, and try to get color from
  foreground `par("fg")`

- max_height:

  maximum height of the widget

- ...:

  will be ignored

## Value

\`HTML\` tags

## See also

[`updateCompoundInput2`](https://dipterix.org/dipsaus/reference/updateCompoundInput2.md)
for how to update inputs

## Examples

``` r
library(shiny); library(dipsaus)
compoundInput2(
  'input_id', 'Group',
    div(
    textInput('text', 'Text Label'),
    sliderInput('sli', 'Slider Selector', value = 0, min = 1, max = 1)
  ),
  label_color = 1:10,
  value = list(
    list(text = '1'),  # Set text first group to be "1"
    list(),                # no settings for second group
    list(sli = 0.2)    # sli = 0.2 for the third group
  ))
#> Warning: In sliderInput(): `value` should be greater than or equal to `min` (value = 0, min = 1).
#> Warning: In sliderInput(): `value` should be greater than or equal to `min` (value = 0, min = 1).
#> <div id="input_id" class="dipsaus-compound-input">
#>   <div class="dipsaus-compound-input-header force-hidden" style="display:none!important;">{"template":["&lt;div class=\"dipsaus-compound-input-item col-xs-12\" data-value=\"${{ind}}\"&gt;\n  &lt;fieldset style=\"border: 1px solid #efefef; padding:.35em .625em .75em; margin-bottom: 15px;\"&gt;\n    &lt;legend style=\"border:none; margin: 0; padding: 0 10px; font-size: 14px; color: ${{label_color}}\"&gt;Group - ${{ind}}&lt;\/legend&gt;\n    &lt;div&gt;\n      &lt;div class=\"form-group shiny-input-container\"&gt;\n        &lt;label class=\"control-label\" id=\"input_id_text_${{ind}}-label\" for=\"input_id_text_${{ind}}\"&gt;Text Label&lt;\/label&gt;\n        &lt;input id=\"input_id_text_${{ind}}\" type=\"text\" class=\"shiny-input-text form-control\" value=\"\" data-update-on=\"change\"/&gt;\n      &lt;\/div&gt;\n      &lt;div class=\"form-group shiny-input-container\"&gt;\n        &lt;label class=\"control-label\" id=\"input_id_sli_${{ind}}-label\" for=\"input_id_sli_${{ind}}\"&gt;Slider Selector&lt;\/label&gt;\n        &lt;input class=\"js-range-slider\" id=\"input_id_sli_${{ind}}\" data-skin=\"shiny\" data-min=\"1\" data-max=\"1\" data-from=\"0\" data-step=\"0.02\" data-grid=\"true\" data-grid-num=\"NaN\" data-grid-snap=\"false\" data-prettify-separator=\",\" data-prettify-enabled=\"true\" data-keyboard=\"true\" data-data-type=\"number\"/&gt;\n      &lt;\/div&gt;\n    &lt;\/div&gt;\n  &lt;\/fieldset&gt;\n&lt;\/div&gt;"],"initial_ncomp":[1],"min_ncomp":[0],"max_ncomp":[10],"bind_infos":{"text":{"binding":["shiny.textInput"],"update_function":["shiny::updateTextInput"],"call_function":["shiny::textInput"]},"sli":{"binding":["shiny.sliderInput"],"update_function":["shiny::updateSliderInput"],"call_function":["shiny::sliderInput"]}},"label_color":["#CD8500","#87CEFF","#CD8500","#87CEFF","#CD8500","#87CEFF","#CD8500","#87CEFF","#CD8500","#87CEFF"],"initial_value":[{"text":["1"]},[],{"sli":[0.2]}]}</div>
#>   <div class="dipsaus-compound-input-body row"></div>
#>   <div class="dipsaus-compound-input-foot">
#>     <div class="dipsaus-compound-input-foot-ctrl"></div>
#>   </div>
#>   <div class="force-hidden">
#>     <div class="dipsaus-compound-input-item col-xs-12" data-value="...junk...">
#>       <fieldset style="border: 1px solid #efefef; padding:.35em .625em .75em; margin-bottom: 15px;">
#>         <legend style="border:none; margin: 0; padding: 0 10px; font-size: 14px; color: ${{label_color}}">Group - ...junk...</legend>
#>         <div>
#>           <div class="form-group shiny-input-container">
#>             <label class="control-label" id="input_id_text_...junk...-label" for="input_id_text_...junk...">Text Label</label>
#>             <input id="input_id_text_...junk..." type="text" class="shiny-input-text form-control" value="" data-update-on="change"/>
#>           </div>
#>           <div class="form-group shiny-input-container">
#>             <label class="control-label" id="input_id_sli_...junk...-label" for="input_id_sli_...junk...">Slider Selector</label>
#>             <input class="js-range-slider" id="input_id_sli_...junk..." data-skin="shiny" data-min="1" data-max="1" data-from="0" data-step="0.02" data-grid="true" data-grid-num="NaN" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
#>           </div>
#>         </div>
#>       </fieldset>
#>     </div>
#>   </div>
#> </div>

# Source - system.file('demo/example-compountInput2.R', package='dipsaus')

# demo('example-compountInput2', package='dipsaus')

library(shiny)
library(dipsaus)
ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      compoundInput2(
        'compound', 'Group Label', label_color = c(NA,1:9),
        components = div(
          textInput('txt', 'Text'),
          selectInput('sel', 'Select', choices = 1:10, multiple = TRUE),
          sliderInput('sli', 'Slider', max=1, min=0, val=0.5)
        ),
        value = list(
          list(txt = '1'),  # Set text first group to be "1"
          '',                # no settings for second group
          list(sli = 0.2)    # sli = 0.2 for the third group
        )
      ),
      hr(),
      actionButton('action', 'Update compound input')
    )
  )
)

server <- function(input, output, session) {
  observe({
    print(input$compound)
  })
  observe({
    # Getting specific input at group 1
    print(input$compound_txt_1)
  })
  observeEvent(input$action, {
    updateCompoundInput2(
      session, 'compound',
      # Update values for each components
      value = lapply(1:5, function(ii){
        list(
          txt = sample(LETTERS, 1),
          sel = sample(1:10, 3),
          sli = runif(1)
        )
      }), ncomp = NULL, txt = list(label = as.character(Sys.time())))
  })
}

if( interactive() ){
  shinyApp(ui, server, options = list(launch.browser = TRUE))
}
```
