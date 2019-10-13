library(shiny)
library(dipsaus)
local({
  ui <- fluidPage(
    fluidRow(
      column(
        width = 4,
        compoundInput2(
          'compound', 'Group Label', label_color = 1:10,
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

  shinyApp(ui, server, options = list(launch.browser = TRUE))
})