## Demo for fancyDirectoryInput - Directory Drag & Drop
## This example shows how to use fancyDirectoryInput to upload entire directories

library(shiny)
library(dipsaus)

ui <- fluidPage(
  titlePanel("fancyDirectoryInput Demo"),

  sidebarLayout(
    sidebarPanel(
      h3("Upload a Directory"),
      p("Drag and drop an entire directory, or click the button to browse."),
      p(strong("Note:"), "Hidden files (starting with '.') are filtered out."),

      fancyDirectoryInput(
        'directory_upload',
        "Select Directory",
        size = "l"
      ),

      hr(),

      h4("Browser Compatibility:"),
      tags$ul(
        tags$li("Chrome/Edge: Full support"),
        tags$li("Safari: Full support"),
        tags$li("Firefox: Desktop only"),
        tags$li("IE: Not supported")
      )
    ),

    mainPanel(
      h3("Upload Results"),

      conditionalPanel(
        condition = "!output.has_upload",
        div(
          style = "padding: 40px; text-align: center; color: #999;",
          icon("folder-open", "fa-3x"),
          h4("No directory uploaded yet"),
          p("Select a directory using the input on the left")
        )
      ),

      conditionalPanel(
        condition = "output.has_upload",
        tabsetPanel(
          tabPanel(
            "File List",
            br(),
            uiOutput("file_count"),
            hr(),
            shiny::tableOutput("file_table")
          ),

          tabPanel(
            "Directory Structure",
            br(),
            verbatimTextOutput("directory_structure")
          ),

          tabPanel(
            "File Details",
            br(),
            verbatimTextOutput("raw_data")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive value to store directory data
  directory_data <- reactiveVal(NULL)

  # Update directory data when input changes
  observeEvent(input$directory_upload, {
    req(input$directory_upload)

    # Convert to data frame format
    upload_data <- input$directory_upload
    print(upload_data)

    # Create data frame from the uploaded data
    df <- data.frame(
      name = upload_data$name,
      size = upload_data$size,
      type = upload_data$type,
      datapath = upload_data$datapath,
      relativePath = upload_data$relativePath,
      stringsAsFactors = FALSE
    )

    # Attach directory structure as attribute
    attr(df, "directoryStructure") <- upload_data$directoryStructure

    directory_data(df)

    # Debug output
    cat("\n=== Directory Upload Debug ===\n")
    cat("Number of files:", nrow(df), "\n")
    cat("Columns:", paste(names(df), collapse=", "), "\n")
    cat("Sample paths:\n")
    print(head(df$relativePath, 5))
    cat("=============================\n\n")
  })

  # Check if upload exists
  output$has_upload <- reactive({
    !is.null(directory_data())
  })
  outputOptions(output, "has_upload", suspendWhenHidden = FALSE)

  # File count
  output$file_count <- renderUI({
    data <- directory_data()
    req(data)

    total_size <- sum(data$size, na.rm = TRUE)
    size_mb <- total_size / (1024^2)

    div(
      class = "alert alert-info",
      h4(icon("info-circle"), " Upload Summary"),
      p(strong("Total Files:"), nrow(data)),
      p(strong("Total Size:"), sprintf("%.2f MB", size_mb)),
      p(strong("Root Directory:"), basename(dirname(data$relativePath[1])))
    )
  })

  # File table
  output$file_table <- shiny::renderTable({
    data <- directory_data()
    req(data)

    # Format size column
    data$size_formatted <- sapply(data$size, function(s) {
      if(is.na(s)) return("N/A")
      if(s < 1024) {
        sprintf("%d B", s)
      } else if(s < 1024^2) {
        sprintf("%.2f KB", s / 1024)
      } else {
        sprintf("%.2f MB", s / (1024^2))
      }
    })

    # Select and rename columns for display
    display_data <- data[, c("name", "relativePath", "size_formatted", "type", "datapath")]
    colnames(display_data) <- c("File Name", "Relative Path", "Size", "MIME Type", "Server Path")

    display_data
  })

  # Directory structure
  output$directory_structure <- renderPrint({
    data <- directory_data()
    req(data)

    dir_struct <- attr(data, "directoryStructure")

    if(!is.null(dir_struct)) {
      cat("Directory Structure:\n\n")
      str(dir_struct, max.level = 4)
    } else {
      cat("No directory structure information available.\n")
      cat("The structure is built on the client side.\n")
    }
  })

  # Raw data
  output$raw_data <- renderPrint({
    data <- directory_data()
    req(data)

    cat("Raw Data Frame:\n\n")
    print(head(data, 20))

    if(nrow(data) > 20) {
      cat(sprintf("\n... and %d more files\n", nrow(data) - 20))
    }
  })

  observeEvent(input$directory_upload, {
    files <- input$directory_upload

    # Iterate through each file
    for(i in 1:length(files$name)) {
      file_path <- files$datapath[i]
      rel_path <- files$relativePath[i]

      # Skip files that are too large or couldn't be uploaded
      if(is.na(file_path) || !file.exists(file_path)) {
        cat("Skipping (not available):", rel_path, "\n")
        next
      }

      # Read the file content
      tryCatch({
        content <- readLines(file_path, warn = FALSE)
        cat("Processing:", rel_path, "-", length(content), "lines\n")
        # Do something with the content...
      }, error = function(e) {
        cat("Error reading:", rel_path, "-", e$message, "\n")
      })
    }
  })
}

# Run the app
if(interactive()) {
  shinyApp(ui, server)
}
