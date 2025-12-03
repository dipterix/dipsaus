## Demo for fancyDirectoryInput - Directory Drag & Drop
## This example shows how to use fancyDirectoryInput to upload entire directories
## with incremental file processing and status tracking

library(shiny)
library(dipsaus)

# Helper operator for default values
`%||%` <- function(x, y) {
  if(is.null(x)) y else x
}

# Print the current shiny.maxRequestSize setting
max_size <- getOption("shiny.maxRequestSize", 5*1024^2)
cat("Current shiny.maxRequestSize:", max_size, "bytes (", 
    round(max_size / 1024^2, 2), "MB )\n")

ui <- fluidPage(
  titlePanel("fancyDirectoryInput Demo"),

  sidebarLayout(
    sidebarPanel(
      h3("Upload a Directory"),
      p("Drag and drop an entire directory, or click the button to browse."),
      p(strong("Note:"), "Hidden files (starting with '.') are filtered out."),
      
      p(strong("New in this version:"), "Files are processed incrementally. 
        You can start working with files as they arrive, rather than waiting 
        for all files to upload."),

      fancyDirectoryInput(
        'directory_upload',
        "Select Directory",
        size = "l",
        progress = "Loading files"
      ),

      hr(),
      
      h4("How it works:"),
      tags$ul(
        tags$li(strong("Immediate:"), "Directory metadata and structure"),
        tags$li(strong("Incremental:"), "Files processed one-by-one"),
        tags$li(strong("Tracking:"), "Real-time status updates"),
        tags$li(strong("Large files:"), "Automatically chunked (5MB)")
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
        # Main file list with upload status
        uiOutput("file_count"),
        hr(),
        h4("Uploaded Files"),
        p("This table shows all files with their upload status and server paths."),
        shiny::tableOutput("file_table"),
        
        hr(),
        
        # Optional: Advanced details in tabs
        h4("Additional Information"),
        tabsetPanel(
          tabPanel(
            "Directory Structure",
            br(),
            verbatimTextOutput("directory_structure")
          ),
          
          tabPanel(
            "Detailed Status",
            br(),
            p("Advanced: Real-time processing status for each file"),
            uiOutput("status_summary"),
            hr(),
            shiny::tableOutput("status_table")
          ),

          tabPanel(
            "Raw Metadata",
            br(),
            verbatimTextOutput("raw_data")
          ),
          
          tabPanel(
            "Processing Log",
            br(),
            verbatimTextOutput("processed_files_log")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Enable progress tracking
  # cat("[DEBUG demo] Setting up progress observers for: directory_upload\n")
  observeDirectoryProgress("directory_upload")

  # Reactive values for status tracking (optional for advanced view)
  file_status <- reactiveVal(NULL)
  processed_log <- reactiveVal(character(0))
  
  # Note: input$directory_upload automatically updates with datapaths
  # when all files complete - no manual merging needed!
  
  # Track file processing status (for advanced details tab)
  observeEvent(input$directory_upload__status, {
    req(input$directory_upload__status)
    file_status(input$directory_upload__status)
  })
  
  # Log individual files as they arrive (for processing log)
  observeEvent(input$directory_upload__file, {
    req(input$directory_upload__file)
    file_data <- input$directory_upload__file
    
    # Create log entry
    timestamp <- format(Sys.time(), "%H:%M:%S")
    has_error <- !is.null(file_data$error) && !identical(file_data$error, "")
    has_datapath <- !is.null(file_data$datapath) && !is.na(file_data$datapath)
    
    log_status <- if(has_error) {
      paste("ERROR:", file_data$error)
    } else if(has_datapath) {
      "SUCCESS"
    } else {
      "SKIPPED"
    }
    
    log_entry <- sprintf(
      "[%s] File: %s (%.2f KB) - %s",
      timestamp,
      file_data$name %||% "unknown",
      (file_data$size %||% 0) / 1024,
      log_status
    )
    
    current_log <- processed_log()
    processed_log(c(current_log, log_entry))
  })

  # Check if upload exists
  output$has_upload <- reactive({
    !is.null(input$directory_upload)
  })
  outputOptions(output, "has_upload", suspendWhenHidden = FALSE)

  # File count
  output$file_count <- renderUI({
    data <- input$directory_upload
    req(data)

    total_size <- sum(data$size, na.rm = TRUE)
    size_mb <- total_size / (1024^2)
    
    ready <- attr(data, "ready", exact = TRUE)
    total_files <- attr(data, "totalFiles", exact = TRUE)
    
    # Get upload status
    upload_status <- attr(data, "upload_status", exact = TRUE) %||% "initialized"
    
    ready_badge <- if(upload_status == "completed") {
      tags$span(class = "label label-success", "Completed")
    } else if(upload_status == "errored") {
      tags$span(class = "label label-danger", "Completed with errors")
    } else if(isTRUE(ready)) {
      tags$span(class = "label label-success", "All files processed")
    } else {
      tags$span(class = "label label-warning", "Processing...")
    }

    # Get root directory from first file's relative path
    first_path <- data$relativePath[1]
    root_dir <- if(!is.null(first_path) && nchar(first_path) > 0) {
      # Split by / and take the first component
      parts <- strsplit(first_path, "/")[[1]]
      if(length(parts) > 0) parts[1] else "Unknown"
    } else {
      "Unknown"
    }
    
    div(
      class = "alert alert-info",
      h4(icon("info-circle"), " Upload Summary ", ready_badge),
      p(strong("Total Files:"), total_files %||% nrow(data)),
      p(strong("Total Size:"), sprintf("%.2f MB", size_mb)),
      p(strong("Root Directory:"), root_dir)
    )
  })
  
  # Status summary
  output$status_summary <- renderUI({
    status <- file_status()
    req(status)
    
    # Validate it's a data frame with status column
    if(!is.data.frame(status) || !"status" %in% names(status)) {
      return(div(class = "alert alert-warning", "Status data not yet available"))
    }
    
    status_counts <- table(status$status)
    
    # Helper to safely extract counts
    get_count <- function(name) {
      count <- status_counts[name]
      if(is.na(count)) return(0)
      as.numeric(count)
    }
    
    complete <- get_count("complete")
    processing <- get_count("processing")
    pending <- get_count("pending")
    error <- get_count("error")
    skipped <- get_count("skipped")
    
    total <- nrow(status)
    progress_pct <- if(total > 0) round((complete / total) * 100) else 0
    
    div(
      class = "alert alert-info",
      h4(icon("tasks"), " Processing Status"),
      div(
        class = "progress",
        div(
          class = "progress-bar progress-bar-success",
          role = "progressbar",
          style = sprintf("width: %d%%", progress_pct),
          sprintf("%d%%", progress_pct)
        )
      ),
      p(
        tags$span(class = "label label-success", paste("Complete:", complete)),
        " ",
        tags$span(class = "label label-info", paste("Processing:", processing)),
        " ",
        tags$span(class = "label label-default", paste("Pending:", pending)),
        " ",
        if(error > 0) tags$span(class = "label label-danger", paste("Error:", error)) else NULL,
        " ",
        if(skipped > 0) tags$span(class = "label label-warning", paste("Skipped:", skipped)) else NULL
      )
    )
  })
  
  # Status table
  output$status_table <- shiny::renderTable({
    status <- file_status()
    req(status)
    
    # Validate it's a data frame
    if(!is.data.frame(status)) {
      return(data.frame(Message = "Status data not available"))
    }
    
    # Format for display
    display_status <- status
    if("progress" %in% names(status)) {
      display_status$progress <- sprintf("%.0f%%", status$progress)
    }
    if("error" %in% names(status)) {
      display_status$error <- ifelse(is.na(status$error), "", status$error)
    }
    
    colnames(display_status) <- c("File ID", "Name", "Relative Path", "Status", "Progress", "Error")
    display_status
  })

  # File table
  output$file_table <- shiny::renderTable({
    data <- input$directory_upload
    req(data)
    
    # Get status information for display
    status <- file_status()
    
    # Create display data
    display_data <- data.frame(
      name = data$name,
      relativePath = data$relativePath,
      size = data$size,
      type = data$type,
      fileId = data$fileId,
      datapath = data$datapath,  # Automatically populated by input handler
      stringsAsFactors = FALSE
    )
    
    # Format size column
    display_data$size_formatted <- sapply(display_data$size, function(s) {
      if(is.na(s)) return("N/A")
      if(s < 1024) {
        sprintf("%d B", s)
      } else if(s < 1024^2) {
        sprintf("%.2f KB", s / 1024)
      } else {
        sprintf("%.2f MB", s / (1024^2))
      }
    })
    
    # Add status indicator from file_status (this updates in real-time)
    display_data$status_icon <- sapply(display_data$fileId, function(fid) {
      if(!is.null(status) && is.data.frame(status) && fid %in% status$fileId) {
        file_status <- status$status[status$fileId == fid][1]
        if(file_status == "complete") return("\u2713 Complete")
        if(file_status == "error") return("\u2717 Error")
        if(file_status == "skipped") return("\u2298 Skipped")
        if(file_status == "processing") return("\u27f3 Processing")
        return("\u23f3 Pending")
      }
      # Fallback to checking datapath
      dp <- display_data$datapath[display_data$fileId == fid][1]
      if(!is.na(dp)) return("\u2713 Complete")
      else return("\u23f3 Pending")
    })

    # Select and rename columns for display
    display_data <- display_data[, c("name", "relativePath", "size_formatted", "type", "status_icon", "datapath")]
    colnames(display_data) <- c("File Name", "Relative Path", "Size", "MIME Type", "Status", "Server Path")

    display_data
  })

  # Directory structure
  output$directory_structure <- renderPrint({
    data <- input$directory_upload
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
    data <- input$directory_upload
    req(data)

    cat("Initial Metadata Data Frame:\n\n")
    print(head(data, 20))

    if(nrow(data) > 20) {
      cat(sprintf("\n... and %d more files\n", nrow(data) - 20))
    }
    
    cat("\n\nAttributes:\n")
    cat("  - ready:", attr(data, "ready", exact = TRUE), "\n")
    cat("  - totalFiles:", attr(data, "totalFiles", exact = TRUE), "\n")
    cat("  - upload_status:", attr(data, "upload_status", exact = TRUE) %||% "not set", "\n")
    cat("  - directoryStructure: ", 
        if(!is.null(attr(data, "directoryStructure"))) "present" else "NULL", 
        "\n")
  })
  
  # Processed files log
  output$processed_files_log <- renderPrint({
    log_entries <- processed_log()
    req(length(log_entries) > 0)
    
    cat("=== Processed Files Log ===\n\n")
    cat(paste(log_entries, collapse = "\n"))
    cat("\n\n")
    cat(sprintf("Total processed: %d files\n", length(log_entries)))
  })
}

# Run the app
if(interactive()) {
  shinyApp(ui, server)
}
