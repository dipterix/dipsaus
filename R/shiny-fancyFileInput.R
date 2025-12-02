#' @title Shiny drag-and-drop file input
#' @description
#' Fancy drag and drop file upload for \code{shiny} apps.
#' @param inputId the input slot that will be used to access the value
#' @param label display label for the control, or NULL for no label.
#' @param width the width of the input
#' @param after_content tiny content that is to be displayed below the input box
#' @param size height of the widget, choices are \code{'s'}, \code{'m'}, \code{'l'}, and \code{'xl'}
#' @param ... passed to \code{\link[shiny]{fileInput}}
#' @returns See \code{\link[shiny]{fileInput}}
#' @examples
#'
#'
#' library(shiny)
#' library(dipsaus)
#'
#' ui <- basicPage(
#'   fancyFileInput('file_input', "Please upload")
#' )
#'
#' if(interactive()) {
#'   shinyApp(
#'     ui, server = function(input, output, session){},
#'     options = list(launch.browser = TRUE)
#'   )
#' }
#'
#' @export
fancyFileInput <- function( inputId, label, width = NULL,
                            after_content = "Drag & drop, or button",
                            size = c("s", "m", "l", "xl"),
                            ... ) {

  if(missing(label)) {
    label <- NULL
  }
  size <- match.arg(size)

  htmltools <- asNamespace("htmltools")
  max_size <- dipsaus::to_ram_size(getOption("shiny.maxRequestSize", 5*1024^2), 1024)

  shiny::div(
    class = c("dipsaus-fancy-file-input", sprintf("dipsaus-fancy-file-input-%s", size)),
    `dipsaus-after-content` = sprintf('%s (max: %.1f %s)',
                                      after_content,
                                      max_size, attr(max_size, "unit")),
    style = htmltools$css(
      width = htmltools$validateCssUnit(width),
    ),
    use_shiny_dipsaus(),
    shiny::fileInput(inputId = inputId, label = label, width = "100%", ...)
  )

}


#' @title Shiny drag-and-drop directory input
#' @description
#' Fancy drag and drop directory upload for \code{shiny} apps. This function
#' allows users to drag and drop entire directories. Note: This feature requires
#' browser support for the \code{webkitdirectory} attribute (Chrome, Edge, Safari).
#' Firefox has limited support.
#' @param inputId the input slot that will be used to access the value
#' @param label display label for the control, or NULL for no label.
#' @param width the width of the input
#' @param after_content tiny content that is to be displayed below the input box
#' @param size height of the widget, choices are \code{'s'}, \code{'m'}, \code{'l'}, and \code{'xl'}
#' @param maxSize maximum file size per file in bytes (default uses \code{shiny.maxRequestSize} option, typically 5MB)
#' @param progress logical or character; if \code{TRUE}, displays upload progress using \code{\link{progress2}};
#'   if a character string, uses it as the progress title; if \code{FALSE} (default), no progress is shown
#' @param ... additional arguments (currently unused)
#' @returns A reactive data frame with components: \code{fileId} (unique file identifier),
#' \code{name} (file name), \code{size} (file size in bytes), \code{type} (MIME type), 
#' \code{datapath} (temporary file path on server), and \code{relativePath} 
#' (full relative path including subdirectories). The data frame also has attributes:
#' \code{directoryStructure} (nested list representing the directory tree),
#' \code{ready} (logical indicating if all files are processed),
#' \code{totalFiles} (total number of files), and \code{upload_status} (one of 
#' "initialized", "completed", or "errored").
#' 
#' \strong{Important:} The \code{datapath} column is \code{NA} initially when 
#' \code{upload_status = "initialized"}. The input automatically updates when all files 
#' complete (\code{upload_status = "completed"}), and \code{datapath} values are 
#' populated with server file paths. Check \code{attr(input$<inputId>, "upload_status")} 
#' to determine when files are ready.
#' 
#' Optional real-time tracking: Individual file data is available via 
#' \code{input$<inputId>__file} as files upload. File processing status can be 
#' tracked via \code{input$<inputId>__status}, which returns a data frame with columns: 
#' \code{fileId}, \code{name}, \code{relativePath}, \code{status} 
#' (pending/processing/complete/error/skipped), \code{progress} (0-100), and 
#' \code{error} (error message if any).
#' @details
#' The directory input uses the \code{webkitdirectory} HTML attribute which is
#' not part of the HTML5 standard but is widely supported. Browser compatibility:
#' \itemize{
#'   \item Chrome/Edge: Full support
#'   \item Safari: Full support
#'   \item Firefox: Partial support (desktop only, no mobile)
#'   \item Internet Explorer: Not supported
#' }
#'
#' Hidden files (starting with '.') are filtered out by default on the client side.
#'
#' Files are transferred as base64-encoded data, so they use approximately 33\% more
#' bandwidth than their actual size. Files exceeding \code{maxSize} will be skipped
#' with a warning in the browser console.
#'
#' @examples
#'
#'
#' library(shiny)
#' library(dipsaus)
#'
#' ui <- basicPage(
#'   fancyDirectoryInput('dir_input', "Please upload a directory")
#' )
#'
#' # Example with progress tracking
#' ui2 <- basicPage(
#'   fancyDirectoryInput('dir_input2', "Upload with progress", progress = TRUE)
#' )
#' 
#' if(interactive()) {
#'   # Basic example
#'   shinyApp(
#'     ui,
#'     server = function(input, output, session){
#'       # Observe directory upload - updates automatically when complete
#'       observeEvent(input$dir_input, {
#'         files <- input$dir_input
#'         if(!is.null(files)) {
#'           upload_status <- attr(files, "upload_status")
#'           
#'           cat("Directory upload event:\n")
#'           cat("  Status:", upload_status, "\n")
#'           cat("  Total files:", attr(files, "totalFiles"), "\n")
#'           cat("  Ready:", attr(files, "ready"), "\n")
#'           
#'           if(upload_status == "completed") {
#'             # All files uploaded - datapaths are now populated!
#'             cat("\nAll files uploaded successfully!\n")
#'             cat("Files with datapaths:\n")
#'             print(files[!is.na(files$datapath), 
#'                         c("name", "relativePath", "datapath")])
#'             
#'             # Now you can process all files
#'             for(i in seq_len(nrow(files))) {
#'               if(!is.na(files$datapath[i])) {
#'                 cat("\nProcessing:", files$name[i], "\n")
#'                 cat("  Server path:", files$datapath[i], "\n")
#'                 cat("  File size:", file.size(files$datapath[i]), "bytes\n")
#'               }
#'             }
#'           } else if(upload_status == "initialized") {
#'             # Initial metadata - datapaths are NA at this point
#'             cat("Upload started, processing files...\n")
#'             
#'             # Access directory structure
#'             dir_struct <- attr(files, "directoryStructure")
#'             cat("\nDirectory structure:\n")
#'             print(str(dir_struct, max.level = 2))
#'           }
#'         }
#'       })
#'       
#'       # Optional: Track individual files as they upload (real-time)
#'       observeEvent(input$dir_input__file, {
#'         file_data <- input$dir_input__file
#'         if(!is.null(file_data) && !is.na(file_data$datapath)) {
#'           cat("File uploaded:", file_data$name, "->", file_data$datapath, "\n")
#'         }
#'       })
#'       
#'       # Optional: Monitor upload progress
#'       observeEvent(input$dir_input__status, {
#'         status <- input$dir_input__status
#'         if(!is.null(status) && is.data.frame(status)) {
#'           completed <- sum(status$status == "complete")
#'           total <- nrow(status)
#'           cat("Progress:", completed, "/", total, "files\n")
#'         }
#'       })
#'     },
#'     options = list(launch.browser = TRUE)
#'   )
#'   
#'   # Example with progress tracking
#'   shinyApp(
#'     ui2,
#'     server = function(input, output, session){
#'       # Enable progress tracking
#'       observeDirectoryProgress("dir_input2")
#'       
#'       # Process files when complete
#'       observeEvent(input$dir_input2, {
#'         files <- input$dir_input2
#'         if(!is.null(files) && attr(files, "upload_status") == "completed") {
#'           cat("Received", nrow(files), "files\n")
#'         }
#'       })
#'     },
#'     options = list(launch.browser = TRUE)
#'   )
#' }
#'
#' @export
fancyDirectoryInput <- function( inputId, label, width = NULL,
                                 after_content = "Drag & drop directory, or button",
                                 size = c("s", "m", "l", "xl"),
                                 maxSize = NULL,
                                 progress = FALSE,
                                 ... ) {

  if(missing(label)) {
    label <- NULL
  }
  size <- match.arg(size)

  htmltools <- asNamespace("htmltools")

  # Use provided maxSize or fall back to shiny option
  if(is.null(maxSize)) {
    maxSize <- getOption("shiny.maxRequestSize", 5*1024^2)
  }
  max_size <- dipsaus::to_ram_size(maxSize, 1024)

  # Create the HTML structure manually to avoid Shiny's file input binding
  input_id_directory <- paste0(inputId, "_directory")

  # Handle progress parameter
  progress_enabled <- FALSE
  progress_title <- "Uploading directory"
  if(is.logical(progress) && isTRUE(progress)) {
    progress_enabled <- TRUE
  } else if(is.character(progress) && length(progress) == 1) {
    progress_enabled <- TRUE
    progress_title <- progress
  }
  
  shiny::div(
    class = c("dipsaus-fancy-directory-input", sprintf("dipsaus-fancy-directory-input-%s", size)),
    id = inputId,
    `data-max-file-size` = maxSize,  # Pass max size to JavaScript in bytes
    `data-progress-enabled` = tolower(as.character(progress_enabled)),
    `data-progress-title` = progress_title,
    `dipsaus-after-content` = sprintf('%s (max per file: %.1f %s)',
                                      after_content,
                                      max_size, attr(max_size, "unit")),
    style = htmltools$css(
      width = htmltools$validateCssUnit(width),
    ),
    use_shiny_dipsaus(),

    # Manual HTML structure for file input
    shiny::div(
      class = "shiny-input-container",
      style = "width: 100%;",

      if(!is.null(label)) {
        shiny::tags$label(
          class = "control-label",
          `for` = input_id_directory,
          label
        )
      },

      shiny::div(
        class = "input-group",

        shiny::tags$label(
          class = "input-group-btn",
          shiny::tags$span(
            class = "btn btn-default btn-file",
            "Browse...",
            shiny::tags$input(
              id = input_id_directory,
              name = input_id_directory,
              type = "file",
              class = "dipsaus-directory-file-input",
              style = "display: none;",
              webkitdirectory = NA,
              directory = NA,
              multiple = NA
            )
          )
        ),

        shiny::tags$input(
          type = "text",
          class = "form-control",
          placeholder = "No directory selected",
          readonly = "readonly"
        )
      ),

      shiny::div(
        class = "progress progress-striped active shiny-file-input-progress",
        style = "display: none;",
        shiny::div(class = "progress-bar")
      )
    ),
    
    # Hidden inputs for file data and status tracking
    shiny::tags$input(
      id = paste0(inputId, "__file"),
      class = "dipsaus-directory-file-data",
      type = "hidden",
      `data-input-binding` = "dipsaus.directoryInput.file"
    ),
    shiny::tags$input(
      id = paste0(inputId, "__status"),
      class = "dipsaus-directory-file-status",
      type = "hidden",
      `data-input-binding` = "dipsaus.directoryInput.status"
    )
  )

}


# Internal function to register directory input handlers
# Called from .onLoad in zzz.R
registerDirectoryInputHandlers <- function() {
  
  # Internal operator for default values
  `%||%` <- function(x, y) {
    if(is.null(x)) y else x
  }
  
  # Register input handler for directory inputs (initial metadata)
  shiny::registerInputHandler("dipsaus.directoryInput", function(data, shinysession, name) {
    if(is.null(data)) {
      return(NULL)
    }
    
    # Get or create session-specific storage
    session_token <- shinysession$token
    state_key <- paste0(session_token, "_", name)
    
    # cat("[Main Handler] Called for input:", name, "\n")
    # cat("  Has 'value' property:", !is.null(data$value), "\n")
    # cat("  Has 'fileMetadata' property:", !is.null(data$fileMetadata), "\n")
    # cat("  upload_status:", data$upload_status %||% "not set", "\n")
    
    # Check if this is a state retrieval call (value property present)
    if(!is.null(data$value) && is.data.frame(data$value)) {
      # cat("[Main Handler] Returning updated state from storage\n")
      # Return the current state from storage
      if(exists(state_key, envir = .directory_upload_state)) {
        state <- .directory_upload_state[[state_key]]
        # cat("  Datapaths in state:", sum(!is.na(state$df$datapath)), "/", nrow(state$df), "\n")
        return(state$df)
      }
      # cat("  State not found, returning data$value\n")
      return(data$value)
    }
    
    if(!is.null(data$fileMetadata) && length(data$fileMetadata) > 0) {
      # Check if this is a completion update (upload_status = completed)
      if((data$upload_status %||% "initialized") %in% c("completed", "errored")) {
        # cat("[Main Handler] Upload complete! Returning state with datapaths\n")
        # Return the updated state with datapaths
        if(exists(state_key, envir = .directory_upload_state)) {
          state <- .directory_upload_state[[state_key]]
          # cat("  Datapaths in state:", sum(!is.na(state$df$datapath)), "/", nrow(state$df), "\n")
          
          # Copy attributes from new data
          attr(state$df, "directoryStructure") <- data$directoryStructure
          attr(state$df, "ready") <- data$ready
          attr(state$df, "totalFiles") <- data$totalFiles
          attr(state$df, "upload_status") <- data$upload_status
          
          return(state$df)
        }
      }
      
      # cat("[Main Handler] Initializing new directory upload\n")
      # New incremental format - initialize metadata
      df <- data.frame(
        fileId = sapply(data$fileMetadata, function(x) x$fileId),
        name = sapply(data$fileMetadata, function(x) x$name),
        size = sapply(data$fileMetadata, function(x) x$size),
        type = sapply(data$fileMetadata, function(x) x$type),
        relativePath = sapply(data$fileMetadata, function(x) x$relativePath),
        datapath = NA_character_,  # Will be filled as files arrive
        stringsAsFactors = FALSE
      )
      
      # Attach directory structure and metadata
      attr(df, "directoryStructure") <- data$directoryStructure
      attr(df, "ready") <- data$ready
      attr(df, "totalFiles") <- data$totalFiles
      attr(df, "upload_status") <- data$upload_status %||% "initialized"
      
      # Store initial state
      .directory_upload_state[[state_key]] <- list(
        df = df,
        total_files = data$totalFiles,
        completed_files = 0
      )
      
      # cat("  State key:", state_key, "\n")
      # cat("  Total files:", data$totalFiles, "\n")
      
      return(df)
    } else {
      # Legacy format for backward compatibility
      datapaths <- character(length(data$name))
      
      for(i in seq_along(data$name)) {
        tryCatch({
          if(!is.null(data$base64data[[i]]) && length(data$base64data[[i]]) > 0) {
            # Handle both chunked and non-chunked data
            base64_data <- data$base64data[[i]]
            
            if(is.list(base64_data) || length(base64_data) > 1) {
              # Chunked data - reconstruct from chunks
              binary_data <- raw(0)
              for(chunk in base64_data) {
                chunk_string <- chunk
                if(grepl("^data:", chunk_string)) {
                  chunk_string <- sub("^data:[^,]*,", "", chunk_string)
                }
                binary_data <- c(binary_data, base64enc::base64decode(chunk_string))
              }
            } else {
              # Single base64 string
              base64_string <- base64_data
              if(grepl("^data:", base64_string)) {
                base64_string <- sub("^data:[^,]*,", "", base64_string)
              }
              binary_data <- base64enc::base64decode(base64_string)
            }
            
            # Create temp file with original filename
            temp_file <- tempfile(pattern = "upload_", fileext = paste0("_", basename(data$name[[i]])))
            writeBin(binary_data, temp_file)
            datapaths[i] <- temp_file
          } else {
            datapaths[i] <- NA_character_
          }
        }, error = function(e) {
          warning("Failed to process file ", data$name[[i]], ": ", e$message)
          datapaths[i] <<- NA_character_
        })
      }
      
      df <- data.frame(
        name = unlist(data$name),
        size = unlist(data$size),
        type = unlist(data$type),
        datapath = datapaths,
        relativePath = unlist(data$relativePath),
        stringsAsFactors = FALSE
      )
      
      if(!is.null(data$directoryStructure)) {
        attr(df, "directoryStructure") <- data$directoryStructure
      }
      
      return(df)
    }
  }, force = TRUE)
  
  # Register input handler for individual file data (__file)
  shiny::registerInputHandler("dipsaus.directoryInput.file", function(data, shinysession, name) {
    if(is.null(data)) {
      return(NULL)
    }
    
    # Extract the base input name (remove __file suffix)
    base_name <- sub("__file$", "", name)
    session_token <- shinysession$token
    state_key <- paste0(session_token, "_", base_name)
    
    tryCatch({
      # Process individual file data
      datapath <- NA_character_
      error_msg <- NULL
      
      if(!is.null(data$base64data) && length(data$base64data) > 0) {
        base64_data <- data$base64data
        
        if(data$chunked %||% FALSE) {
          # Chunked data - reconstruct from chunks
          binary_data <- raw(0)
          for(chunk in base64_data) {
            chunk_string <- chunk
            if(grepl("^data:", chunk_string)) {
              chunk_string <- sub("^data:[^,]*,", "", chunk_string)
            }
            binary_data <- c(binary_data, base64enc::base64decode(chunk_string))
          }
        } else {
          # Single base64 string
          base64_string <- base64_data
          if(grepl("^data:", base64_string)) {
            base64_string <- sub("^data:[^,]*,", "", base64_string)
          }
          binary_data <- base64enc::base64decode(base64_string)
        }
        
        # Create temp file with original filename
        temp_file <- tempfile(pattern = "upload_", fileext = paste0("_", basename(data$name)))
        writeBin(binary_data, temp_file)
        datapath <- temp_file
      } else {
        # File was skipped or had error
        error_msg <- data$`_error` %||% if(data$`_tooLarge` %||% FALSE) "File too large" else "Unknown error"
      }
      
      # Update the stored state with this file's datapath
      if(exists(state_key, envir = .directory_upload_state)) {
        state <- .directory_upload_state[[state_key]]
        file_idx <- which(state$df$fileId == data$fileId)
        
        if(length(file_idx) > 0) {
          state$df$datapath[file_idx] <- datapath
          state$completed_files <- state$completed_files + 1
          
          # cat("[File Handler] Processed file", state$completed_files, "/", state$total_files, "\n")
          # cat("  File:", data$name, "\n")
          # cat("  Datapath:", datapath, "\n")
          
          # Update the state
          .directory_upload_state[[state_key]] <- state
          
          # If all files are complete, trigger an update to the main input
          if(state$completed_files >= state$total_files) {
            # cat("[File Handler] All files complete!\n")
            # cat("  Total datapaths populated:", sum(!is.na(state$df$datapath)), "/", nrow(state$df), "\n")
            # cat("  JavaScript will resend the data to trigger main handler\n")
          }
        } else {
          # cat("[File Handler] WARNING: fileId", data$fileId, "not found in state\n")
        }
      } else {
        # cat("[File Handler] WARNING: state_key", state_key, "not found\n")
      }
      
      return(list(
        fileId = data$fileId,
        name = data$name,
        size = data$size,
        type = data$type,
        relativePath = data$relativePath,
        datapath = datapath,
        error = error_msg
      ))
    }, error = function(e) {
      warning("Failed to process file ", data$name, ": ", e$message)
      return(list(
        fileId = data$fileId,
        name = data$name,
        size = data$size,
        type = data$type,
        relativePath = data$relativePath,
        datapath = NA_character_,
        error = e$message
      ))
    })
  }, force = TRUE)
  
  # Register input handler for file status table (__status)
  shiny::registerInputHandler("dipsaus.directoryInput.status", function(data, shinysession, name) {
    if(is.null(data) || length(data) == 0) {
      return(NULL)
    }
    
    # Check if data is already a data frame
    if(is.data.frame(data)) {
      return(data)
    }
    
    # Check if it's a list of lists (expected format)
    if(is.list(data) && length(data) > 0 && is.list(data[[1]])) {
      # Convert list to data frame
      df <- data.frame(
        fileId = sapply(data, function(x) x$fileId %||% NA_character_),
        name = sapply(data, function(x) x$name %||% NA_character_),
        relativePath = sapply(data, function(x) x$relativePath %||% NA_character_),
        status = sapply(data, function(x) x$status %||% NA_character_),
        progress = sapply(data, function(x) x$progress %||% 0),
        error = sapply(data, function(x) x$error %||% NA_character_),
        stringsAsFactors = FALSE
      )
      return(df)
    }
    
    # If it's a flat named vector (which seems to be happening), try to reconstruct
    if(is.vector(data) && !is.null(names(data))) {
      # Data came as a flattened structure - need to reconstruct
      # Get unique field names to determine pattern
      field_names <- unique(names(data))
      n_fields <- length(field_names)
      n_records <- length(data) / n_fields
      
      # Removed cat() calls for CRAN compliance
      # cat("Reconstructing from flat vector:\n")
      # cat("  Unique fields:", paste(field_names, collapse=", "), "\n")
      # cat("  Fields per record:", n_fields, "\n")
      # cat("  Number of records:", n_records, "\n")
      
      if(length(data) %% n_fields == 0 && n_records == floor(n_records)) {
        # Create a matrix and then data frame
        data_names <- names(data)
        
        # Initialize lists for each field
        field_lists <- list()
        for(field in field_names) {
          field_lists[[field]] <- data[data_names == field]
        }
        
        # Create data frame
        df <- as.data.frame(field_lists, stringsAsFactors = FALSE)
        
        # Convert progress to numeric if it exists
        if("progress" %in% names(df)) {
          df$progress <- as.numeric(df$progress)
        }
        
        # Handle error field - add if missing
        if(!"error" %in% names(df)) {
          df$error <- NA_character_
        } else {
          # Replace "null" strings with NA
          df$error[df$error == "null" | df$error == ""] <- NA_character_
        }
        
        # Removed cat() call for CRAN compliance
        # cat("  Successfully reconstructed data frame with", nrow(df), "rows\n\n")
        return(df)
      } else {
        # Removed cat() call for CRAN compliance
        # cat("  ERROR: Length mismatch -", length(data), "elements cannot be divided into", n_fields, "fields\n\n")
      }
    }
    
    # Fallback: return NULL if we can't parse it
    warning("Unable to parse status data structure")
    return(NULL)
  }, force = TRUE)
  
  invisible(TRUE)
}


#' @title Enable progress tracking for directory uploads
#' @description
#' Helper function to set up progress tracking using \code{\link{progress2}} 
#' for \code{\link{fancyDirectoryInput}}. Call this in your server function 
#' to enable automatic progress updates.
#' @param inputId the input ID of the \code{fancyDirectoryInput}
#' @param session the Shiny session object (default: \code{shiny::getDefaultReactiveDomain()})
#' @return invisible NULL; sets up reactive observers for progress tracking
#' @examples 
#' \dontrun{
#' server <- function(input, output, session) {
#'   # Enable progress tracking
#'   observeDirectoryProgress("dir_input")
#'   
#'   # Your other server logic...
#' }
#' }
#' @export
observeDirectoryProgress <- function(inputId, session = shiny::getDefaultReactiveDomain()) {
  
  if(is.null(session)) {
    stop("observeDirectoryProgress must be called from within a Shiny server function")
  }
  
  # Internal operator for default values
  `%||%` <- function(x, y) {
    if(is.null(x)) y else x
  }
  
  progress_id <- paste0(inputId, "_upload_progress")
  progress_obj <- NULL
  
  # Start progress
  shiny::observeEvent(session$input[[paste0(progress_id, "_start")]], {
    config <- session$input[[paste0(progress_id, "_start")]]
    cat("[DEBUG observeDirectoryProgress] Start triggered for:", progress_id, "_start\n")
    cat("[DEBUG observeDirectoryProgress] Config:", paste(names(config), collapse = ", "), "\n")
    if(!is.null(config)) {
      progress_obj <<- progress2(
        title = config$title %||% "Uploading directory",
        max = config$max %||% 100,
        quiet = config$quiet %||% FALSE,
        session = session
      )
      cat("[DEBUG observeDirectoryProgress] Progress object created with title:", config$title %||% "Uploading directory", "\n")
    }
  }, ignoreInit = TRUE)
  
  # Update progress
  shiny::observeEvent(session$input[[paste0(progress_id, "_update")]], {
    update_data <- session$input[[paste0(progress_id, "_update")]]
    cat("[DEBUG observeDirectoryProgress] Update triggered\n")
    if(!is.null(progress_obj) && !is.null(update_data)) {
      # Format filename - truncate if too long
      filename <- update_data$filename %||% ""
      if(nchar(filename) > 40) {
        filename <- paste0(substr(filename, 1, 37), "...")
      }
      
      # Format detail message
      detail_msg <- sprintf("Loaded %d files", update_data$current %||% 0)
      
      cat("[DEBUG observeDirectoryProgress] Incrementing progress:", detail_msg, "\n")
      progress_obj$inc(
        detail = detail_msg,
        amount = update_data$inc %||% 1
      )
    } else {
      cat("[DEBUG observeDirectoryProgress] Skipped - progress_obj is NULL:", is.null(progress_obj), "\n")
    }
  }, ignoreInit = TRUE)
  
  # Close progress
  shiny::observeEvent(session$input[[paste0(progress_id, "_close")]], {
    cat("[DEBUG observeDirectoryProgress] Close triggered\n")
    if(!is.null(progress_obj)) {
      cat("[DEBUG observeDirectoryProgress] Closing progress object\n")
      progress_obj$close()
      progress_obj <<- NULL
    } else {
      cat("[DEBUG observeDirectoryProgress] No progress object to close\n")
    }
  }, ignoreInit = TRUE)
  
  invisible(NULL)
}