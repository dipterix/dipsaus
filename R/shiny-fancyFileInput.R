#' @title Shiny drag-and-drop file input
#' @description
#' Fancy drag and drop file upload for \code{shiny} apps.
#' @param inputId the input slot that will be used to access the value
#' @param label display label for the control, or NULL for no label.
#' @param width the width of the input
#' @param after_content tiny content that is to be displayed below the input box
#' @param size height of the widget, choices are \code{'s'}, \code{'m'}, \code{'l'}, and \code{'xl'}
#' @param maxSize maximum file size per file in bytes (default uses \code{shiny.maxRequestSize} option, typically 5MB)
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
                            maxSize = NULL,
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
#' @param autoCleanup logical; if \code{TRUE}, removes all files from the upload directory
#'   before each new upload. Default is \code{FALSE}. This is useful to prevent stale files
#'   from previous uploads. Can be changed dynamically by updating the \code{data-auto-cleanup}
#'   HTML attribute on the input element.
#' @param autoCleanupLocked logical; if \code{TRUE}, hides the auto-cleanup checkbox,
#'   preventing users from changing the setting. Default is \code{FALSE}, which shows
#'   the check-box allowing users to toggle auto-cleanup behavior.
#' @param ... additional arguments (currently unused)
#' @returns A reactive data frame with components: \code{fileId} (unique file identifier),
#' \code{name} (file name), \code{size} (file size in bytes), \code{type} (MIME type),
#' \code{datapath} (temporary file path on server), and \code{relativePath}
#' (full relative path including sub-directories). The data frame also has attributes:
#' \code{directoryStructure} (nested list representing the directory tree),
#' \code{ready} (logical indicating if all files are processed),
#' \code{totalFiles} (total number of files), \code{upload_status} (one of
#' \code{"initialized"}, \code{"completed"}, or \code{"errored"}), and \code{upload_dir} (character string
#' with the upload directory path where files are stored, preserving their relative directory structure).
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
#' \strong{Upload Directory Management:}
#'
#' Uploaded files are stored in a session-specific directory with a deterministic path:
#' \code{tempdir()/dipsaus_uploads/{6-char-hash}/} where the hash is computed as
#' \code{substr(digest(session_token + full_inputId), 1, 6)}. This ensures:
#' \itemize{
#'   \item The same directory is used for all uploads to the same input within a session
#'   \item Files preserve their original relative directory structure within this directory
#'   \item Different sessions and inputs get isolated directories
#' }
#'
#' For example, uploading a directory with structure \code{project/src/utils/helper.R}
#' will create \code{tempdir()/dipsaus_uploads/a3f5c2/project/src/utils/helper.R}.
#'
#' The \code{autoCleanup} parameter controls whether the upload directory is cleaned
#' before each new upload. When \code{FALSE} (default), files accumulate across uploads.
#' When \code{TRUE}, the directory is removed and recreated before each upload, preventing
#' stale files from previous uploads. A checkbox is displayed below the input widget
#' allowing users to toggle the auto-cleanup behavior dynamically. Use
#' \code{\link{get_dipsaus_upload_dir}} to retrieve the upload directory path for
#' manual cleanup: \code{unlink(get_dipsaus_upload_dir(inputId), recursive = TRUE)}
#'
#' \strong{Related Functions:}
#' \itemize{
#'   \item \code{\link{observeDirectoryProgress}}: Enable progress tracking with
#'         \code{\link{progress2}} for directory uploads. Call this in your server
#'         function to display upload progress automatically.
#'   \item \code{\link{get_dipsaus_upload_dir}}: Retrieve the upload directory path
#'         for a given input ID. Useful for manual file cleanup or custom processing.
#' }
#'
#' @seealso \code{\link{observeDirectoryProgress}} for progress tracking,
#'   \code{\link{get_dipsaus_upload_dir}} for directory path retrieval,
#'   \code{\link{fancyFileInput}} for single file uploads,
#'   \code{\link{progress2}} for custom progress bars
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
#'
#'   # Example with autoCleanup and manual directory cleanup
#'   ui3 <- basicPage(
#'     fancyDirectoryInput('dir_input3', "Upload directory", autoCleanup = TRUE),
#'     actionButton('cleanup', 'Clean Up Files')
#'   )
#'
#'   shinyApp(
#'     ui3,
#'     server = function(input, output, session){
#'       observeEvent(input$dir_input3, {
#'         files <- input$dir_input3
#'         if(!is.null(files) && attr(files, "upload_status") == "completed") {
#'           # Get upload directory with preserved structure
#'           upload_dir <- attr(files, "upload_dir")
#'           cat("Files stored in:", upload_dir, "\\n")
#'           cat("Example file path:", files$datapath[1], "\\n")
#'           # Process files with their directory structure...
#'         }
#'       })
#'
#'       # Manual cleanup option
#'       observeEvent(input$cleanup, {
#'         upload_dir <- get_dipsaus_upload_dir('dir_input3')
#'         if(!is.null(upload_dir) && dir.exists(upload_dir)) {
#'           unlink(upload_dir, recursive = TRUE, force = TRUE)
#'           cat("Cleaned up:", upload_dir, "\\n")
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
                                 autoCleanup = FALSE,
                                 autoCleanupLocked = FALSE,
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
  progress_title <- "Loading files"
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
    `data-auto-cleanup` = tolower(as.character(autoCleanup)),
    `data-auto-cleanup-locked` = tolower(as.character(autoCleanupLocked)),
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
        ),

        shiny::div(
          class = "progress progress-striped active shiny-file-input-progress",
          style = "display: none;",
          shiny::div(class = "progress-bar")
        )
      ),

      # After-content text
      shiny::div(
        class = "dipsaus-directory-after-content",
        sprintf('%s (max per file: %.1f %s)',
                after_content,
                max_size, attr(max_size, "unit"))
      ),

      # Auto-cleanup checkbox option (only show if not locked)
      if(!autoCleanupLocked) {
        shiny::div(
          class = "dipsaus-directory-options",
          shiny::tags$label(
            style = "font-weight: normal; cursor: pointer; margin-bottom: 0;",
            shiny::tags$input(
              type = "checkbox",
              class = "dipsaus-auto-cleanup-checkbox",
              checked = if(autoCleanup) NA else NULL
            ),
            shiny::tags$span(
              style = "margin-left: 5px;",
              "Auto-cleanup upload directory"
            )
          )
        )
      }
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

  # Helper function to create deterministic upload directory
  get_session_upload_dir <- function(shinysession, inputId) {
    session_token <- shinysession$token
    # Create deterministic hash based on session token and input ID (no timestamp)
    dir_hash <- substr(digest::digest(paste0(session_token, inputId)), 1, 6)
    upload_dir <- file.path(tempdir(), "dipsaus_uploads", dir_hash)

    # Create directory if it doesn't exist
    if(!dir.exists(upload_dir)) {
      dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)
    }

    return(upload_dir)
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
        df <- state$df
        attr(df, "upload_dir") <- state$upload_dir
        return(df)
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
          attr(state$df, "upload_dir") <- state$upload_dir

          return(state$df)
        }
      }

      # cat("[Main Handler] Initializing new directory upload\n")

      # Get or create upload directory
      upload_dir <- get_session_upload_dir(shinysession, name)

      # Handle autoCleanup: remove and recreate directory if requested
      if(isTRUE(data$autoCleanup)) {
        if(dir.exists(upload_dir)) {
          unlink(upload_dir, recursive = TRUE, force = TRUE)
        }
        dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)
      }

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
      attr(df, "upload_dir") <- upload_dir

      # Store simplified state (only tracking upload_dir and file processing)
      .directory_upload_state[[state_key]] <- list(
        upload_dir = upload_dir,
        df = df,
        total_files = data$totalFiles,
        completed_files = 0
      )

      # cat("  State key:", state_key, "\n")
      # cat("  Total files:", data$totalFiles, "\n")
      # cat("  Upload dir:", upload_dir, "\n")

      return(df)
    } else {
      # Legacy format for backward compatibility

      # Get or create upload directory
      upload_dir <- get_session_upload_dir(shinysession, name)

      # Handle autoCleanup: remove and recreate directory if requested
      if(isTRUE(data$autoCleanup)) {
        if(dir.exists(upload_dir)) {
          unlink(upload_dir, recursive = TRUE, force = TRUE)
        }
        dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)
      }

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

            # Save file preserving directory structure
            file_path <- file.path(upload_dir, data$relativePath[[i]])

            # Create parent directory if needed
            parent_dir <- dirname(file_path)
            if(!dir.exists(parent_dir)) {
              dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
            }

            writeBin(binary_data, file_path)
            datapaths[i] <- file_path
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
      attr(df, "upload_dir") <- upload_dir

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

        # Get upload directory from state and save file preserving directory structure
        if(exists(state_key, envir = .directory_upload_state)) {
          state <- .directory_upload_state[[state_key]]
          upload_dir <- state$upload_dir

          # Create full file path preserving relative path
          file_path <- file.path(upload_dir, data$relativePath)

          # Create parent directory if needed
          parent_dir <- dirname(file_path)
          if(!dir.exists(parent_dir)) {
            dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
          }

          writeBin(binary_data, file_path)
          datapath <- file_path
        } else {
          # Fallback to tempfile if state not found
          temp_file <- tempfile(pattern = "upload_", fileext = paste0("_", basename(data$name)))
          writeBin(binary_data, temp_file)
          datapath <- temp_file
        }
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
  shiny::bindEvent(
    shiny::observe({
      tryCatch({
        config <- session$input[[paste0(progress_id, "_start")]]
        # cat("[DEBUG observeDirectoryProgress] Start triggered for:", progress_id, "_start\n")
        # cat("[DEBUG observeDirectoryProgress] Config:", paste(names(config), collapse = ", "), "\n")
        if(!is.null(config)) {
          progress_obj <<- progress2(
            title = config$title %||% "Loading files",
            max = config$max %||% 100,
            quiet = config$quiet %||% FALSE,
            session = session
          )
          # cat("[DEBUG observeDirectoryProgress] Progress object created with title:", config$title %||% "Loading files", "\n")
        }
      }, error = function(e) {
        warning("Error starting directory upload progress: ", e$message)
      })
    }),
    session$input[[paste0(progress_id, "_start")]],
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  # Update progress
  shiny::bindEvent(
    shiny::observe({
      tryCatch({
        update_data <- session$input[[paste0(progress_id, "_update")]]
        # cat("[DEBUG observeDirectoryProgress] Update triggered\n")
        if(!is.null(progress_obj) && !is.null(update_data)) {
          # Format filename - truncate if too long
          filename <- update_data$filename %||% ""
          if(nchar(filename) > 40) {
            filename <- paste0(substr(filename, 1, 37), "...")
          }

          # Format detail message
          detail_msg <- sprintf("Loaded %d files", update_data$current %||% 0)

          # cat("[DEBUG observeDirectoryProgress] Incrementing progress:", detail_msg, "\n")
          progress_obj$inc(
            detail = detail_msg,
            amount = update_data$inc %||% 1
          )
        } else {
          # cat("[DEBUG observeDirectoryProgress] Skipped - progress_obj is NULL:", is.null(progress_obj), "\n")
        }
      }, error = function(e) {
        warning("Error updating directory upload progress: ", e$message)
      })
    }),
    session$input[[paste0(progress_id, "_update")]],
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  # Close progress
  shiny::bindEvent(
    shiny::observe({
      tryCatch({
        # cat("[DEBUG observeDirectoryProgress] Close triggered\n")
        if(!is.null(progress_obj)) {
          # cat("[DEBUG observeDirectoryProgress] Closing progress object\n")
          progress_obj$close()
          progress_obj <<- NULL
        } else {
          # cat("[DEBUG observeDirectoryProgress] No progress object to close\n")
        }
      }, error = function(e) {
        warning("Error closing directory upload progress: ", e$message)
        # Ensure progress_obj is nullified even on error
        progress_obj <<- NULL
      })
    }),
    session$input[[paste0(progress_id, "_close")]],
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  invisible(NULL)
}


#' @title Get upload directory for \code{\link{fancyDirectoryInput}}
#' @description
#' Retrieve the upload directory path for a \code{\link{fancyDirectoryInput}}.
#' The upload directory is generated based on the session token
#' and input ID, and files uploaded to the input preserve their relative directory structure within this directory.
#' @param inputId the input ID (relative to the session, whose namespace
#' will be automatically applied via \code{session$ns(inputId)})
#' @param session the Shiny session object (default: \code{shiny::getDefaultReactiveDomain()})
#' @return Character string with the upload directory path, or \code{NULL} if
#' the input has no uploaded files yet. The directory path follows the pattern
#' \code{tempdir()/dipsaus_uploads/{6-char-hash}/} where the hash is derived
#' from \code{digest(session_token + full_inputId)}.
#' @details
#' This function is useful for:
#' \itemize{
#'   \item Manually cleaning up uploaded files via \code{unlink(get_dipsaus_upload_dir(inputId), recursive = TRUE)}
#'   \item Processing files with their preserved directory structure
#'   \item Accessing the upload directory for custom file operations
#' }
#'
#' The upload directory is deterministic for a given session and input ID, meaning
#' multiple uploads to the same input will use the same directory (unless
#' \code{autoCleanup = TRUE} is set, which clears the directory before each new upload).
#'
#' In Shiny modules, use the relative \code{inputId} (e.g., \code{"dir_input"})
#' rather than the full ID (with namespace applied). The function will automatically handle
#' namespace conversion via \code{session$ns(inputId)}.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   observeEvent(input$dir_input, {
#'     files <- input$dir_input
#'
#'     if(!is.null(files) && attr(files, "upload_status") == "completed") {
#'       # Get the upload directory
#'       upload_dir <- get_dipsaus_upload_dir("dir_input")
#'       cat("Files are stored in:", upload_dir, "\n")
#'
#'       # Process files...
#'
#'       # Clean up when done
#'       unlink(upload_dir, recursive = TRUE, force = TRUE)
#'     }
#'   })
#' }
#' }
#' @export
get_dipsaus_upload_dir <- function(inputId, session = shiny::getDefaultReactiveDomain()) {
  if(is.null(session)) {
    stop("get_dipsaus_upload_dir must be called from within a Shiny server function")
  }

  # Convert relative inputId to full namespaced ID
  full_id <- session$ns(inputId)

  # Get the input value (isolate to avoid creating reactive dependencies)
  input_value <- shiny::isolate(session$input[[full_id]])

  if(!is.null(input_value)) {
    upload_dir <- attr(input_value, "upload_dir")
    if(!is.null(upload_dir)) {
      return(upload_dir)
    }
  }

  # Fallback: compute from state storage (for module contexts or when input is not yet populated)
  session_token <- session$token
  state_key <- paste0(session_token, "_", full_id)

  if(exists(state_key, envir = .directory_upload_state)) {
    state <- .directory_upload_state[[state_key]]
    return(state$upload_dir)
  }

  return(NULL)
}