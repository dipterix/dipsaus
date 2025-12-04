# Shiny drag-and-drop directory input

Fancy drag and drop directory upload for `shiny` apps. This function
allows users to drag and drop entire directories. Note: This feature
requires browser support for the `webkitdirectory` attribute (Chrome,
Edge, Safari). Firefox has limited support.

## Usage

``` r
fancyDirectoryInput(
  inputId,
  label,
  width = NULL,
  after_content = "Drag & drop directory, or button",
  size = c("s", "m", "l", "xl"),
  maxSize = NULL,
  progress = FALSE,
  autoCleanup = FALSE,
  autoCleanupLocked = FALSE,
  ...
)
```

## Arguments

- inputId:

  the input slot that will be used to access the value

- label:

  display label for the control, or NULL for no label.

- width:

  the width of the input

- after_content:

  tiny content that is to be displayed below the input box

- size:

  height of the widget, choices are `'s'`, `'m'`, `'l'`, and `'xl'`

- maxSize:

  maximum file size per file in bytes (default uses
  `shiny.maxRequestSize` option, typically 5MB)

- progress:

  logical or character; if `TRUE`, displays upload progress using
  [`progress2`](https://dipterix.org/dipsaus/reference/progress2.md); if
  a character string, uses it as the progress title; if `FALSE`
  (default), no progress is shown

- autoCleanup:

  logical; if `TRUE`, removes all files from the upload directory before
  each new upload. Default is `FALSE`. This is useful to prevent stale
  files from previous uploads. Can be changed dynamically by updating
  the `data-auto-cleanup` HTML attribute on the input element.

- autoCleanupLocked:

  logical; if `TRUE`, hides the auto-cleanup checkbox, preventing users
  from changing the setting. Default is `FALSE`, which shows the
  checkbox allowing users to toggle auto-cleanup behavior.

- ...:

  additional arguments (currently unused)

## Value

A reactive data frame with components: `fileId` (unique file
identifier), `name` (file name), `size` (file size in bytes), `type`
(MIME type), `datapath` (temporary file path on server), and
`relativePath` (full relative path including subdirectories). The data
frame also has attributes: `directoryStructure` (nested list
representing the directory tree), `ready` (logical indicating if all
files are processed), `totalFiles` (total number of files),
`upload_status` (one of "initialized", "completed", or "errored"), and
`upload_dir` (character string with the upload directory path where
files are stored, preserving their relative directory structure).

**Important:** The `datapath` column is `NA` initially when
`upload_status = "initialized"`. The input automatically updates when
all files complete (`upload_status = "completed"`), and `datapath`
values are populated with server file paths. Check
`attr(input$<inputId>, "upload_status")` to determine when files are
ready.

Optional real-time tracking: Individual file data is available via
`input$<inputId>__file` as files upload. File processing status can be
tracked via `input$<inputId>__status`, which returns a data frame with
columns: `fileId`, `name`, `relativePath`, `status`
(pending/processing/complete/error/skipped), `progress` (0-100), and
`error` (error message if any).

## Details

The directory input uses the `webkitdirectory` HTML attribute which is
not part of the HTML5 standard but is widely supported. Browser
compatibility:

- Chrome/Edge: Full support

- Safari: Full support

- Firefox: Partial support (desktop only, no mobile)

- Internet Explorer: Not supported

Hidden files (starting with '.') are filtered out by default on the
client side.

Files are transferred as base64-encoded data, so they use approximately
33% more bandwidth than their actual size. Files exceeding `maxSize`
will be skipped with a warning in the browser console.

**Upload Directory Management:**

Uploaded files are stored in a session-specific directory with a
deterministic path: `tempdir()/dipsaus_uploads/{6-char-hash}/` where the
hash is computed as
`substr(digest(session_token + full_inputId), 1, 6)`. This ensures:

- The same directory is used for all uploads to the same input within a
  session

- Files preserve their original relative directory structure within this
  directory

- Different sessions and inputs get isolated directories

For example, uploading a directory with structure
`project/src/utils/helper.R` will create
`tempdir()/dipsaus_uploads/a3f5c2/project/src/utils/helper.R`.

The `autoCleanup` parameter controls whether the upload directory is
cleaned before each new upload. When `FALSE` (default), files accumulate
across uploads. When `TRUE`, the directory is removed and recreated
before each upload, preventing stale files from previous uploads. A
checkbox is displayed below the input widget allowing users to toggle
the auto-cleanup behavior dynamically. Use
[`get_dipsaus_upload_dir`](https://dipterix.org/dipsaus/reference/get_dipsaus_upload_dir.md)
to retrieve the upload directory path for manual cleanup:
`unlink(get_dipsaus_upload_dir(inputId), recursive = TRUE)`

**Related Functions:**

- [`observeDirectoryProgress`](https://dipterix.org/dipsaus/reference/observeDirectoryProgress.md):
  Enable progress tracking with
  [`progress2`](https://dipterix.org/dipsaus/reference/progress2.md) for
  directory uploads. Call this in your server function to display upload
  progress automatically.

- [`get_dipsaus_upload_dir`](https://dipterix.org/dipsaus/reference/get_dipsaus_upload_dir.md):
  Retrieve the upload directory path for a given input ID. Useful for
  manual file cleanup or custom processing.

## See also

[`observeDirectoryProgress`](https://dipterix.org/dipsaus/reference/observeDirectoryProgress.md)
for progress tracking,
[`get_dipsaus_upload_dir`](https://dipterix.org/dipsaus/reference/get_dipsaus_upload_dir.md)
for directory path retrieval,
[`fancyFileInput`](https://dipterix.org/dipsaus/reference/fancyFileInput.md)
for single file uploads,
[`progress2`](https://dipterix.org/dipsaus/reference/progress2.md) for
custom progress bars

## Examples

``` r

library(shiny)
library(dipsaus)

ui <- basicPage(
  fancyDirectoryInput('dir_input', "Please upload a directory")
)

# Example with progress tracking
ui2 <- basicPage(
  fancyDirectoryInput('dir_input2', "Upload with progress", progress = TRUE)
)

if(interactive()) {
  # Basic example
  shinyApp(
    ui,
    server = function(input, output, session){
      # Observe directory upload - updates automatically when complete
      observeEvent(input$dir_input, {
        files <- input$dir_input
        if(!is.null(files)) {
          upload_status <- attr(files, "upload_status")
          
          cat("Directory upload event:\n")
          cat("  Status:", upload_status, "\n")
          cat("  Total files:", attr(files, "totalFiles"), "\n")
          cat("  Ready:", attr(files, "ready"), "\n")
          
          if(upload_status == "completed") {
            # All files uploaded - datapaths are now populated!
            cat("\nAll files uploaded successfully!\n")
            cat("Files with datapaths:\n")
            print(files[!is.na(files$datapath), 
                        c("name", "relativePath", "datapath")])
            
            # Now you can process all files
            for(i in seq_len(nrow(files))) {
              if(!is.na(files$datapath[i])) {
                cat("\nProcessing:", files$name[i], "\n")
                cat("  Server path:", files$datapath[i], "\n")
                cat("  File size:", file.size(files$datapath[i]), "bytes\n")
              }
            }
          } else if(upload_status == "initialized") {
            # Initial metadata - datapaths are NA at this point
            cat("Upload started, processing files...\n")
            
            # Access directory structure
            dir_struct <- attr(files, "directoryStructure")
            cat("\nDirectory structure:\n")
            print(str(dir_struct, max.level = 2))
          }
        }
      })
      
      # Optional: Track individual files as they upload (real-time)
      observeEvent(input$dir_input__file, {
        file_data <- input$dir_input__file
        if(!is.null(file_data) && !is.na(file_data$datapath)) {
          cat("File uploaded:", file_data$name, "->", file_data$datapath, "\n")
        }
      })
      
      # Optional: Monitor upload progress
      observeEvent(input$dir_input__status, {
        status <- input$dir_input__status
        if(!is.null(status) && is.data.frame(status)) {
          completed <- sum(status$status == "complete")
          total <- nrow(status)
          cat("Progress:", completed, "/", total, "files\n")
        }
      })
    },
    options = list(launch.browser = TRUE)
  )
  
  # Example with progress tracking
  shinyApp(
    ui2,
    server = function(input, output, session){
      # Enable progress tracking
      observeDirectoryProgress("dir_input2")
      
      # Process files when complete
      observeEvent(input$dir_input2, {
        files <- input$dir_input2
        if(!is.null(files) && attr(files, "upload_status") == "completed") {
          cat("Received", nrow(files), "files\n")
        }
      })
    },
    options = list(launch.browser = TRUE)
  )
  
  # Example with autoCleanup and manual directory cleanup
  ui3 <- basicPage(
    fancyDirectoryInput('dir_input3', "Upload directory", autoCleanup = TRUE),
    actionButton('cleanup', 'Clean Up Files')
  )
  
  shinyApp(
    ui3,
    server = function(input, output, session){
      observeEvent(input$dir_input3, {
        files <- input$dir_input3
        if(!is.null(files) && attr(files, "upload_status") == "completed") {
          # Get upload directory with preserved structure
          upload_dir <- attr(files, "upload_dir")
          cat("Files stored in:", upload_dir, "\\n")
          cat("Example file path:", files$datapath[1], "\\n")
          # Process files with their directory structure...
        }
      })
      
      # Manual cleanup option
      observeEvent(input$cleanup, {
        upload_dir <- get_dipsaus_upload_dir('dir_input3')
        if(!is.null(upload_dir) && dir.exists(upload_dir)) {
          unlink(upload_dir, recursive = TRUE, force = TRUE)
          cat("Cleaned up:", upload_dir, "\\n")
        }
      })
    },
    options = list(launch.browser = TRUE)
  )
}
```
