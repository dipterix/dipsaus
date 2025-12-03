# Get upload directory for fancyDirectoryInput

Retrieve the upload directory path for a
[`fancyDirectoryInput`](https://dipterix.org/dipsaus/reference/fancyDirectoryInput.md).
The upload directory is deterministically generated based on the session
token and input ID, and files uploaded to the input preserve their
relative directory structure within this directory.

## Usage

``` r
get_dipsaus_upload_dir(inputId, session = shiny::getDefaultReactiveDomain())
```

## Arguments

- inputId:

  the input ID (relative to the session, will be automatically
  namespaced via `session$ns(inputId)`)

- session:

  the Shiny session object (default:
  [`shiny::getDefaultReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html))

## Value

Character string with the upload directory path, or `NULL` if the input
has no uploaded files yet. The directory path follows the pattern
`tempdir()/dipsaus_uploads/{6-char-hash}/` where the hash is derived
from `digest(session_token + full_inputId)`.

## Details

This function is useful for:

- Manually cleaning up uploaded files via
  `unlink(get_dipsaus_upload_dir(inputId), recursive = TRUE)`

- Processing files with their preserved directory structure

- Accessing the upload directory for custom file operations

The upload directory is deterministic for a given session and input ID,
meaning multiple uploads to the same input will use the same directory
(unless `autoCleanup = TRUE` is set, which clears the directory before
each new upload).

In Shiny modules, use the relative `inputId` (e.g., `"dir_input"`)
rather than the full namespaced ID. The function will automatically
handle namespace conversion via `session$ns(inputId)`.

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  observeEvent(input$dir_input, {
    files <- input$dir_input
    
    if(!is.null(files) && attr(files, "upload_status") == "completed") {
      # Get the upload directory
      upload_dir <- get_dipsaus_upload_dir("dir_input")
      cat("Files are stored in:", upload_dir, "\n")
      
      # Process files...
      
      # Clean up when done
      unlink(upload_dir, recursive = TRUE, force = TRUE)
    }
  })
}
} # }
```
