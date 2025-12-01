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

- ...:

  additional arguments (currently unused)

## Value

A data frame with components: `name` (file name), `size` (file size in
bytes), `type` (MIME type), `datapath` (temporary file path on server),
and `relativePath` (full relative path including subdirectories). The
data frame also has an attribute `directoryStructure` containing a
nested list representing the directory tree.

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

## Examples

``` r

library(shiny)
library(dipsaus)

ui <- basicPage(
  fancyDirectoryInput('dir_input', "Please upload a directory")
)

if(interactive()) {
  shinyApp(
    ui,
    server = function(input, output, session){
      observeEvent(input$dir_input, {
        files <- input$dir_input
        if(!is.null(files)) {
          cat("Uploaded files:\n")
          print(files[, c("name", "size", "relativePath")])

          # Access directory structure
          dir_struct <- attr(files, "directoryStructure")
          cat("\nDirectory structure:\n")
          print(str(dir_struct))
        }
      })
    },
    options = list(launch.browser = TRUE)
  )
}
```
