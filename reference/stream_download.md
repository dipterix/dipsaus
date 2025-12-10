# Stream Download Files to Browser

Push files to browser downloads using `'StreamSaver.js'` for efficient
streaming of large files. This function sends file data in chunks to the
browser, which then writes them directly to disk without buffering the
entire file in memory.

## Usage

``` r
stream_download(
  filepath,
  filename = basename(filepath),
  session = shiny::getDefaultReactiveDomain(),
  chunk_size = 2 * 1024^2,
  cleanup = FALSE,
  method = c("streamsaver", "blob"),
  quiet = FALSE
)
```

## Arguments

- filepath:

  path to the file to download; must be an existing file

- filename:

  the file name to use for the download (defaults to the base name of
  `filepath`)

- session:

  the Shiny session; defaults to
  [`shiny::getDefaultReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html)

- chunk_size:

  size in bytes for each chunk; default is 2 MB

- cleanup:

  whether to delete the source file after download completes; default is
  `FALSE`

- method:

  download method: `"streamsaver"` (default, uses service worker for
  true streaming) or `"blob"` (fallback that accumulates chunks in
  memory)

- quiet:

  whether to suppress progress messages; default is `FALSE`

## Value

Invisible `NULL`; the function is called for its side effects

## Details

This function requires the
[`use_shiny_dipsaus()`](https://dipterix.org/dipsaus/reference/use_shiny_dipsaus.md)
to be called in your Shiny UI to load the necessary 'JavaScript'
dependencies.

The `"streamsaver"` method (default) uses service workers to enable true
streaming downloads that start immediately without buffering the entire
file. This is ideal for large files (100MB+). The `"blob"` method is a
fallback that accumulates chunks in memory before triggering the
download, which works on more browsers but uses more memory.

## Service Worker Requirements

`'StreamSaver.js'` requires the following files to be served from the
same origin:

- `streamsaver/StreamSaver.js` - Main library

- `streamsaver/sw.js` - Service worker

- `streamsaver/mitm.html` - Man-in-the-middle page

These files are automatically included when using
[`use_shiny_dipsaus`](https://dipterix.org/dipsaus/reference/use_shiny_dipsaus.md).

## See also

[`use_shiny_dipsaus`](https://dipterix.org/dipsaus/reference/use_shiny_dipsaus.md),
[`progress2`](https://dipterix.org/dipsaus/reference/progress2.md)

## Examples

``` r
if(interactive()) {
  library(shiny)

  ui <- fluidPage(
    use_shiny_dipsaus(),
    actionButton("download_btn", "Download Large File")
  )

  server <- function(input, output, session) {
    observeEvent(input$download_btn, {
      # Create a temporary file to demonstrate
      temp_file <- tempfile(fileext = ".txt")
      writeLines(rep("Hello World!\n", 100000), temp_file)

      # Stream the file to the browser
      stream_download(
        filepath = temp_file,
        filename = "large_file.txt",
        cleanup = TRUE  # Delete temp file after download
      )
    })
  }

  shinyApp(ui, server)
}
```
