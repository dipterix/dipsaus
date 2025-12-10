#' @title Stream Download Files to Browser
#' @name stream_download
#' @description Push files to browser downloads using \code{'StreamSaver.js'} for efficient
#' streaming of large files. This function sends file data in chunks to the browser,
#' which then writes them directly to disk without buffering the entire file in memory.
#'
#' @param filepath path to the file to download; must be an existing file
#' @param filename the file name to use for the download (defaults to the base name
#'   of \code{filepath})
#' @param session the Shiny session; defaults to \code{shiny::getDefaultReactiveDomain()}
#' @param chunk_size size in bytes for each chunk; default is 2 MB
#' @param cleanup whether to delete the source file after download completes;
#'   default is \code{FALSE}
#' @param method download method: \code{"streamsaver"} (default, uses service worker
#'   for true streaming) or \code{"blob"} (fallback that accumulates chunks in memory)
#' @param quiet whether to suppress progress messages; default is \code{FALSE}
#'
#' @return Invisible \code{NULL}; the function is called for its side effects
#'
#' @details
#' This function requires the \code{use_shiny_dipsaus()} to be called in your
#' Shiny UI to load the necessary 'JavaScript' dependencies.
#'
#' The \code{"streamsaver"} method (default) uses service workers to enable true
#' streaming downloads that start immediately without buffering the entire file.
#' This is ideal for large files (100MB+). The \code{"blob"} method is a fallback
#' that accumulates chunks in memory before triggering the download, which works
#' on more browsers but uses more memory.
#'
#' @section Service Worker Requirements:
#' \code{'StreamSaver.js'} requires the following files to be served from the same origin:
#' \itemize{
#'   \item \code{streamsaver/StreamSaver.js} - Main library
#'   \item \code{streamsaver/sw.js} - Service worker
#'   \item \code{streamsaver/mitm.html} - Man-in-the-middle page
#' }
#'
#' These files are automatically included when using \code{\link{use_shiny_dipsaus}}.
#'
#' @examples
#' if(interactive()) {
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     use_shiny_dipsaus(),
#'     actionButton("download_btn", "Download Large File")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$download_btn, {
#'       # Create a temporary file to demonstrate
#'       temp_file <- tempfile(fileext = ".txt")
#'       writeLines(rep("Hello World!\n", 100000), temp_file)
#'
#'       # Stream the file to the browser
#'       stream_download(
#'         filepath = temp_file,
#'         filename = "large_file.txt",
#'         cleanup = TRUE  # Delete temp file after download
#'       )
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @seealso \code{\link{use_shiny_dipsaus}}, \code{\link{progress2}}
#'
#' @export
stream_download <- function(
    filepath,
    filename = basename(filepath),
    session = shiny::getDefaultReactiveDomain(),
    chunk_size = 2 * 1024^2,
    cleanup = FALSE,
    method = c("streamsaver", "blob"),
    quiet = FALSE
) {

  method <- match.arg(method)

  # Validate session
  if (!inherits(session, c("ShinySession", "session_proxy", "R6"))) {
    stop("stream_download must be called from within a Shiny session")
  }

  # Validate file exists
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }

  # Get file info
  finfo <- file.info(filepath)
  filesize <- finfo$size

  if (filesize == 0) {
    shiny::showNotification("File is empty, cannot download", type = "error")
    return(invisible(NULL))
  }

  # Calculate total chunks
  total_chunks <- ceiling(filesize / chunk_size)

  # Generate unique download ID
  download_id <- paste0("dl_", session_uuid(), "_", Sys.time())

  if (method == "streamsaver") {
    # StreamSaver method - true streaming
    .stream_download_streamsaver(
      filepath = filepath,
      filename = filename,
      session = session,
      chunk_size = chunk_size,
      cleanup = cleanup,
      filesize = filesize,
      total_chunks = total_chunks,
      download_id = download_id,
      quiet = quiet
    )
  } else {
    # Blob fallback method
    .stream_download_blob(
      filepath = filepath,
      filename = filename,
      session = session,
      chunk_size = chunk_size,
      cleanup = cleanup,
      filesize = filesize,
      total_chunks = total_chunks,
      quiet = quiet
    )
  }

  invisible(NULL)
}


# Internal: StreamSaver.js streaming method
.stream_download_streamsaver <- function(
    filepath, filename, session, chunk_size, cleanup,
    filesize, total_chunks, download_id, quiet
) {

  # Send start message to initialize the stream

  session$sendCustomMessage(
    type = "dipsaus.streamDownloadStart",
    message = list(
      download_id = download_id,
      filename = filename,
      size = filesize
    )
  )

  # Small delay to allow JS to set up the stream
  Sys.sleep(0.1)

  # Open binary connection
  con <- file(filepath, "rb")
  on.exit({
    close(con)
    if (cleanup && file.exists(filepath)) {
      unlink(filepath)
    }
  })

  # Read and send chunks
  tryCatch({
    # Initialize progress
    progress <- progress2(
      sprintf("Exporting %s", filename),
      max = total_chunks,
      shiny_auto_close = TRUE,
      quiet = quiet
    )

    for (i in seq_len(total_chunks)) {
      # Read chunk
      raw_chunk <- readBin(con, "raw", n = chunk_size)

      if (length(raw_chunk) == 0) {
        break
      }

      # Base64 encode
      base64_chunk <- jsonlite::base64_enc(raw_chunk)

      # Send chunk to client
      session$sendCustomMessage(
        type = "dipsaus.streamDownloadChunk",
        message = list(
          download_id = download_id,
          chunk = base64_chunk,
          chunk_index = i,
          total_chunks = total_chunks
        )
      )

      # Update progress
      progress_message <- sprintf("Chunk %d/%d", i, total_chunks)
      progress$inc(progress_message)

      # Small delay to prevent overwhelming the message queue
      Sys.sleep(0.001)
    }

    # Send completion message
    session$sendCustomMessage(
      type = "dipsaus.streamDownloadEnd",
      message = list(
        download_id = download_id
      )
    )

  }, error = function(e) {
    # Abort the stream on error
    session$sendCustomMessage(
      type = "dipsaus.streamDownloadAbort",
      message = list(
        download_id = download_id,
        reason = e$message
      )
    )

    shiny::showNotification(
      paste("Download failed:", e$message),
      type = "error",
      duration = NULL
    )
    stop(e)
  })
}


# Internal: Blob fallback method (accumulates in browser memory)
.stream_download_blob <- function(
    filepath, filename, session, chunk_size, cleanup,
    filesize, total_chunks, quiet
) {

  # Open binary connection
  con <- file(filepath, "rb")
  on.exit({
    close(con)
    if (cleanup && file.exists(filepath)) {
      unlink(filepath)
    }
  })

  # Read and send chunks
  tryCatch({
    # Initialize progress
    progress <- progress2(
      sprintf("Exporting %s", filename),
      max = total_chunks,
      shiny_auto_close = TRUE,
      quiet = quiet
    )

    for (i in seq_len(total_chunks)) {
      # Read chunk
      raw_chunk <- readBin(con, "raw", n = chunk_size)

      if (length(raw_chunk) == 0) {
        break
      }

      # Base64 encode
      base64_chunk <- jsonlite::base64_enc(raw_chunk)

      # Send to client
      session$sendCustomMessage(
        type = "dipsaus.blobDownloadChunk",
        message = list(
          chunk = base64_chunk,
          chunk_index = i,
          total_chunks = total_chunks,
          filename = filename,
          filesize = filesize,
          is_complete = FALSE
        )
      )

      # Update progress
      progress_message <- sprintf("Chunk %d/%d", i, total_chunks)
      progress$inc(progress_message)

      # Small delay to prevent overwhelming the message queue
      Sys.sleep(0.001)
    }

    # Send completion message
    session$sendCustomMessage(
      type = "dipsaus.blobDownloadChunk",
      message = list(
        chunk = "",
        chunk_index = total_chunks,
        total_chunks = total_chunks,
        filename = filename,
        filesize = filesize,
        is_complete = TRUE
      )
    )

  }, error = function(e) {
    shiny::showNotification(
      paste("Download failed:", e$message),
      type = "error",
      duration = NULL
    )
    stop(e)
  })
}



