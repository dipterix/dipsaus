# add_js_script <- function(){
#   v <- utils::packageVersion('dipsaus')
#   print(v)
#   shiny::addResourcePath(
#     prefix = sprintf('dipsaus', as.character(v)),
#     directoryPath = system.file('shiny-addons/dipsaus', package='dipsaus')
#   )
# }

#' @name sumsquared
#' @title Fast Calculation of Sum-squared for Large Matrices/Vectors
#' @description Calculate \code{sum(x^2)}, but faster when the number of
#' elements exceeds 1000.
#' @param x double, integer, or logical vector/matrix
#' @return A numerical scalar
#' @examples
#'
#' x <- rnorm(10000)
#' sumsquared(x)
#'
#' # Compare speed
#' microbenchmark::microbenchmark(
#'   cpp = {sumsquared(x)},
#'   r = {sum(x^2)}
#' )
#'
#' @export
NULL

register_shiny <- function(){

  # register CompoundInput2
  registerCompoundInput2()

  registerSetInputs()

}

.master_session_id <- local({
  master_id <- NULL
  function(uuid){
    if(!missing(uuid)){
      master_id <<- uuid
    }
    master_id
  }
})

is_master <- function(){
  identical(.master_session_id(), session_uuid())
}

.onLoad <- function(libname, pkgname){

  ns <- asNamespace(pkgname)
  assign(".locker_keys", fastmap::fastmap(), envir = ns)
  assign(".shiny_input_bindings", fastmap::fastmap(), envir = ns)
  ns$.master_session_id( session_uuid() )

  register_shiny()
  options("dipsaus.shortcuts" = fastmap2())
  
  # Register input handler for directory inputs
  shiny::registerInputHandler("dipsaus.directoryInput", function(data, shinysession, name) {
    if(is.null(data)) {
      return(NULL)
    }
    
    # Process base64 data and create temp files
    datapaths <- character(length(data$name))
    
    for(i in seq_along(data$name)) {
      tryCatch({
        if(!is.null(data$base64data[[i]]) && nchar(data$base64data[[i]]) > 0) {
          # Decode base64 data and save to temp file
          # Base64 data comes as "data:mime/type;base64,<data>"
          base64_string <- data$base64data[[i]]
          
          # Extract the actual base64 part (after the comma)
          if(grepl("^data:", base64_string)) {
            base64_string <- sub("^data:[^,]*,", "", base64_string)
          }
          
          # Create temp file with original filename
          temp_file <- tempfile(pattern = "upload_", fileext = paste0("_", basename(data$name[[i]])))
          
          # Decode and write binary data
          binary_data <- base64enc::base64decode(base64_string)
          writeBin(binary_data, temp_file)
          
          datapaths[i] <- temp_file
        } else {
          # No data available (file too large or error)
          datapaths[i] <- NA_character_
        }
      }, error = function(e) {
        warning("Failed to process file ", data$name[[i]], ": ", e$message)
        datapaths[i] <<- NA_character_
      })
    }
    
    # Convert lists to data frame structure
    df <- data.frame(
      name = unlist(data$name),
      size = unlist(data$size),
      type = unlist(data$type),
      datapath = datapaths,
      relativePath = unlist(data$relativePath),
      stringsAsFactors = FALSE
    )
    
    # Attach directory structure as attribute
    if(!is.null(data$directoryStructure)) {
      attr(df, "directoryStructure") <- data$directoryStructure
    }
    
    return(df)
  }, force = TRUE)

}


.onUnload <- function(libpath){

  dipsaus_sessionfinalizer$do_finalize()
  if('dipsaus' %in% names(shiny::resourcePaths())){
    shiny::removeResourcePath('dipsaus')
  }
}



#' @name dipsaus-defunct
#' @title Defunct Functions in Package \pkg{dipsaus}
#' The functions or variables listed here are no longer part of the package.
NULL
