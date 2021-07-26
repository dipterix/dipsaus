
QsMap <- R6::R6Class(
  classname = 'QsMap',
  inherit = FileMap,
  portable = TRUE,
  cloneable = TRUE,
  public = list(

    `@set` = function(key, value, signature){
      # If new key, then no-harm as there is no writing
      self$`@remove`(key)

      # Generate filename from key
      encoded_key <- safe_urlencode(key)
      # signature is already hashed

      # save value
      fpath <- file.path(private$db_dir, encoded_key)
      qs::qsave(value, file = fpath)

      write.table(data.frame(
        Key = encoded_key,
        Hash = signature
      ), file = private$header_file, sep = '|', append = TRUE, quote = FALSE,
      row.names = FALSE, col.names = FALSE)

      return( signature )
    },

    get = function(key, missing_default){
      ekey <- safe_urlencode(key)
      fpath <- file.path(private$db_dir, ekey)
      if( file.exists(fpath) ){
        qs::qread(fpath)
      }else{
        if(missing(missing_default)){ missing_default <- self$missing_default }
        missing_default
      }
    },

    initialize = function(path){
      path <- dir_create(path)
      private$root_path <- path
      private$db_dir <- dir_create(file.path(path, 'MAP-QSDB'))
      header_file <- file.path(path, 'MAP-QSHEAD')
      if( !file.exists(header_file) ){
        header_file <- file_create(header_file)
        writeLines('Key|Hash', con = header_file)
      }
      private$header_file <- header_file
      self$lockfile <- file.path(path, 'MAP-QSLOCK')
    }
  )
)
