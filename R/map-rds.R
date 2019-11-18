
FileMap <- R6::R6Class(
  classname = 'FileMap',
  inherit = AbstractMap,
  portable = TRUE,
  cloneable = TRUE,
  private = list(
    root_path = character(0),
    header_file = character(0),
    db_dir = character(0)
  ),
  public = list(

    `@remove` = function(keys){
      tbl <- read.csv(private$header_file, header = TRUE, sep = '|',
                     stringsAsFactors = FALSE)
      if(!length(tbl$Key)){ return(invisible()) }

      enkeys <- sapply(keys, base64url::base64_urlencode)
      sel <- tbl$Key %in% enkeys
      if( any(sel) ){
        fs <- tbl$Key[sel]
        tbl <- tbl[!sel, ]
        write.table(tbl, private$header_file, sep = '|', quote = FALSE,
                    row.names = FALSE, col.names = TRUE, append = FALSE)
        # Unlink files
        lapply(file.path(private$db_dir, fs), unlink)
      }
      invisible()
    },
    reset = function(...){
      writeLines('Key|Hash', private$header_file)
      unlink(private$db_dir, recursive = TRUE)
      dir_create(private$db_dir)
    },

    keys = function(include_signatures = FALSE){
      tbl <- read.csv(private$header_file, header = TRUE, sep = '|',
                     stringsAsFactors = FALSE)
      if(!length(tbl$Key)){ return(NULL) }

      keys <- sapply(tbl$Key, base64url::base64_urldecode)
      if(include_signatures){
        keys <- cbind(keys, tbl$Hash)
      }

      keys
    },

    `@set` = function(key, value, signature){
      # If new key, then no-harm as there is no writing
      self$`@remove`(key)

      # Generate filename from key
      encoded_key <- base64url::base64_urlencode(key)
      # signature is already hashed

      # save value
      fpath <- file.path(private$db_dir, encoded_key)
      saveRDS(value, file = fpath)

      write.table(data.frame(
        Key = encoded_key,
        Hash = signature
        ), file = private$header_file, sep = '|', append = TRUE, quote = FALSE,
        row.names = FALSE, col.names = FALSE)

      return( signature )
    },

    `@get` = function(key){
      not_implemented()
    },
    get = function(key, missing_default){
      ekey <- base64url::base64_urlencode(key)
      fpath <- file.path(private$db_dir, ekey)
      if( file.exists(fpath) ){
        readRDS(fpath)
      }else{
        if(missing(missing_default)){ missing_default <- self$missing_default }
        missing_default
      }
    },

    mget = function(keys, missing_default){
      if(missing(missing_default)){ missing_default <- self$missing_default }
      force(missing_default)

      re <- lapply(keys, function(key){
        self$get(key, missing_default)
      })
      names(re) <- keys
      re
    },

    `@validate` = function(...){
      stopifnot2(file.exists(self$lockfile), msg = 'Lock-file is missing')
      stopifnot2(file.exists(private$header_file), msg = 'Header-file is missing')
      stopifnot2(dir.exists(private$db_dir), msg = 'Database directory is missing')
      stopifnot2(isTRUE(readLines(private$header_file, n = 1) == "Key|Hash"),
                 msg = 'Corruped header file')
    },

    # will be called during Class$new(...), three tasks,
    # 1. set `get_locker` `free_locker` if lock type is not a file
    # 2. set lockfile (if using default lockers)
    # 3. call self$connect
    initialize = function(path){
      path <- dir_create(path)
      private$root_path <- path
      private$db_dir <- dir_create(file.path(path, 'MAP-RDSDB'))
      header_file <- file.path(path, 'MAP-RDSHEAD')
      if( !file.exists(header_file) ){
        header_file <- file_create(header_file)
        writeLines('Key|Hash', con = header_file)
      }
      private$header_file <- header_file
      self$lockfile <- file.path(path, 'MAP-RDSLOCK')
    },

    # destroy a queue, free up space
    # and call `delayedAssign('.lockfile', {stop(...)}, assign.env=private)`
    # to raise error if a destroyed queue is called again later.
    destroy = function(){
      unlink(self$lockfile)
      private$valid <- FALSE
      delayedAssign('.lockfile', { stop("Map is destroyed", call. = FALSE) }, assign.env=private)
    }
  )
)
