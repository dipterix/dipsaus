
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
                     stringsAsFactors = FALSE, na.strings = 'NA', colClasses = 'character')
      if(!length(tbl$Key)){ return(invisible()) }

      enkeys <- sapply(keys, safe_urlencode)
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
                     stringsAsFactors = FALSE, na.strings = 'NA', colClasses = 'character')
      if(!length(tbl$Key)){ return(NULL) }

      keys <- sapply(tbl$Key, safe_urldecode)
      if(include_signatures){
        keys <- cbind(keys, tbl$Hash)
      }

      keys
    },

    `@set` = function(key, value, signature){
      # If new key, then no-harm as there is no writing
      self$`@remove`(key)

      # Generate filename from key
      encoded_key <- safe_urlencode(key)
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
      ekey <- safe_urlencode(key)
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
      lockpath <- file.path(path, 'MAP-RDSLOCK')
      if(!file.exists(lockpath)){
        writeLines(rand_string(), lockpath)
      }
      self$lockfile <- readLines(lockpath, n = 1)
    },

    # destroy a queue, free up space
    # and call `delayedAssign('.lockfile', {stop(...)}, assign.env=private)`
    # to raise error if a destroyed queue is called again later.
    destroy = function(){
      lockpath <- file.path(private$root_path, 'MAP-RDSLOCK')
      unlink(lockpath)
      if(dir.exists(private$db_dir)){
        unlink(private$db_dir, recursive = TRUE)
      }
      if(file.exists(private$header_file)){
        unlink(private$header_file)
      }
      unlink(private$root_path, recursive = TRUE, force = FALSE)

      private$valid <- FALSE
      delayedAssign('.lockfile', { cat2("Map is destroyed", level = 'FATAL') }, assign.env=private)
    }
  )
)
