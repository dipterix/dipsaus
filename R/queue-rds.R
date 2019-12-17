FileQueue <- R6::R6Class(
  classname = 'FileQueue',
  inherit = AbstractQueue,
  private = list(
    root_path = character(0),
    head_file = character(0),
    total_file = character(0),
    header_file = character(0),
    db_dir = character(0)
  ),
  public = list(
    # Get head so that we know where we are in the queue
    `@get_head` = function(){
      scan(file = private$head_file, what = integer(), n = 1, quiet = TRUE)
    },
    `@set_head` = function(v){
      write(v, private$head_file, ncolumns = 1, append = FALSE)
    },

    # Get total number of items in the queue
    `@get_total` = function(){
      scan(file = private$total_file, what = integer(), n = 1, quiet = TRUE)
    },
    `@set_total` = function(v){
      write(v, private$total_file, ncolumns = 1, append = FALSE)
    },

    `@inc_total` = function(n=1){
      con <- file(description = private$total_file, open = 'r+')
      on.exit(close(con))
      total <- scan(file = con, what = integer(), n = 1, quiet = TRUE) + n
      write(total, file = con, ncolumns = 1, append = FALSE)
      invisible(total)
    },

    `@append_header` = function(msg, ...){
      con <- file(private$header_file, 'a+')
      on.exit(close(con))
      writeLines(msg, con)
      return(length(msg))
    },

    `@store_value` = function(value, key){
      path <- file.path(private$db_dir, key)
      saveRDS(value, file = path)
      key
    },
    restore_value = function(hash, key, preserve = FALSE){
      path <- file.path(private$db_dir, key)
      re <- tryCatch({
        readRDS(path)
      }, error = function(e){
        NULL
      })
      if(!preserve){
        unlink(path)
      }
      re
    },

    `@log` = function(n = -1, all = FALSE){
      if( all ){ head <- 0 }else{ head <- self$head }
      total <- self$total
      count <- total - head
      if( n <= 0 ){ n <- count }else{ n <- min(n, count) }
      if( n == 0 ){ return() }
      re <- read.table(private$header_file, skip = head, header = TRUE, nrows = n)
      stringr::str_split_fixed(re[[1]], '\\|', 4)
    },

    `@reset` = function() {
      self$`@set_head`(0)
      self$`@set_total`(0)
      write('HEADER', file = private$header_file, append = FALSE)
      fs <- list.files(private$db_dir, full.names = TRUE)
      lapply(fs, unlink)
    },

    `@clean` = function(preserve = FALSE, ...) {
      head <- self$head
      total <- self$total
      if( total - head < 1 ){
        self$`@reset`()
      }else{
        re <- readLines(private$header_file)
        if( !preserve && head > 0 ){
          lapply(seq_len(head), function(ii){
            re <- self$print_item(stringr::str_split_fixed(re[[ii+1]], '\\|', n = 4))
            unlink(file.path(private$db_dir, re$hash))
          })
        }
        writeLines(re[c(1, (head+2) : (total+1))], private$header_file)
        self$head <- 0
        self$total <- total - head
      }
    },
    `@validate` = function() {
      assert_dir(private$root_path)
      assert_dir(private$db_dir)
      assert_file_scalar(private$head_file)
      assert_file_scalar(private$total_file)
      assert_file(private$header_file)
    },

    `@connect` = function(path){
      if(!missing(path)){
        path <- normalizePath(path, mustWork = TRUE)
        private$root_path <- path
      }

      if( !dir.exists( private$root_path ) ){
        cat2('path is not a directory', level = 'FATAL')
      }

      tmp <- file.path(private$root_path, 'HEAD')
      file_create(tmp)
      private$head_file <- normalizePath(tmp, mustWork = TRUE)

      head <- scan(tmp, what = integer(), n = 1, nlines = 1, quiet = TRUE)
      if(length(head) != 1 || !is.numeric(head) || head < 0){
        self$`@set_head`(0)
      }


      tmp <- file.path(private$root_path, 'TOTAL')
      file_create(tmp)
      private$total_file <- normalizePath(tmp, mustWork = TRUE)

      total <- scan(tmp, what = integer(), n = 1, nlines = 1, quiet = TRUE)
      if(length(total) != 1 || !is.numeric(total) || total < 0){
        self$`@set_total`(0)
      }

      tmp <- file.path(private$root_path, 'KEYS')
      if( !file.exists(tmp) ){
        file_create(tmp)
        writeLines('HEADER', tmp)
      }
      private$header_file <- normalizePath(tmp, mustWork = TRUE)


      tmp <- file.path(private$root_path, 'DB')
      dir_create(tmp)
      private$db_dir <- normalizePath(tmp, mustWork = TRUE)


      self$`@clean`(preserve = TRUE)
      self$`@validate`()

    },

    initialize = function(path = tempfile()){
      dir_create(path)
      private$root_path <- normalizePath(path)
      lockpath <- file.path(path, 'LOCK')
      if(!file.exists(lockpath)){
        writeLines(rand_string(), lockpath)
      }
      self$lockfile <- readLines(con = lockpath, n = 1)
      self$connect()
    },

    destroy = function(){
      dipsaus_unlock(self$lockfile)
      unlink(private$db_dir, recursive = TRUE, force = TRUE)
      unlink(file.path(private$root_path, 'LOCK'), force = TRUE)
      unlink(private$head_file, force = TRUE)
      unlink(private$total_file, force = TRUE)
      unlink(private$header_file, force = TRUE)

      # If user accidentally set root_path to be '/', removing it would be a disaster
      unlink(private$root_path, recursive = TRUE, force = FALSE)

      delayedAssign('.lockfile', {cat2('Queue destroyed', level = 'FATAL')}, assign.env = private)

      invisible()
    }
  )
)
