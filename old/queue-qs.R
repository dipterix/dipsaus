QsQueue <- R6::R6Class(
  classname = 'QsQueue',
  inherit = FileQueue,
  public = list(
    compress_level = 4L,
    nthread = 1L,

    `@store_value` = function(value, key){
      path <- file.path(private$db_dir, key)
      qs::qsave(value, file = path, compress_level = self$compress_level,
                nthreads = self$nthread)
      key
    },
    restore_value = function(hash, key, preserve = FALSE){
      path <- file.path(private$db_dir, key)
      re <- tryCatch({
        qs::qread(file = path, nthreads = self$nthread)
      }, error = function(e){
        NULL
      })
      if(!preserve){
        unlink(path)
      }
      re
    }

  )
)
