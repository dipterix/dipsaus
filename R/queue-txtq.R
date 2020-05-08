# TextQueue <- R6::R6Class(
#   classname = 'TextQueue',
#   inherit = AbstractQueue,
#   private = list(
#     txtq = NULL,
#     txtq_private = NULL
#   ),
#   public = list(
#     # Get head so that we know where we are in the queue
#     `@get_head` = function(){
#       # Have to do this, they didn't expose head
#       private$txtq_private$txtq_get_head()
#     },
#     `@set_head` = function(v){
#       private$txtq_private$txtq_set_head(v)
#     },
#
#     # Get total number of items in the queue
#     `@get_total` = function(){
#       private$txtq_private$txtq_get_total()
#     },
#     `@set_total` = function(v){
#       private$txtq_private$txtq_set_total(v)
#     },
#
#     `@inc_total` = function(n=1){
#       private$txtq_private$txtq_inc_total(n=n)
#     },
#
#     `@append_header` = function(msg, ...){
#       private$txtq$push(title = 'header', message = msg)
#       return(0)
#     },
#
#     `@store_value` = function(value, key){
#       s <- ''
#       con <- textConnection('s', 'w', local = TRUE)
#       serialize(value, connection = con)
#       close(con)
#       s <- paste(s, collapse = '\n')
#       s
#     },
#     restore_value = function(hash, key, preserve = FALSE){
#       con <- textConnection(hash, 'r', local = TRUE)
#       re <- tryCatch({
#         unserialize(connection = con)
#       }, error = function(e){
#         NULL
#       })
#       re
#     },
#
#     `@log` = function(n = -1, all = FALSE){
#       if( all ){ head <- 0 }else{ head <- self$head }
#       total <- self$total
#       count <- total - head
#       if( n <= 0 ){ n <- count }else{ n <- min(n, count) }
#       if( n == 0 ){ return() }
#       if( all ){
#         tbl <- private$txtq$log()
#       }else{
#         tbl <- private$txtq$list(n = n)
#       }
#       stringr::str_split_fixed(tbl$message, '\\|', 4)
#     },
#
#     `@reset` = function() {
#       private$txtq$reset()
#     },
#
#     `@clean` = function(preserve = FALSE, ...) {
#       private$txtq$clean()
#     },
#     `@validate` = function() {
#       private$txtq$validate()
#     },
#
#     `@connect` = function(path){
#
#     },
#
#     initialize = function(path = tempfile()){
#       if(requireNamespace('txtq'))
#       private$txtq <- txtq::txtq(path)
#       private$txtq_private <- private$txtq$.__enclos_env__$private
#     },
#
#     destroy = function(){
#       private$txtq$destroy()
#       delayedAssign('.lockfile', {cat2('Queue destroyed', level = 'FATAL')},
#                     assign.env = private)
#       delayedAssign('txtq', {cat2('Queue destroyed', level = 'FATAL')},
#                     assign.env = private)
#       delayedAssign('txtq_private', {cat2('Queue destroyed', level = 'FATAL')},
#                     assign.env = private)
#       invisible()
#     }
#   )
# )
