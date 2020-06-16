


# init -> ready, suspended
.async_evaluator_states <- factor(
  c('init', 'ready', 'running', 'suspend', 'stopped', 'terminated'),
  levels = c('init', 'ready', 'running', 'suspend', 'stopped', 'terminated'),
  ordered = TRUE)


c.cluster <- function (...) {
  x <- list(...)
  class <- lapply(x, FUN = class)
  class <- Reduce(intersect, class)
  x <- lapply(x, FUN = unclass)
  x <- Reduce(c, x)
  class(x) <- class
  x
}

ChildEvaluator <- R6::R6Class(
  classname = 'ChildEvaluator',
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    path = character(0),
    state = .async_evaluator_states[[5]],
    uuid = NULL,
    # normal, and high-priority queue
    queue0 = NULL,
    queue1 = NULL,
    # feedback messages
    queue2 = NULL,
    queue_callback = NULL,
    map = NULL,
    index = NULL,
    master_uuid = character(0)
  ),
  public = list(
    DEBUG = FALSE,
    in_master_session = function(){
      isTRUE(private$master_uuid == session_uuid())
    },
    initialize = function( tmp_path, index, master_uuid, debug = FALSE ){
      self$DEBUG <- debug
      private$path <- dir_create(tmp_path)
      private$state <- .async_evaluator_states[[4]] # 'suspend'
      private$uuid <- session_uuid()
      private$master_uuid <- master_uuid
      private$index <- index
      private$queue0 <- qs_queue(file.path(private$path, 'Q0'))
      private$queue1 <- qs_queue(file.path(private$path, 'Q1'))
      private$queue2 <- qs_queue(file.path(private$path, 'Q2'))
      private$queue_callback <- qs_queue(file.path(private$path, 'Q3'))
      private$map <- qs_map(file.path(private$path, 'M0'))
      if(debug){
        self$logger2 <- function(...){cat2(as.character(private$state), ': ', ...)}
      }else{
        self$logger2 <- function(...){}
      }
      private$map$set(sprintf('CHILD_UUID_%d', private$index), private$uuid)
      self$set_state( 'ready' )
    },
    set_state = function( state ){
      stopifnot2(state %in% c('init', 'ready', 'suspend', 'stopped', 'running', 'terminated'),
                 msg = 'Wrong state for child session!')
      state <- .async_evaluator_states[.async_evaluator_states == state]
      private$state <- state
      private$map$set(sprintf('CHILD_STATE_%d', private$index), state)
      self$logger2('Set state - ', as.character(state))
    },
    run = function(){
      if(private$state %in% c('ready')){
        # Get from queue and run
        # TODO: Implement this
        item <- private$queue1$pop()
        if(!length(item)){
          item <- private$queue0$pop()
        }
        if(!length(item)){ return() }

        value <- item[[1]]$value
        message <- item[[1]]$message

        if( length(value$id)!= 1 || !is.character(value$id)){
          return()
        }
        self$logger2('Got 1 item - ', message)

        private$queue_callback$push(value = message, message = '')

        future::future({
          tryCatch({
            self$handle_message(message, value)
          }, error = function(e){
            self$logger2(paste(as.character(e), collapse = '\n'))
            private$queue2$push(value = list(
              passed = FALSE,
              error = e
            ), message = value$id)
          }, finally = {
            private$queue_callback$pop()
            self$set_state( 'ready' )
          })
        })

      }
    },

    handle_message = function(message, value){
      # message: suspend, eval, set_state
      switch (
        message,
        # suspend
        'suspend' = {
          self$set_state( 'suspend' )
        },
        'eval' = {
          # Value: id (key), at_global (T/F), expr
          d <- private$map$get(key = value$id, missing_default = list())
          private$map$remove( value$id )
          if( isTRUE(value$at_global) ){
            env <- .GlobalEnv
          }else{
            env <- new.env(parent = .GlobalEnv)
          }
          re <- eval_dirty(value$expr, env = env, data = d)

          private$queue2$push(value = list(
            passed = TRUE,
            result = re
          ), message = value$id)
        }
      )
    },

    save_map = function(key, value, ...){
      private$map$set(key = sprintf('%s_%d', key, private$index), value = value, signature = NULL, ...)
    },

    logger2 = NULL
  ),
  active = list(
    is_child = function(){ TRUE },
    id = function(){
      private$uuid
    },
    n_running = function(){
      private$queue_callback$count
    }
  )
)

child_evaluator_singleton <- local({
  env <- environment()
  evaluator <- NULL
  ncores <- 0
  last_n <- 0
  # ffs = list()
  check <- function(){
    evaluator$n_running
    # n = length(ffs)
    # if(n){
    #   rs = vapply(ffs, future::resolved, FUN.VALUE = FALSE)
    #   if(any(rs)){
    #     env$ffs[rs] <- NULL
    #   }
    #   n - sum(rs)
    # }
    # n
  }
  new <- function( path, idx, master_uuid, ncores = 1, debug = FALSE ){
    if( is.null(evaluator) ){
      env$evaluator <- ChildEvaluator$new( tmp_path = path, index = idx,
                                        master_uuid = master_uuid, debug = debug )
    }
    if( ncores != env$ncores ){
      future::plan(future::multisession, workers = ncores)
      env$ncores <- ncores
    }
    evaluator$id
  }
  get <- function(){
    evaluator
  }

  run <- function(){
    if( is.null(evaluator) ){ return() }
    # n = length(ffs)
    # if( ncores <= n){
    #   n = check()
    # }
    n <- check()
    if( ncores > n){
      evaluator$set_state( 'ready' )
      # returns a list
      f <- lapply(seq_len(ncores - n), function(ii){
        evaluator$run()
      })
      f <- drop_nulls(f)
      if( length(f) ){
        # env$ffs <- c(ffs, f)
        n <- n + length(f)
      }
    }else{
      evaluator$set_state( 'running' )
    }
    if( last_n != n ){
      evaluator$save_map('PROCESSING', n)
      env$last_n <- n
    }
  }
  list(
    new = new,
    get = get,
    run = run,
    check = check
  )
})

launch_child_evaluator <- child_evaluator_singleton$new

get_child_evaluator <- child_evaluator_singleton$get

run_child_evaluator <- child_evaluator_singleton$run

#' Generator Class for Asynchronous Evaluation
#' @export
MasterEvaluator <- R6::R6Class(
  classname = 'MasterEvaluator',
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    cl = NULL,
    path = character(0),
    state = .async_evaluator_states[[5]],
    uuid = NULL,
    child_uuids = NULL,
    child_states = NULL,
    # normal, and high-priority queue
    queue0 = NULL,
    queue1 = NULL,
    # feedback messages
    queue2 = NULL,
    map_callback = NULL,
    map = NULL,

    finished = 0,
    total = 0,

    `@later_loop` = NULL
  ),
  public = list(
    logger2 = NULL,
    listen_interval = 1,

    get_active_idx = function( which, exclude = 'terminated' ){
      if( !length(private$cl) ){ return(NULL) }
      actives <- which(!private$child_states %in% exclude)
      if( !length(actives) ){ return(NULL) }
      if( length(which) == 1 && which <= 0 ){
        which <- actives
      }else{
        which <- which[which %in% actives]
      }
      which
    },

    initialize = function( tmp_path = tempfile(), debug = FALSE ){
      private$path <- dir_create(tmp_path)
      private$state <- .async_evaluator_states[[4]] # 'suspend'
      private$uuid <- session_uuid()
      private$queue0 <- qs_queue(file.path(private$path, 'Q0'))
      private$queue1 <- qs_queue(file.path(private$path, 'Q1'))
      private$queue2 <- qs_queue(file.path(private$path, 'Q2'))
      private$map_callback <- session_map()
      private$map <- qs_map(file.path(private$path, 'M0'))
      private$`@later_loop` <- later::create_loop(autorun = FALSE)
      if(debug){
        self$logger2 <- function(...){cat2(as.character(private$state), ': ', ...)}
      }else{
        self$logger2 <- function(...){}
      }
      private$map$set('MASTER_UUID', private$uuid)
      self$set_state( 'ready' )
    },

    # Register finalizer:
    finalize = function(...){
      if( self$in_master_session() ){
        tryCatch({
          self$set_state( 'terminated' )
        }, error = function(e){
          private$state <- .async_evaluator_states[[6]]
        })
        self$logger2('Finalizing - closing clusters')
        later::destroy_loop( private$`@later_loop` )
        self$shutdown( which = -1, force = TRUE )
        f <- function(...){}
        tryCatch({ private$map_callback$destroy() }, error = f, warning = f)
        tryCatch({ private$queue1$destroy() }, error = f, warning = f)
        tryCatch({ private$queue0$destroy() }, error = f, warning = f)
        tryCatch({ private$map$destroy() }, error = f, warning = f)
        tryCatch({ private$queue2$destroy() }, error = f, warning = f)
      }
      invisible()
    },

    set_state = function( state ){
      stopifnot2(state %in% c('init', 'ready', 'suspend', 'stopped', 'running', 'terminated'),
                 msg = 'Wrong state for master session!')
      state <- .async_evaluator_states[.async_evaluator_states == state]
      private$state <- state
      private$map$set('MASTER_STATE', state)
      self$logger2('Set state - ', as.character(state))
    },

    # Check whether the instance is in master session
    in_master_session = function(){
      isTRUE(session_uuid() == private$uuid)
    },

    n_workers = function( deep = FALSE, max_state = 'stopped',
                          min_state = 'ready', return_index = FALSE ){
      if(!length(private$cl)){ return( 0 ) }

      if( deep ){
        currect_state <- private$state
        on.exit({
          if( currect_state == 'running' ){
            self$listen()
          }else{
            self$set_state( currect_state )
          }
        })
        self$set_state( 'suspend' )
        self$check_state( which = -1 )

        sel <- private$child_states <= max_state &
          private$child_states >= min_state
        not_running <- private$child_states %in% c('ready') & sel

        if(any(not_running)){
          # TODO: ping-pong and check results
        }

      }else{
        # self$check_state( which = -1 )

        sel <- private$child_states <= max_state &
          private$child_states >= min_state
      }

      if( return_index ){
        return(which(sel))
      }else{
        return( sum(sel) )
      }


    },

    ramp_up = function( n, subs = 1, ... ){
      previous_state <- private$state
      stopifnot2(previous_state < 'stopped', msg = 'The evaluator is stopped. Cannot ramp up.')

      # Suspend and stop listen in case of failure
      self$set_state( 'suspend' )

      if( !self$in_master_session() ){
        cat2('You are not in the master session, cannot ramp up more workers.')
        return(FALSE)
      }
      m <- self$n_workers( deep = TRUE, min_state = 'init' )
      more <- n - m
      if( more <= 0 ){ return(FALSE) }
      cat2('Initializing ', more, ' workers with ', subs, ' sub-workers. Total workers will be (',
           m, '+', more, ') x ', subs)
      new_cl <- future::makeClusterPSOCK(worker = more, ... )

      # Add clusters first
      if(!length(private$cl)){
        private$cl <- new_cl
        private$child_states <- rep(.async_evaluator_states[[4]], more)
      }else{
        private$cl <- c(private$cl, new_cl)
        states <- c(private$child_states, rep(.async_evaluator_states[[4]], more))
        if(is.numeric(states)){
          states <- .async_evaluator_states[states]
        }
        private$child_states <- states
      }

      # TODO: Initialize clusters
      self$logger2('Clusters created. TODO: Initialize clusters')

      tmp_path <- private$path
      master_uuid <- private$uuid
      new_idx <- seq_len(more) + length(private$cl) - more
      lapply(new_idx, function(ii){
        f <- self$fire_direct(rlang::quo({
          local({
            dipsaus <- asNamespace('dipsaus')
            dipsaus$launch_child_evaluator(!!tmp_path, !!ii, !!master_uuid, !!subs)
          }, envir = new.env(parent = globalenv()))
        }), which = ii, force = TRUE, quoted = TRUE)
        uuid <- f[[1]]
        private$child_uuids <- c(private$child_uuids, uuid)
      })
      self$check_state(which = new_idx)

      # Basically init won't appear, we have ready, running (listening), suspend,
      if( previous_state == 'running' ){
        self$listen()
      }
    },

    shutdown = function( which = -1, force = FALSE ){
      if(!self$in_master_session() ){ return(FALSE) }
      n <- self$n_workers(deep = FALSE)
      if( n <= 0 ){ return(FALSE) }

      which <- self$get_active_idx( which )
      if(!length(which)){ return(TRUE) }
      if(!force){
        # TODO: implement
        cat2('force = FALSE is not implemented, force shut-down')
        force <- TRUE
      }

      if( force ){
        # terminated
        private$child_states[which] <- .async_evaluator_states[[6]]
        lapply(which, function(ii){
          tryCatch({
            parallel::stopCluster(private$cl[ii])
          }, error = function(e){
            self$logger2('Cluster ', ii , ' is closed.')
          })
          try({
            private$map$set(sprintf('CHILD_STATE_%d' , ii), .async_evaluator_states[[6]])
          }, silent = TRUE)
        })
        # Set again in case re-opened
        private$child_states[which] <- .async_evaluator_states[[6]]
      }
    },

    `@listen` = function(){
      if( private$state != 'running' ){
        return(invisible())
      }
      later::later(self$`@listen`, self$listen_interval)#, private$`@later_loop`)
      # TODO: Once create_loop(autorun=TRUE), then remove the following line as
      # they will be auto ran
      # later::later(function(){
      #   later::run_now(0, loop = private$`@later_loop`)
      # }, delay = self$listen_interval + 0.01)
      # Check queues
      n_queue <- private$queue0$count + private$queue1$count
      no_await <- FALSE
      if( n_queue > 0 ){
        # Let child sessions to work
        # Check how many readys
        self$check_state()
        self$fire_direct(rlang::quo({
          local({
            dipsaus <- asNamespace('dipsaus')
            dipsaus$run_child_evaluator()
          })
        }), run_one = FALSE, which = -1, quoted = TRUE, force = TRUE)
      }else{
        no_await <- TRUE
      }

      # Check feedbacks
      q2_count <- private$queue2$count
      if( q2_count > 0 ){
        self$logger2('Capture ', q2_count, ' results.')
        # get all results
        items <- private$queue2$pop( q2_count )
        private$finished <- private$finished + q2_count
        lapply(items, function(item){
          event_id <- item$message
          passed <- isTRUE(item$value$passed)
          if(isTRUE(is.character(event_id)) && length(event_id) == 1){
            on.exit({private$map_callback$remove(keys = event_id)}, add = TRUE)
            fb <- private$map_callback$get(key = event_id)
            tryCatch({
              if( passed && is.function(fb$success) ){
                fb$success( item$value$result )
              }else if( !passed && is.function(fb$failure) ){
                fb$failure( item$value$error )
              }
            }, error = function(e){
              self$logger2('Callback tunnel is corrupted. Did you just shutdown the async evaluator?')
            })
          }
        })
      }else{
        no_await <- TRUE
      }
      if( no_await ){
        pg <- self$progress()
        # Every thing is finished, no need to listen
        if( pg[[1]] <= pg[[4]] ){
          self$set_state('ready')
          self$check_state()
          self$logger2('Everything finished yay!')
        }else{
          # Evaluated, but somehow the result is no returned
          self$fire_direct(rlang::quo({
            local({
              dipsaus <- asNamespace('dipsaus')
              dipsaus$child_evaluator_singleton$check()
            })
          }), run_one = FALSE, which = -1, quoted = TRUE, force = TRUE)
        }
      }
    },
    listen = function(){
      if( private$state %in% c('ready', 'suspend') && self$in_master_session() ){
        self$set_state( 'running' )
        self$`@listen`()
        return(TRUE)
      }
      return(FALSE)
    },

    check_state = function( which = -1 ){
      # check children state from the map
      if( length(which) == 1 && which <= 0 ){
        which <- seq_along(private$cl)
      }else{
        which <-  which[which %in% seq_along(private$cl)]
      }
      if( !length(which) ){ return(0) }

      lapply(which, function(ii){
        s <- private$map$get(sprintf('CHILD_STATE_%d' , ii))
        if( length(s) ){
          private$child_states[[ii]] <- s
        }
      })
      return(length(which))
    },

    fire_direct = function(expr, quoted = FALSE, run_one = TRUE,
                           which = -1, force = FALSE){
      # get state
      self$check_state()
      if( !force ){
        which <- self$get_active_idx( which, exclude = .async_evaluator_states[-2] )
      }else{
        which <- self$get_active_idx( which, exclude = c('stopped', 'terminated') )
      }
      # Then there is no active cluster, no future is fired
      if( !length(which) ){ return( FALSE ) }

      if( run_one ){ which <- which[[1]] }
      if(!quoted){
        expr <- substitute(expr)
      }else if(rlang::is_quosure(expr)){
        expr <- rlang::quo_squash(expr)
      }

      do.call(parallel::clusterEvalQ, list(
        cl = quote(private$cl[which]),
        expr = expr
      ))
    },

    exec = function(expr, success = NULL, failure = NULL,
                    priority = 0, persist = FALSE,
                    quoted = FALSE, ..., .list = NULL){

      stopifnot2(
        self$n_workers(deep = FALSE, min_state = 'init', max_state = 'running') > 0,
        msg = 'No workers found. Please ramp up them.'
      )

      stopifnot2(priority %in% c(0,1), msg="priority must be either 0 (normal priority) or 1 (high priority)")
      if(!quoted){ expr <- rlang::enquo(expr) }

      event_id <- rand_string()
      addons <- c(list(...), .list)
      if(length(addons)){
        private$map$set(event_id, addons, signature = NULL)
      }
      queue_name <- paste0('queue', priority)

      private$map_callback$set(value = list(
        success = success,
        failure = failure
      ), key = event_id, signature = NULL)

      private[[queue_name]]$push(value = list(
        id = event_id,
        at_global = isTRUE(persist),
        expr = expr
      ), message = 'eval')

      private$total <- private$total + 1

      # Make sure the child process is running
      self$listen()

    },

    clean = function(){
      private$queue0$clean()
      private$queue1$clean()
      private$queue2$clean()
    },

    reset = function(){
      private$map_callback$reset()
      private$queue1$reset()
      private$queue0$reset()
      private$map$reset()
      private$queue2$reset()
      private$total <- private$finished
      pg <- self$progress()
      pg
    },

    progress = function(){
      total <- private$total
      await <- private$queue0$count + private$queue1$count

      finished <- private$finished + private$queue2$count
      running <- total - await - finished
      if( running < 0 ){
        total <- total - running
        running <- 0
        private$total <- total
      }

      # idx = self$n_workers(return_index = TRUE)
      # if(length(idx)){
      #   self$check_state(idx)
      #   running = sum(unlist(private$map$mget(sprintf('PROCESSING_%d', idx), missing_default = 0)))
      # }else{
      #   running = 0
      # }


      c(total, running, await, finished)
    }

  ),
  active = list(
    is_master = function(){
      return(TRUE)
    },
    valid = function(){
      private$state < 'terminated'
    },
    file_path = function(){
      private$path
    },
    id = function(){
      private$uuid
    }
  )
)












.async_master_globals <- local({
  # name - instance pair
  instances <- list()

  # Path - name pair
  paths <- list()

  get <- function(name){
    instances[[name]]
  }

  new <- function(name, path, n_nodes = max(floor(future::availableCores()/2), 1),
                  n_subnodes = 1, verbose = TRUE, ...){

    if( !is.null(instances[[name]]) ){
      if( verbose ){
        cat2('Not re-initializing as an old instance already exists.')
      }
      return( FALSE )
    }

    if(dir.exists(path)){
      path <- normalizePath(path)
      nm <- paths[[path]]
      if( !is.null(nm) ){
        stopifnot2(nm == name, msg = sprintf(
          'An instance %s  already exists at current path.',  sQuote(nm)
        ))
        if( verbose ){
          cat2('Not re-initializing as an old instance already exists.')
        }
        return( FALSE )
      }
    }
    if( file.exists(path) ){
      # check if this is a directory and no files
      stopifnot2(dir.exists(path),
                 msg = 'path already exists and is not a directory. Need a new path or empty directory.')
      stopifnot2(list.files(path) == 0,
                 msg = 'path already exists and is not empty. Need a new path or empty directory.')
    }

    path <- dir_create(path)
    paths[[path]] <<- name
    self <- MasterEvaluator$new(path, ...)
    instances[[name]] <<- self

    # Ramp-up
    self$ramp_up(n_nodes, subs = n_subnodes)

    return( TRUE )
  }

  terminate <- function(name){
    instance <- instances[[name]]
    if(!is.null(instance)){
      instance$finalize()

      idx <- vapply(paths, function(nm){ nm == name }, FALSE)
      if( any(idx) ){
        idx <- which(idx)[[1]]
        paths[[idx]] <<- NULL
        instances[[name]] <<- NULL
      }

    }
    invisible()
  }

  stop <- function(name){
    self <- instances[[name]]
    if(!is.null(self)){
      self$reset()
      return(invisible(TRUE))
    }
    return(invisible(FALSE))
  }
  suspend <- function(name){
    self <- instances[[name]]
    if(!is.null(self)){
      self$set_state('suspend')
      return(invisible(TRUE))
    }
    return(invisible(FALSE))
  }

  run <- function(name, expr, success = NULL, failure = NULL,
                  priority = 0, persist = FALSE, quoted = FALSE, ..., .list = NULL){
    instance <- instances[[name]]
    if(!is.null(instance)){
      if( !quoted ){
        expr <- rlang::enquo(expr)
      }
      instance$exec(expr = expr,success = success, failure = failure,
                priority = priority, persist = persist,
                quoted = TRUE, ..., .list = .list)
      return(invisible(TRUE))
    }
    return(invisible(FALSE))
  }

  scale_down <- function(name, n_nodes, n_subnodes = 1){
    n_nodes <- floor(n_nodes)
    n_subnodes <- max(1, floor(n_subnodes))
    stopifnot2(isTRUE(n_nodes >= 1),
               msg = 'Cannot trim to less than 1 cores. Use asynceval_shutdown to shut down the evaluator')

    self <- instances[[name]]
    if(!is.null(self)){
      # Ramp-up
      idx <- self$n_workers(deep = TRUE, min_state = 'init', return_index = TRUE)
      if( length(idx) > n_nodes ){
        self$shutdown(which = idx[-seq_len(n_nodes)])
        idx <- idx[seq_len(n_nodes)]
      }
      file_path <- self$file_path
      master_uuid <- self$id

      lapply(idx, function(ii){
        self$fire_direct(rlang::quo({
          local({
            dipsaus <- asNamespace('dipsaus')
            dipsaus$launch_child_evaluator(!!file_path, !!ii, !!master_uuid, !!n_subnodes)
          }, envir = new.env(parent = globalenv()))
        }), which = ii, force = TRUE, quoted = TRUE)
      })

      return(n_nodes)
    }
    return(0)
  }

  scale_up <- function(name, n_nodes, n_subnodes = 1,
                       create_if_missing = FALSE, path = tempfile()){
    self <- instances[[name]]
    if( is.null(self) ){
      if( create_if_missing ){
        new(name, path, n_nodes = n_nodes, n_subnodes = n_subnodes)
        return(n_nodes)
      }else{
        return(0)
      }
    }

    idx <- self$n_workers(deep = TRUE, min_state = 'init', return_index = TRUE)
    file_path <- self$file_path
    master_uuid <- self$id

    lapply(idx, function(ii){
      self$fire_direct(rlang::quo({
        local({
          dipsaus <- asNamespace('dipsaus')
          dipsaus$launch_child_evaluator(!!file_path, !!ii, !!master_uuid, !!n_subnodes)
        }, envir = new.env(parent = globalenv()))
      }), which = ii, force = TRUE, quoted = TRUE)
    })

    self$ramp_up(n = n_nodes, subs = n_subnodes)
    n_nodes

  }

  workers <- function(name, ...){
    self <- instances[[name]]
    if(is.null(self)){ return(0) }
    self$n_workers(...)
  }

  progress <- function(name){
    self <- instances[[name]]
    if(is.null(self)){ return(c(0,0,0,0)) }
    self$progress()
  }

  return(list(
    new = new,
    run = run,
    terminate = terminate,
    scale_down = scale_down,
    scale_up = scale_up,
    workers = workers,
    progress = progress,
    get = get
  ))

})









#' Create Asynchronous Evaluator to Queue Tasks
#' @description
#' Asynchronous evaluator aims at queuing R evaluations from sub-processes
#' without blocking the main session. It's based on \code{'parallel'} and
#' \code{'future'} packages.
#' @param name unique name for the evaluator
#' @param path blank directory for evaluator to store data
#' @param n_nodes number of control nodes, default is 1
#' @param n_subnodes number of sub-sessions for each control node, default is
#' the number of CPU cores minus 1
#' @param verbose for internal debug use
#' @param ... passed to the constructor of \code{\link[dipsaus]{MasterEvaluator}}
#' @details
#' \code{'parallel'} blocks the main session when evaluating expressions.
#' \code{'future'} blocks the main session when the number of running futures
#' exceed the maximum number of workers. (For example if 4 workers are planned,
#' then running 5 future instances at the same time will freeze the session).
#'
#' Asynchronous evaluator is designed to queue any number of R expressions
#' without blocking the main session. The incoming expressions are stored in
#' \code{\link[dipsaus]{AbstractQueue}} instances, and main session monitors
#' the queue and is charge of notifying child sessions to evaluate these
#' expressions whenever available.
#'
#' Important: Asynchronous evaluator is not designed for super high-performance
#' computing. The internal scheduler schedules \code{n_nodes} evaluations for
#' every 1 second. Therefore if each of the process can be finished within
#' \code{1 / n_nodes} seconds, then use `future` instead.
#'
#' @section Value:
#'
#' A list of functions to control the evaluator:
#'
#' \describe{
#'
#' \item{\code{run(expr, success = NULL, failure = NULL, priority = 0,
#' persist = FALSE, quoted = FALSE, ..., .list = NULL)}}{
#' Queue and run an R expression.
#'
#' \describe{
#'
#' \item{\code{expr}}{
#'  can be anything except for
#' \code{q()}, which terminates the session. \code{'rlang'}
#' \code{\link[rlang]{nse-force}} is also supported. For example, you
#' can use \code{`!!`} to quasi-quote the expression and unquote the values.
#' }
#'
#' \item{\code{..., .list}}{
#' provides additional data for \code{expr}. For example,
#' \code{expr} uses a large data object \code{dat} in the main session, which
#' might not be available to the child sessions. Also because the object
#' is large, quasi-quotation could be slow or fail. By passing \code{dat=...}
#' or \code{.list=list(dat=...)}, it's able to temporary store the data on
#' hard-drive and persist for evaluators. The back-end is using
#' \code{\link{qs_map}}, which is super fast for data that are no
#' more than \code{2GB}.
#' }
#'
#' \item{\code{success} and \code{failure}}{
#' functions to handle the results once
#' the evaluator returns the value. Since it's almost impossible to know
#' when the evaluator returns values, it's recommended that these functions
#' to be simple.
#' }
#'
#' \item{\code{priority}}{
#' puts the priority of the expression. It can only be `0` or
#' `1`. Evaluators will run expressions with priority equal to 1 first.
#' }
#'
#' \item{\code{persist}}{
#' indicates whether to run the expression and persist
#' intermediate variables.
#' }
#' }
#' }
#'
#' \item{\code{terminate()}}{
#' Shut down and release all the resource.
#' }
#'
#' \item{\code{scale_down(n_nodes, n_subnodes = 1)},
#'       \code{scale_up(n_nodes, n_subnodes = 1,
#'       create_if_missing = FALSE, path = tempfile())}}{
#' Scale down or up the evaluator.
#' \describe{
#' \item{\code{n_nodes} and \code{n_subnodes}}{ see 'usage' }
#' \item{\code{create_if_missing}}{
#' If the evaluator was previously terminated or shutdown, setting this to be
#' true ignores the `invalid` flags and re-initialize the evaluator
#' }
#' \item{\code{path}}{
#' If \code{create_if_missing} is true, then \code{path} will be passed to
#' the constructor of \code{\link[dipsaus]{MasterEvaluator}}. See 'usage'.
#' }
#' }
#' }
#'
#'
#' \item{\code{workers(...)}}{
#' Returns number of workers available in the evaluator. \code{`...`} is for
#' debug use
#' }
#'
#' \item{\code{progress()}}{
#' Returns a vector of 4 integers. They are:
#' \enumerate{
#' \item{ The total number evaluations. }
#' \item{ Number of running evaluations. }
#' \item{ Number of awaiting evaluations. }
#' \item{ Number of finished evaluations. }
#' }
#' }
#'
#' }
#'
#'
#' @export
make_async_evaluator <- function(
  name, path = tempfile(), n_nodes = 1,
  n_subnodes = future::availableCores() - 1, verbose = FALSE, ...){

  .async_eval_name <- name

  .async_master_globals$new(name = .async_eval_name, path = path,
                            n_nodes = n_nodes, n_subnodes = n_subnodes,
                            verbose = verbose, ...)



  return(list(
    get_instance = function(){
      .async_master_globals$get(name = .async_eval_name)
    },
    run = function(expr, success = NULL, failure = NULL,
                   priority = 0, persist = FALSE, quoted = FALSE, ...,
                   .list = NULL){
      if( !quoted ){
        expr <- rlang::enquo(expr)
      }
      .async_master_globals$run(
        name = .async_eval_name, expr = expr, success = success,
        failure = failure, priority = priority, persist = persist,
        quoted = TRUE, ..., .list = .list
      )
    },
    terminate = function(){
      .async_master_globals$terminate(name = .async_eval_name)
    },
    scale_down = function(n_nodes, n_subnodes = 1){
      .async_master_globals$scale_down(name = .async_eval_name,
                                       n_nodes = n_nodes,
                                       n_subnodes = n_subnodes)
    },
    scale_up = function(n_nodes, n_subnodes = 1,
                        create_if_missing = FALSE, path = tempfile()){
      .async_master_globals$scale_up(name = .async_eval_name,
                                     n_nodes = n_nodes,
                                     n_subnodes = n_subnodes,
                                     create_if_missing = create_if_missing,
                                     path = path)
    },
    workers = function(...){
      .async_master_globals$workers(name = .async_eval_name)
    },
    progress = function(){
      .async_master_globals$progress(name = .async_eval_name)
    },
    stop = function(){
      .async_master_globals$stop(name = .async_eval_name)
    },
    suspend = function(){
      .async_master_globals$suspend(name = .async_eval_name)
    }
  ))

}
