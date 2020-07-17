# String manupulation

#' Convert file to 'base64' format
#' @param file file path
#' @param mime 'mime' type, default is blank
#' @return a 'base64' data string looks like \code{'data:;base64,AEF6986...'}
#' @export
to_datauri <- function(file, mime = ''){
  info <- file.info(file)
  ss <- jsonlite::base64_enc(input = readBin(file, what = 'raw', n = info$size))
  ss <- sprintf('data:%s;base64,%s', mime, ss)
  ss
}


#' Convert color to Hex string
#' @param col character or integer indicating color
#' @param alpha \code{NULL} or numeric, transparency. See \code{grDevices::rgb}
#' @param prefix character, default is \code{"#"}
#' @param ... passing to  \code{\link[grDevices]{adjustcolor}}
#' @return characters containing the hex value of each color. See details
#' @details \code{col2hexStr} converts colors such as 1, 2, 3, "red", "blue", ... into
#' hex strings that can be easily recognized by `HTML`, `CSS` and `JavaScript`.
#' Internally this function uses \code{\link[grDevices]{adjustcolor}} with two differences:
#' \enumerate{
#' \item the returned hex string does not contain alpha value if \code{alpha} is \code{NULL};
#' \item the leading prefix "#" can be customized
#' }
#' @seealso  \code{\link[grDevices]{adjustcolor}}
#' @examples
#'
#' col2hexStr(1, prefix = '0x')      # "0x000000"
#' col2hexStr('blue')                # "#0000FF"
#'
#' # Change default palette, see "grDevices::colors()"
#' grDevices::palette(c('orange3', 'skyblue1'))
#' col2hexStr(1)                     # Instead of #000000, #CD8500
#'
#' @export
col2hexStr <- function(col, alpha = NULL, prefix = '#', ...){
  if(is.null(alpha)){
    alpha <- 1
    transparent <- FALSE
  }else{
    transparent <- TRUE
  }
  re <- grDevices::adjustcolor(col, alpha.f = alpha)
  if(!transparent){
    re <- stringr::str_sub(re, end = 7L)
  }
  stringr::str_replace(re, '^[^0-9A-F]*', prefix)
}


#' @title Parse Text Into Numeric Vectors
#'
#' (stable)
#'
#' @param text string with chunks, e.g. \code{"1-10, 14, 16-20, 18-30"} has 4 chunks
#' @param sep default is ",", character used to separate chunks
#' @param connect characters defining connection links for example "1:10" is the same as "1-10"
#' @param sort sort the result
#' @param unique extract unique elements
#' @return a numeric vector. For example, "1-3" returns \code{c(1, 2, 3)}
#' @seealso \code{\link[dipsaus]{deparse_svec}}
#' @examples
#' parse_svec('1-10, 13:15,14-20')
#' @export
parse_svec <- function(text, sep = ',', connect = '-:|', sort = FALSE, unique = TRUE){
  connect <- unlist(stringr::str_split(connect, ''))
  connect[connect %in% c('|', ':')] <- paste0('\\', connect[connect %in% c('|', ':')])
  connect <- paste(connect, collapse = '')


  if(length(text) == 0 || stringr::str_trim(text) == ''){
    return(NULL)
  }

  if(is.numeric(text)){
    return(text)
  }
  s <- as.vector(stringr::str_split(text, sep, simplify = TRUE))
  s <- stringr::str_trim(s)
  s <- s[s!='']

  s <- s[stringr::str_detect(s, sprintf('^[0-9\\ %s]+$', connect))]

  re <- NULL
  for(ss in s){
    if(stringr::str_detect(ss, sprintf('[%s]', connect))){
      ss <- as.vector(stringr::str_split(ss, sprintf('[%s]', connect), simplify = TRUE))
      ss <- ss[stringr::str_detect(ss, '^[0-9]+$')]
      ss <- as.numeric(ss)
      if(length(ss) >= 2){
        re <- c(re, (ss[1]:ss[2]))
      }
    }else{
      re <- c(re, as.numeric(ss))
    }
  }

  if(unique){
    re <- unique(re)
  }

  if(sort){
    re <- sort(re)
  }

  return(re)
}

#' @title Convert Integer Vectors To String
#'
#' (stable)
#'
#' @param nums integer vector
#' @param connect character used to connect consecutive numbers
#' @param concatenate connect strings if there are multiples
#' @param collapse if concatenate, character used to connect strings
#' @param max_lag defines "consecutive", min = 1
#' @return strings representing the input vector. For example, \code{c(1, 2, 3)} returns "1-3".
#' @seealso \code{\link[dipsaus]{parse_svec}}
#' @examples
#' deparse_svec(c(1:10, 15:18))
#' @export
deparse_svec <- function(nums, connect = '-', concatenate = TRUE, collapse = ',', max_lag = 1){
  nums <- nums[is.finite(nums)]
  if(length(nums) == 0){
    return('')
  }
  alag <- seq_len(max(1, max_lag))
  nums <- sort(unique(nums))
  lg <- c(NA, nums)[seq_len(length(nums))]
  ind <- nums - lg
  ind[1] <- 0
  ind2 <- c(ind[-1], -1)
  apply(cbind(nums[!ind %in% alag], nums[!ind2 %in% alag]), 1,function(x){
    if(x[1] == x[2]){
      stringr::str_c(x[1])
    }else{
      stringr::str_c(x, collapse = connect)
    }
  }) ->
    re
  if(concatenate){
    re <- stringr::str_c(re, collapse = collapse)
  }
  re
}




#' @title Color Output
#' @param ... to be printed
#' @param level 'DEBUG', 'INFO', 'WARNING', 'ERROR', or 'FATAL' (total 5 levels)
#' @param print_level if true, prepend levels before messages
#' @param file,sep,fill,labels,append pass to \code{base::cat}
#' @param pal a named list defining colors see details
#' @param end character to append to the string
#' @param use_cli logical, whether to use package 'cli'
#' @param bullet character, if use 'cli', which symbol to show. see
#' \code{\link[cli]{symbol}}
#'
#' @return none.
#'
#' @details
#' There are five levels of colors by default: 'DEBUG', 'INFO', 'WARNING', 'ERROR',
#' or FATAL. Default colors are: 'DEBUG' (\code{grey60}), 'INFO' (\code{#1d9f34}), 'WARNING'
#' (\code{#ec942c}), 'ERROR' (\code{#f02c2c}), 'FATAL' (\code{#763053}) and
#' 'DEFAULT' (\code{#000000}, black). If level is not in preset five levels,
#' the color will be "default"-black color.
#' @export
cat2 <- function(
  ..., level = 'DEBUG', print_level = FALSE,
  file = "", sep = " ", fill = FALSE, labels = NULL,
  append = FALSE, end = '\n', pal = list(
    'DEBUG' = 'grey60',
    'INFO' = '#1d9f34',
    'WARNING' = '#ec942c',
    'ERROR' = '#f02c2c',
    'FATAL' = '#763053',
    'DEFAULT' = 'grey60'
  ), use_cli = TRUE, bullet = 'auto'
){
  if(!level %in% names(pal)){
    level <- 'DEFAULT'
  }
  bullet_list <- list(
    'DEBUG' = 'tick',
    'INFO' = 'heart',
    'WARNING' = 'warning',
    'ERROR' = 'cross',
    'FATAL' = 'cross',
    'DEFAULT' = 'arrow_right'
  )

  .col <- pal[[level]]
  if(is.null(.col)){ .col <- '#000000' }
  if( bullet == 'auto' ){
    bullet <- bullet_list[[level]]
    if(is.null(bullet)){ bullet <- 'arrow_right' }
  }

  # check if interactive
  if(interactive()){

    if( use_cli ){

      cli::cat_bullet(..., col = .col, bullet = bullet)
    }else{
      # use colored console
      col <- cli::make_ansi_style(.col)
      if(print_level){
        base::cat('[', level, ']: ', sep = '')
      }
      base::cat(col(..., sep = sep), end = end, file = file, fill = fill,
                labels = labels, append = append)
    }

    # Update: not print anything
  }else{
    # Whether running in the future session
    base::cat(..., end)
  }

  if(level == 'FATAL'){
    # stop!
    err <- simpleError(paste(..., collapse = '', sep = ''), call = NULL)
    trace <- rlang::trace_back()
    trace <- as.list(utils::capture.output(print(trace)))
    if(length(trace) > 1){
      trace <- c('Full call trees:', trace)
      err$trace <- trace
      class(err) <- c('dipsausError', "simpleError", "error", "condition")
    }

    # logger(sprintf('[FATAL] %s', utils::capture.output(traceback(trace))), flush = TRUE)

    stop(err)
  # }else{
  #   logger(sprintf('%s %s', level, paste(..., collapse = '', sep = '')), flush = FALSE)
  }

  invisible()
}



#' Convert bytes to KB, MB, GB,...
#'
#' @param s size
#' @param kb_to_b how many bytes counts one KB, 1000 by default
#' @return numeric equaling to \code{s} but formatted
#' @export
to_ram_size <- function(s, kb_to_b = 1000){
  base <- floor(log(max(abs(s), 1), kb_to_b))
  s <- s / (kb_to_b ^ (base))
  if(is.na(base)){
    base <- 0
  }
  attr(s, 'unit') <- c('B', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB')[base+1]
  class(s) <- c('dipsaus_bytes', class(s))
  s
}


#' @export
as.character.dipsaus_bytes <- function(x, digit=1, ...){
  sprintf(sprintf('%%.%df %s', digit, attr(x, 'unit')), x)
}

#' @export
print.dipsaus_bytes <- function(x, digit=1, ...){
  re <- as.character(x, digit = digit, ...)
  cat(re)
  invisible(re)
}

#' Get max RAM size
#' This is an experimental function that is designed for non-windows systems
#' @return a list of total free memory.
#' @export
mem_limit2 <- function(){

  total <- get_ram()


  bit <- 8 * .Machine$sizeof.pointer
  bit <- bit - (bit > 32) * 4 - 4
  free <- sum(gc()[,1] * c(bit, 8))

  list(
    total = total,
    free = total - free
  )


}


#' @title Ask and Return True or False from the Terminal
#' @description Ask a question and read from the terminal in interactive scenario
#' @param ...,end,level passed to \code{\link[dipsaus]{cat2}}
#' @param error_if_canceled raise error if canceled
#' @param use_rs whether to use \code{rstudioapi} if possible
#' @param ok button label for yes
#' @param cancel button label for no
#' @param rs_title message title if 'RStudio' question box pops up.
#' @seealso \code{\link[dipsaus]{cat2}}, \code{\link[base]{readline}},
#' \code{\link[dipsaus]{ask_or_default}}
#'
#' @details The prompt string will ask for an yes or no question. Users need to
#' enter "y", "yes" for yes, "n", "no" or no, and "c" for cancel
#' (case-insensitive).
#'
#' This can only be used in an \code{\link{interactive}} session.
#'
#' @return logical or \code{NULL} or raise an error. If "yes" is entered,
#' returns \code{TRUE}; if "no" is entered, returns \code{FALSE}; if "c" is
#' entered, \code{error_if_canceled=TRUE} will result in an error, otherwise
#' return \code{NULL}
#'
#' @examples
#' if(interactive()){
#' ask_yesno('Do you know how hard it is to submit an R package and ',
#'           'pass the CRAN checks?')
#' ask_yesno('Can I pass the CRAN check this time?')
#' }
#' @export
ask_yesno <- function(..., end = '', level = 'INFO', error_if_canceled = TRUE,
                      use_rs = TRUE, ok = 'Yes', cancel = 'No',
                      rs_title = 'Yes or No:'){

  if(use_rs && rs_avail()){
    s <- paste(..., sep = '\n')
    res <- rstudioapi::showQuestion(rs_title, s, ok = ok, cancel = cancel)
    return(isTRUE(res))
  } else {
    cat2(..., ' (Yes/no): ', end = end, level = level)
    answer <- readline()
    answer <- stringr::str_trim(stringr::str_to_upper(answer))
    if( answer %in% c('Y', 'YES') ){ return(TRUE) }
    if( answer %in% c('N', 'NO') ){ return(FALSE) }
    if( answer %in% c('C') ){
      if( error_if_canceled ){
        cat2('Canceled.', level = 'FATAL')
      }else{
        return(NULL)
      }
    }
    Recall('Please answer Y/yes, N/no, or c to cancel.', end = '', level = 'WARNING', error_if_canceled = error_if_canceled)
  }


}


#' @title Read a Line from the Terminal, but with Default Values
#' @description Ask a question and read from the terminal in interactive scenario
#' @param ...,end,level passed to \code{\link[dipsaus]{cat2}}
#' @param default default value to return in case of blank input
#' @seealso \code{\link[dipsaus]{cat2}}, \code{\link[base]{readline}},
#' \code{\link[dipsaus]{ask_yesno}}
#'
#' @details The prompt string will ask a question, providing defaults. Users
#' need to enter the answer. If the answer is blank (no space), then returns the
#' default, otherwise returns the user input.
#'
#' This can only be used in an \code{\link{interactive}} session.
#'
#' @return A character from the user's input, or the default value. See details.
#'
#' @examples
#' if(interactive()){
#' ask_or_default('What is the best programming language?',
#'                default = 'PHP')
#' }
#' @export
ask_or_default <- function(..., default = '', end = '', level = 'INFO'){

  if(rs_avail()){
    answer <- rstudioapi::showPrompt('Question', paste(..., sep = '\n'), default = default)
    if(!length(answer) || answer == ''){
      answer <- default
    }
  } else {
    cat2(..., sprintf('\n  [default is %s] ', sQuote(default)),
         end = end, level = level)
    answer <- stringr::str_trim(readline())
    if( answer == '' ){
      answer <- default
    }
  }
  answer

}




#' Print Directory Tree
#' @param target target directory path, relative to \code{root}
#' @param root root directory, default is \code{'~'}
#' @param child child files in target; is missing, then list all files
#' @param dir_only whether to display directory children only
#' @param collapse whether to concatenate results as one single string
#' @param ... pass to \code{\link[base]{list.files}} when list all files
#' @return Characters, print-friendly directory tree.
#' @export
print_directory_tree <- function(target, root = '~', child, dir_only = FALSE,
                                 collapse = NULL, ...){
  root <- normalizePath(root, winslash = '/', mustWork = FALSE)
  target <- file.path(root, target)
  target <- stringr::str_replace_all(target, '\\\\', '/')
  target <- normalizePath(target, mustWork = FALSE, winslash = '/')

  paths <- stringr::str_split(target, '\\\\|/', simplify = TRUE)
  rpath <- stringr::str_split(root, '\\\\|/', simplify = TRUE)

  tree_id <- cbind(paste(rpath, collapse = '/'), paths[, -seq_along(rpath)])

  df <- list('...' = character(0))

  for(i in seq_len(nrow(tree_id))){

    if(i == 1){
      if( missing(child) ){
        # child is only for the first target
        dir <- target[[i]]
        if( dir.exists(dir) ){
          child <- list.dirs(dir, full.names = FALSE, recursive = FALSE)
          if(!dir_only){
            child <- c(child, list.files(dir, full.names = FALSE, include.dirs = FALSE, ...))
          }
          df[child] <- lapply(child, function(o){ character(0) })
        } else {
          child <- '...'
        }
      } else if(!length(child)){
        child <- character(0)
      } else {
        df[child] <- lapply(child, function(o){ character(0) })
      }
    } else {
      child <- '...'
    }

    x <- c(as.list(tree_id[i, ]), list(child), list(''))
    Reduce(function(a,b){
      if(a != '' && length(a) == 1){
        df[[a]] <<- c(df[[a]], b)
      }
      b
    }, x)
  }

  res <- cli::tree(data.frame(names(df), I(unname(lapply(df, function(x){
    x <- x[x!='']
    if(!length(x)){
      x <- character(0)
    }else {
      x <- unique(x)
    }
    x
  })))), root = root)
  if(length(collapse) == 1){
    res <- paste(res, collapse = collapse)
  }
  res
}

#' Captures Evaluation Output of Expressions as One Single String
#' @description Evaluate expression and captures output as characters, then
#' concatenate as one single string.
#' @param expr R expression
#' @param collapse character to concatenate outputs
#' @param type passed to \code{\link[utils]{capture.output}}
#' @return Character of length 1: output captured by
#' \code{\link[utils]{capture.output}}
#' @export
capture_expr <- function(expr, collapse = '\n', type = c("output", "message")){
  invisible(paste(utils::capture.output(expr, type = type),
                  collapse = collapse))
}
