# String manupulation

#' Convert file to 'base64' format
#' @param file file path
#' @param mime 'mime' type, default is blank
#' @return a 'base64' data string looks like \code{'data:;base64,AEF6986...'}
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
parse_svec <- function(text, sep = ',', connect = '-:|', sort = F, unique = T){
  connect <- unlist(stringr::str_split(connect, ''))
  connect[connect %in% c('|', ':')] <- paste0('\\', connect[connect %in% c('|', ':')])
  connect <- paste(connect, collapse = '')


  if(length(text) == 0 || stringr::str_trim(text) == ''){
    return(NULL)
  }

  if(is.numeric(text)){
    return(text)
  }
  s <- as.vector(stringr::str_split(text, sep, simplify = T))
  s <- stringr::str_trim(s)
  s <- s[s!='']

  s <- s[stringr::str_detect(s, sprintf('^[0-9\\ %s]+$', connect))]

  re <- NULL
  for(ss in s){
    if(stringr::str_detect(ss, sprintf('[%s]', connect))){
      ss <- as.vector(stringr::str_split(ss, sprintf('[%s]', connect), simplify = T))
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
deparse_svec <- function(nums, connect = '-', concatenate = T, collapse = ',', max_lag = 1){
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
  bullet_list = list(
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
    bullet = bullet_list[[level]]
    if(is.null(bullet)){ bullet <- 'arrow_right' }
  }

  # check if interactive
  if(interactive()){

    if( use_cli ){

      cli::cat_bullet(..., col = .col, bullet = bullet)
    }else{
      # use colored console
      col <- crayon::make_style(.col)
      if(print_level){
        base::cat('[', level, ']: ', sep = '')
      }
      base::cat(col(..., sep = sep), end = end, file = file, fill = fill,
                labels = labels, append = append)
    }

  }else{
    # Just use cat
    base::cat(...)
  }

  if(level == 'FATAL'){
    # stop!
    stop(call. = FALSE)
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


