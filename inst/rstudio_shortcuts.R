# <<<<< dipsaus start

#' Dipsaus right-null checked assign
#'
#' @interactive
#' @shortcut Alt+/
.dipsaus_right_null <- function () {
  cts <- rstudioapi::getActiveDocumentContext()

  start <- cts$selection[[1]]$range$start
  end <- cts$selection[[1]]$range$end
  cts$selection[[1]]$text
  s <- cts$contents[start[[1]] : end[[1]]]

  pre <- stringr::str_sub(s[[1]], end = max(start[[2]] - 1, 0))
  pos <- stringr::str_sub(s[[length(s)]], start = end[[2]])
  pre <- stringr::str_remove(pre, "[ ]+$")
  pos <- stringr::str_remove(pos, "^[ ]+")
  s1 <- NULL
  if(start[[1]] > 1){
    s1 <- cts$contents[1:(start[[1]] - 1)]
  }
  s2 <- cts$contents[-(1:end[[1]])]
  s <- sprintf('%s %%?<-%% %s', pre, pos)
  cts$contents <- c(s1, s, s2)

  rstudioapi::setDocumentContents(id = cts$id, text = paste(cts$contents, collapse = '\n'))
  start[[2]] <- start[[2]] + 7
  rstudioapi::setCursorPosition(start, id = cts$id)

  invisible()
}



#' Dipsaus left-null checked assign
#'
#' @interactive
#' @shortcut Alt+Shift+/
.dipsaus_left_null <- function () {
  cts <- rstudioapi::getActiveDocumentContext()

  start <- cts$selection[[1]]$range$start
  end <- cts$selection[[1]]$range$end
  cts$selection[[1]]$text
  s <- cts$contents[start[[1]] : end[[1]]]

  pre <- stringr::str_sub(s[[1]], end = max(start[[2]] - 1, 0))
  pos <- stringr::str_sub(s[[length(s)]], start = end[[2]])
  pre <- stringr::str_remove(pre, "[ ]+$")
  pos <- stringr::str_remove(pos, "^[ ]+")
  s1 <- NULL
  if(start[[1]] > 1){
    s1 <- cts$contents[1:(start[[1]] - 1)]
  }
  s2 <- cts$contents[-(1:end[[1]])]
  s <- sprintf('%s %%<-?%% %s', pre, pos)
  cts$contents <- c(s1, s, s2)

  rstudioapi::setDocumentContents(id = cts$id, text = paste(cts$contents, collapse = '\n'))
  start[[2]] <- start[[2]] + 7
  rstudioapi::setCursorPosition(start, id = cts$id)
  invisible()
}

# <<<<< dipsaus end
