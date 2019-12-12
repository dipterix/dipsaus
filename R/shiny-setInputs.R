
#' @export
set_shiny_input <- function(
  session, inputId, value, priority = c('event', 'deferred', 'immediate'),
  method = c('proxy', 'serialize', 'value', 'expression'), quoted = TRUE){

  priority <- match.arg(priority)
  method <- match.arg(method)
  inputId <- session$ns(inputId)
  proxy <- method

  switch (method,
    'serialize' = {
      raw <- qs::qserialize(value, preset = 'fast', algorithm = 'lz4')
      raw <- base64enc::base64encode(raw)
    },
    'value' = {
      raw <- jsonlite::toJSON(value, auto_unbox = TRUE, digits = 22)
      raw <- base64url::base64_urlencode(raw)
    },
    'expression' = {
      if(!quoted){
        value = substitute(value)
      }
      raw <- paste(deparse(value), collapse = '\n')
      raw <- jsonlite::toJSON(raw, auto_unbox = TRUE)
      raw <- base64url::base64_urlencode(raw)
    },
    'proxy' = {
      session$userData$dipsaus_reserved %?<-% new.env(parent = emptyenv())
      session$userData$dipsaus_reserved$proxy_data %?<-% new.env(parent = emptyenv())
      session$userData$dipsaus_reserved$proxy_data[[inputId]] = value
      raw <- inputId
    }
  )

  session$sendCustomMessage('dipsaus-set-input', list(
    inputId = inputId,
    proxy = proxy,
    value = raw,
    priority = priority
  ))
}

registerSetInputs <- function(){
  # register input
  shiny::registerInputHandler("dipsaus_asis", function(data, session, name) {

    if(!is.list(data)){
      return(NULL)
    }
    proxy = match.arg(data$proxy, c('serialize', 'value', 'expression', 'proxy'))

    raw <- data$value
    switch (
      proxy,

      'serialize' = {
        qs::qdeserialize(base64enc::base64decode(raw))
      },

      'value' = {
        jsonlite::fromJSON(base64url::base64_urldecode(raw))
      },

      'expression' = {
        raw <- jsonlite::fromJSON(base64url::base64_urldecode(raw))
        str2lang(raw)
      },

      'proxy' = {
        if(is.environment(session$userData$dipsaus_reserved$proxy_data)){
          re <- session$userData$dipsaus_reserved$proxy_data[[raw]]
          rm(list = raw, envir = session$userData$dipsaus_reserved$proxy_data)
          re
        }else{
          NULL
        }

      },

      {
        cat2('set_shiny_input method must be from ',
             sQuote('serialize'), ', ', sQuote('value'),
             level = 'FATAL')
      }
    )

  }, force = TRUE)
}

