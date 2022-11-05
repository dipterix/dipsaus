#' @title Generate a random password
#' @description Please note that this function is not meant to be used in
#' production. It is not meant to be used for highly secured
#' cryptographic purposes.
#' @param master_password a master password that only you know, should have
#' at least 8 characters
#' @param method whether to query token map, or to create the password,
#' choices are \code{'get_or_create'} (default), \code{'replace'},
#' \code{'query'}; see 'Details'
#' @param service service name, must only contains letters, digits, equal sign,
#' underscore, comma, dot, dash
#' @param special_chr special characters allowed in the password
#' @param tokenfile a file containing all the tokens. Warning: if you lose the
#' token book, it is hard (not impossible, but impractical) to restore
#' the passwords
#' @param verbose whether to print out service names; default is false
#' @return If method is \code{'query'}, returns token map; otherwise returns
#' the password itself
#'
#' @seealso \code{\link[digest]{digest}}
#'
#' @details Please note that this function is not meant to be used in
#' production or anything that requires high security level. This is most
#' likely for my personal use since I am tired of
#' storing the passwords on the cloud or having to buy the services.
#'
#' The encryption adopts \code{'sha256'} algorithm provided by
#' \code{\link[digest]{digest}} function. To restore a password,
#' you will need twp components: \code{master_password}, a token book (
#' \code{tokenfile}). If any of them is missing, then the password is lost.
#' Please store the token book properly (for example, in 'Dropbox' vault).
#'
#' The token book could be shared. Anyone who do not have master password will
#' be unlikely to restore the service password. Do not share the master password
#' with anyone other than yourself.
#'
#' By default, \code{method='get_or_create'} will try to retrieve existing
#' tokens to generate password. If the token is missing, then a new token
#' will be generated. The \code{method='replace'} will ignore existing tokens
#' and directly create a new one.
#'
#' @examples
#'
#' tokenfile <- tempfile()
#'
#' # ---------- Create a password and store the tokens to token book ------
#' pass1 <- get_credential(
#'   master_password = "my password",
#'   service = "google.com:my_username",
#'   special_chr = "@#$%^&*",
#'   tokenfile = tokenfile
#' )
#' print(pass1)
#'
#' # ---------- Query existing tokens ------
#' token_params <- get_credential(
#'   method = "query",
#'   tokenfile = tokenfile,
#'   verbose = TRUE
#' )
#'
#' print(token_params)
#'
#' # ---------- retrieve stored password ----------
#' pass2 <- get_credential(
#'   master_password = "my password",
#'   service = "google.com",
#'   tokenfile = tokenfile
#' )
#' identical(pass1, pass2)
#'
#' # Using wrong master password
#' pass3 <- get_credential(
#'   master_password = "wrong password",
#'   service = "google.com",
#'   tokenfile = tokenfile
#' )
#' identical(pass1, pass3)
#'
#' # ---------- Replace token ----------
#' # Existing token will be replaced with a new token
#' pass4 <- get_credential(
#'   master_password = "my password",
#'   method = "replace",
#'   service = "google.com",
#'   special_chr = "@#$%^&*",
#'   tokenfile = tokenfile
#' )
#' print(pass4)
#' identical(pass1, pass4)
#'
#' @export
get_credential <- function(
  master_password, method = c("get_or_create", "replace", "query"),
  service = NULL, special_chr = "~`! @#$%^&*()_-+={[}]|:;'<,>.?/",
  tokenfile = NULL, verbose = FALSE
){

  method <- match.arg(method)

  illegal_regexp <- "[^a-zA-Z0-9=_.,@:-]"

  if(method %in% c("get_or_create", "replace")) {
    # service must be valid
    if(length(service) != 1 || is.na(service) || !is.character(service) ||
       trimws(service) == "" || grepl(illegal_regexp, service) ) {
      stop("In 'get_or_create' or 'replace' mode, `service` must be provided with a non-empty string. This string can only contain letters, digits, and/or the following characters: \"=_.,-\"")
    }
    if(length(master_password) != 1 || is.na(master_password) ||
       nchar(master_password) < 8) {
      stop("`master_password` must be a character string of length >= 8")
    }
    if(length(special_chr) != 1 || is.na(special_chr) || !is.character(special_chr)) {
      stop("`special_chr` must be a character string")
    }
  }

  if(is.null(tokenfile)) {
    tokenfile <- getOption("dipsaus.file.credential",
                         file.path(R_user_dir("dipsaus", "config"), "credential_tokenfile"))
  }

  ensure_tokenfile <- function() {
    if(file.exists(tokenfile)) { return() }
    if(!dir.exists(dirname(tokenfile))) {
      dir.create(dirname(tokenfile), showWarnings = FALSE, recursive = TRUE)
    }
    file.create(tokenfile)
  }

  # no check, service token seed must contain no space, \t, or \n
  encode <- function(service, token, seed, special_chr0) {
    if(missing(special_chr0)) {
      special_chr0 <- special_chr
    } else {
      if(length(special_chr0) != 1 || is.na(special_chr0) || !is.character(special_chr0)) {
        stop("Internal error: Invalid `special_chr0`")
        special_chr0 <- special_chr
      }
    }
    special_enc <- base64_urlencode(special_chr0)
    service <- gsub(illegal_regexp, "", service)
    if(length(service) != 1 || service == "") { stop("Invalid service") }
    service <- base64_urlencode(service)
    token <- gsub(illegal_regexp, "", token)
    if(length(token) != 1 || service == token) { stop("Invalid token") }
    token <- base64_urlencode(token)
    seed <- as.integer(seed)
    if(length(seed) != 5 || any(is.na(seed) | seed <= 0)) { stop("Invalid seed") }
    seed <- base64_urlencode(paste(seed, collapse = " "))
    base64_urlencode(sprintf("%s\t%s\t%s\t%s", service, token, seed, special_enc))
  }

  decode <- function(s) {
    s <- base64_urldecode(s)
    s <- strsplit(s, split = "\t")[[1]]
    service <- base64_urldecode(s[[1]])
    token <- base64_urldecode(s[[2]])
    seed <- as.integer(strsplit(base64_urldecode(s[[3]]), " ")[[1]])
    specials <- base64_urldecode(s[[4]])
    if(length(service) != 1 || service == "" ||
       grepl(illegal_regexp, service)) { stop("Invalid service") }
    if(length(token) != 1 || service == token ||
       grepl(illegal_regexp, token)) { stop("Invalid token") }
    if(length(seed) != 5 || any(is.na(seed) | seed <= 0)) { stop("Invalid seed") }
    if(length(specials) != 1 || !is.character(specials)) {
      stop("invalid specials")
    }

    list(
      service = service,
      token = token,
      seed = seed,
      specials = specials
    )
  }

  maps <- NULL
  if(file.exists(tokenfile)) {

    maps <- lapply(readLines(tokenfile), function(s) {
      tryCatch({
        return(decode(s))
      }, error = function(e){
        NULL
      })
    })

    maps <- drop_nulls(maps)
    if(length(maps)) {

      map_names <- vapply(maps, "[[", FUN.VALUE = "", "service")
      dups <- duplicated(map_names)
      if(any(dups)) {
        map_names <- map_names[!dups]
        maps <- maps[!dups]
      }

      names(maps) <- map_names

    }

  }
  maps <- as.list(maps)

  if(verbose) {
    cat(names(maps), "", sep = "\n")
  }

  if(method == "query") {
    return(maps)
  }

  if(service %in% names(maps)) {
    if(method == "replace") {
      token <- list(
        service = service,
        token = rand_string(50),
        seed = c(sample(2^31-1, 1), sample(20, 4, replace = TRUE)),
        specials = special_chr
      )
      # re-generate map-file
      maps[[service]] <- token
      enc <- sapply(maps, function(token) {
        encode(
          service = token$service,
          token = token$token,
          seed = token$seed,
          special_chr0 = token$specials
        )
      })
      ensure_tokenfile()
      writeLines(enc, tokenfile)
    } else {
      token <- maps[[service]]
    }
  } else {
    token <- list(
      service = service,
      token = rand_string(50),
      seed = c(sample(2^31-1, 1), sample(20, 4, replace = TRUE)),
      specials = special_chr
    )
    enc <- encode(
      service = token$service,
      token = token$token,
      seed = token$seed,
      special_chr0 = token$specials
    )
    # Append
    ensure_tokenfile()
    cat(sprintf("%s\n", enc), file = tokenfile, append = TRUE)
  }

  pwd_sig <- digest::digest(master_password, algo = "sha256", serialize = FALSE)
  token_string <- sprintf("MasterPassword:%s Service:%s Token:%s Seed: %s", pwd_sig, token$service, token$token, paste(token$seed, collapse = ","))

  # generate password
  pwd <- digest::digest(token_string, algo = "sha256", serialize = FALSE)

  # break pwd every 4 characters
  pwd <- apply(matrix(strsplit(pwd, "")[[1]], nrow = 2), 2, function(x){
    strtoi(paste(x, collapse = ""), base = 16L)
  })
  pwd_order <- order(pwd)

  distribution <- sort(token$seed[-1])
  distribution <- ceiling(distribution / sum(distribution) * length(pwd))
  distribution <- cumsum(distribution)

  pwd_case <- 5 - (
    (pwd_order <= distribution[[1]]) + (pwd_order <= distribution[[2]]) +
      (pwd_order <= distribution[[3]]) + (pwd_order <= distribution[[4]])
  )


  dict_special <- strsplit(token$specials, "")[[1]]
  dict_digit <- as.character(seq.int(0,9))
  dict_lower <- letters
  dict_upper <- letters

  dict_list <- list(
    dict_special, dict_digit, dict_lower, dict_upper
  )

  res <- apply(cbind(pwd, pwd_case), 1, function(x){
    dict <- dict_list[[x[[2]]]]
    dict[(x[[1]] %% (length(dict) - 1)) + 1]
  })

  res <- paste(res, collapse = "")
  return(res)

}
