

#' @name map
#' @title Create R object map.
#' @description Provides five types of map that fit in different use cases.
#' @return An \code{R6} instance that inherits \code{\link[dipsaus]{AbstractMap}}
#' @details There are five types of map implemented. They all inherit class
#' \code{\link[dipsaus]{AbstractMap}}. There are several differences in
#' use case scenarios and they backend implementations.
#'
#' \describe{
#' \item{\code{\link{session_map}}}{
#' A session map takes a \code{\link[fastmap]{fastmap}} object. All objects are
#' stored in current R session. This means you cannot access the map from other
#' process nor parent process. The goal of this map is to share the data across
#' different environments and to store global variables, as long as they share
#' the same map object. If you are looking for maps that can be shared
#' by different processes, check the rest map types. The closest map type is
#' \code{redis_map}, which is also memory-based.
#' }
#' \item{\code{\link{rds_map}}}{
#' An 'RDS' map uses file system to store values. The values are stored
#' separately in '.rds' files. Compared to session maps, 'RDS' map can be
#' shared across different R process. It's recommended to store
#' large files in \code{rds_map}. If the value is not large in RAM,
#' \code{text_map} and \code{redis_map} are recommended.
#' }
#' \item{\code{\link{qs_map}}}{
#' A 'qs' map uses package 'qs' as backend. This map is very similar to
#' \code{rds_map}, but is especially designed for large values. For example,
#' pushing 1GB data to \code{qs_map} will be 100 times faster than using
#' \code{rds_map}, and \code{text_map} will almost fail. However, compared
#' to \code{rds_map} the stored data cannot be normally read by R as they
#' are compressed binary files. And \code{qs_map} is heavier than
#' \code{text_map}.
#' }
#' \item{\code{\link{text_map}}}{
#' A 'text' map uses file system to store values. Similar to \code{rds_map},
#' it can be stored across multiple processes as long as the maps share the
#' same file directory. However, unlike \code{rds_map}, \code{text_map}
#' the \code{text_map} can only store basic data values, namely atom data types.
#' The supported types are: numeric, character, vector, list, matrix
#' It's highly recommended to convert factors to characters. Do NOT use if the
#' values are functions or environments. The recommended use case scenario
#' is when the speed is not the major concern, and you want to preserve data
#' with backward compatibility. Otherwise it's highly recommended to use
#' \code{redis_map}, \code{qs_map}, and \code{rds_map}.
#' }
#' \item{\code{\link{redis_map}}}{
#' A 'Redis' map uses free open source software `Redis` and R package
#' 'RcppRedis' as backend. Compared to session map, 'Redis' map can be
#' shared across sessions. Compared to 'text' and 'rds' maps, 'Redis' map
#' stores data in memory, meaning a potential of significant speed ups. To use
#' \code{redis_map}, you need to install `Redis` on your computer.
#' \itemize{
#'   \item On Mac: use `\code{brew install redis}` to install and
#'     `\code{brew services start redis}` to start the service
#'   \item On Linux: use `\code{sudo apt-get install redis-server}` to install
#'     and `\code{sudo systemctl enable redis-server.service}` to start the
#'     service
#'   \item On Windows: Download from
#'     \url{https://github.com/dmajkic/redis/downloads} and double click
#'     'redis-server.exe'
#' }
#' }
#' }
#' @examples
#' # ----------------------Basic Usage ----------------------
#'
#' # Define a path to your map.
#' path = tempfile()
#' map <- qs_map(path)
#'
#' # Reset
#' map$reset()
#'
#' # Check if the map is corrupted.
#' map$validate()
#'
#' # You have not set any key-value pairs yet.
#' # Let's say two parallel processes (A and B) are sharing this map.
#' # Process A set values
#' map$keys()
#'
#' # Start push
#' # set a normal message
#' map$set(key = 'a', value = 1)
#'
#' # set a large object
#' map$set(key = 'b', value = rnorm(100000))
#'
#' # set an object with hash of another object
#' map$set(key = 'c', value = 2, signature = list(
#'   parameter1 = 123,
#'   parameter2 = 124
#' ))
#'
#' # Check what's in the map from process B
#' mapB <- qs_map(path)
#' mapB$keys()
#' mapB$keys(include_signatures = TRUE)
#'
#' # Number of key-values pairs in the map.
#' mapB$size()
#'
#' # Check if key exists
#' mapB$has(c('1','a', 'c'))
#'
#' # Check if key exists and signature also matches
#' mapB$has('c', signature = list(
#'   parameter1 = 123,
#'   parameter2 = 124
#' ))
#'
#' # Signature changed, then return FALSE. This is especially useful when
#' # value is really large and reading the value takes tons of time
#' mapB$has('c', signature = list(
#'   parameter1 = 1244444,
#'   parameter2 = 124
#' ))
#'
#' # Destroy the map's files altogether.
#' mapB$destroy()
#'
#' \dontrun{
#'   # Once destroyed, validate will raise error
#'   mapB$validate()
#' }
#'
#'
NULL

#' @rdname map
#' @param map a \code{fastmap::fastmap()} list
#' @export
session_map <- function(map = fastmap::fastmap()){
  SessionMap$new(map = map)
}

#' @rdname map
#' @param path directory path where map data should be stored
#' @export
rds_map <- function(path = tempfile()){
  FileMap$new(path = path)
}

#' @rdname map
#' @export
text_map <- function(path = tempfile()){
  warning('text_map is soft deprecated Please use rds_map instead.')
  TextMap$new(path = path)
}

#' @rdname map
#' @export
qs_map <- function(path = tempfile()){
  QsMap$new(path = path)
}

#' @rdname map
#' @param name character, map name. If map names are the same, the data
#' will be shared.
#' @export
redis_map <- function(name = rand_string()){
  RedisMap$new(map_id = name)
}


