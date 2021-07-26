#' \item{\code{\link{qs_map}}}{
#' A 'qs' map uses package 'qs' as backend. This map is very similar to
#' \code{rds_map}, but is especially designed for large values. For example,
#' pushing 1GB data to \code{qs_map} will be 100 times faster than using
#' \code{rds_map}, and \code{text_map} will almost fail. However, compared
#' to \code{rds_map} the stored data cannot be normally read by R as they
#' are compressed binary files. And \code{qs_map} is heavier than
#' \code{text_map}.
#' }

#' @rdname map
#' @export
qs_map <- function(path = tempfile()){
  QsMap$new(path = path)
}

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

#' @rdname map
#' @param name character, map name. If map names are the same, the data
#' will be shared.
#' @export
redis_map <- function(name = rand_string()){
  RedisMap$new(map_id = name)
}

