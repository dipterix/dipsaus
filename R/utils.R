#' Convert color to Hex string
#' @param col character or integer indicating color
#' @param alpha \code{NULL} or numeric, transparency. See \code{grDevices::rgb}
#' @param prefix character, default is \code{"#"}
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
col2hexStr <- function(col, alpha = NULL, prefix = '#'){
  col = grDevices::col2rgb(col, alpha = FALSE) / 255
  col = grDevices::rgb(red = col[1,], green = col[2,], blue = col[3,], alpha = alpha)
  stringr::str_replace(col, '^[^0-9A-F]*', prefix)
}