#' Convert Windows file path to R compatible file path
#'
#'Copy the file path from in the windows explorer to your clipboard and call the function with no argument.
#'
#' @name r_path
#' @description Copy the file path from in the windows explorer to your clipboard and call the function with no argument.
#' The R compatible file Path will be returned

#' @keywords conversion
#' @param empty
#' @return Character vector of R compatible file path
#' @examples
#' Importent: Copy file path first to the cliboard.
#' r_path()
#' @export

r_path <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    utils::readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  utils::writeClipboard(x)
  return(x)
}

