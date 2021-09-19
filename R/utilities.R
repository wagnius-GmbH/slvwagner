#' Convert Windows file path to R compatible file path
#'
#' @name r_path
#' @description Copy the file path from windows explorer to your clipboard and call the function with no argument.
#' The R compatible file Path will be returned
#' @param  path
#' @return Character vector of R compatible file path
#' @examples # Copy file path to the cliboard and call the function.
#' utils::writeClipboard("C:\\Windows")
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

