###################################################################
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

r_path <- function(path = "clipboard",...) {
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

###################################################################
#' Function to convert R path to windows compatible path
#'
#' @name r_win_path
#' @description Function to convert R path to windows compatible path.
#' @param  x R path character vector
#' @return Windows compatible character vector
#' @examples
#' r_win_path(tempdir())
#' @export

r_win_path <- function(x){
   x <- chartr("\\", "/", x)
   utils::writeClipboard(x)
   return(x)
}


###################################################################
#' Function to check if variable is defined
#'
#' @name r_is.defined
#' @description Function to convert windows path to R compatible path.
#' @param  sym variable or function name
#' @return TRUE or FALSE
#' @export

r_is.defined <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env)
}




