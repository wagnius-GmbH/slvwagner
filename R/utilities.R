###################################################################
#' Convert Windows file path to R compatible file path
#'
#' @name r_path
#' @description Copy the file path from windows explorer to your clipboard and call the function with no argument.
#' The R compatible file Path will be returned
#' @param path Path can be copied to clipboard
#' @return Character vector of R compatible file path
#' @examples # Copy file path to the cliboard and call the function.
#' utils::writeClipboard("C:\\Windows")
#' r_path()
#' r_path("C:\\Windows")
#' @export

r_path <- function(path = "clipboard") {
  if (path == "clipboard") {
    y <- utils::readClipboard()
  } else {
    y <- path
  }
  x <- chartr("\\", "/", y)
  utils::writeClipboard(x)
  return(x)
}

###################################################################
#' Convert R path to windows compatible path
#'
#' @name r_win_path
#' @description Function to convert R path to windows compatible path.
#' @param  x R path character vector
#' @return Windows compatible character vector
#' @examples
#' r_win_path(tempdir())
#' @export

r_win_path <- function(path = "clipboard"){
  if (path == "clipboard") {
    y <- utils::readClipboard()
  }else{
    y <- path
  }
  x <- chartr("/","\\", y)
  utils::writeClipboard(x)
  return(x)
}

# r_path("C:\\src\\repos\\slvwagner")

###################################################################
#' Check if variable is defined in R`s global environment.
#'
#' @name r_is.defined
#' @description Function to Check if variable is defined in R`s global environment.
#' @param  sym variable or function name
#' @return TRUE or FALSE
#' @export

r_is.defined <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env)
}

###################################################################
#' Colourise text for display in the terminal.
#'
#' If R is not currently running in a system that supports terminal colours
#' the text will be returned unchanged.
#'
#' Allowed colours are: black, blue, brown, cyan, dark gray, green, light
#' blue, light cyan, light gray, light green, light purple, light red,
#' purple, red, white, yellow
#'
#' @param text character vector
#' @param fg foreground colour, defaults to white
#' @param bg background colour, defaults to transparent
#' @examples
#' print(r_colourise("Red", "red"))
#' cat(r_colourise("Red", "red"), "\n")
#' cat(r_colourise("White on red", "white", "red"), "\n")
#' @export

r_colourise <- function(text, fg = "black", bg = NULL) {
  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")

  if(r_cmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    return(text)
  }

  col_escape <- function(col) {
    paste0("\033[", col, "m")
  }

  col <- .fg_colours[tolower(fg)]
  if (!is.null(bg)) {
    col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")
  }

  init <- col_escape(col)
  reset <- col_escape("0")
  paste0(init, text, reset)
}

.fg_colours <- c(
  "black" = "0;30",
  "blue" = "0;34",
  "green" = "0;32",
  "cyan" = "0;36",
  "red" = "0;31",
  "purple" = "0;35",
  "brown" = "0;33",
  "light gray" = "0;37",
  "dark gray" = "1;30",
  "light blue" = "1;34",
  "light green" = "1;32",
  "light cyan" = "1;36",
  "light red" = "1;31",
  "light purple" = "1;35",
  "yellow" = "1;33",
  "white" = "1;37"
)

.bg_colours <- c(
  "black" = "40",
  "red" = "41",
  "green" = "42",
  "brown" = "43",
  "blue" = "44",
  "purple" = "45",
  "cyan" = "46",
  "light gray" = "47"
)

r_cmd_running <- function() {
  nchar(Sys.getenv('R_TESTS')) != 0
}



