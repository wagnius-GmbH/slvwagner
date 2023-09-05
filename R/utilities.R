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
#' @param  path R path character vector
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
#' @param color foreground colour, defaults to white
#' @examples
#' # copy paste the code to the terminal
#' print(r_colourise("text", "red"))
#' r_colourise("text", "red")
#' c("this", "text","is colored")|>r_colourise(c("red","green","yellow"))
#' @export

r_colourise <- function(text, color = "black") {
  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")

  if(r_cmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    return(text)
  }

  col_escape <- function(col) {
    paste0("\033[", col, "m")
  }
  # stings to concatenate
  col <- .color_colours[tolower(color)]
  init <- col_escape(col)
  reset <- col_escape("0")

  # execute it
  paste0(init, text, reset)|>
    writeLines()
}

.color_colours <- c(
  "black" = "0;30",
          "blue" = "0;34",
          "green" = "0;32",
          "cyan" = "0;36",
          "red" = "0;31",
          "purple" = "0;35",
          "brown" = "0;33",
          "lightgray" = "0;37",
          "darkgray" = "1;30",
          "lightblue" = "1;34",
          "lightgreen" = "1;32",
          "lightcyan" = "1;36",
          "pink" = "1;31",
          "purple" = "1;35",
          "yellow" = "1;33",
          "white" = "1;37"
)


r_cmd_running <- function() {
  nchar(Sys.getenv('R_TESTS')) != 0
}


###################################################################
#' Creates a table of contents for a Rmd file
#' @name r_tbl_contents
#' @details
#' Scans documents for headings and creates a table of contents (hyper linked). The returnend string can directly be written as .Rmd file.
#' @param  FileName file name or connection
#' @return sting of .Rmd file with table of contents
#' @export

r_tbl_contents <- function(FileName) {
  # read markdown file
  c_Rmd <- suppressWarnings(readLines(FileName))
  c_Rmd

  # regex search pattern
  p <- "^#"

  # find position to insert table of contents
  for(ii in 1:length(c_Rmd)){
    if(stringr::str_detect(c_Rmd[ii],p)) {
      c_start <- ii
      break
    }
  }

  # Headings to find
  heading <- c_Rmd[stringr::str_detect(c_Rmd, p)]|>stringr::str_remove_all("#")|>stringr::str_split("<", simplify = T)
  heading <- heading[,1]|>stringr::str_trim()

  # put anchor to headings
  c_Rmd <- ifelse(stringr::str_detect(c_Rmd, p), paste0(c_Rmd, "<a name=\"",c_Rmd|>stringr::str_remove_all("#")|>stringr::str_trim(),"\"></a>"), c_Rmd)
  c_Rmd

  # create table on contents
  c_Rmd <- c(c_Rmd[1:(c_start-1)],
             "\n# Tabel of Content",
             paste0("[",heading,"]","(#",heading,")","  \\"), "\n", # insert table of contents
             c_Rmd[c_start:length(c_Rmd)])
  return(c_Rmd)
}


