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
#' spez. Round  for Swiss currency "CHF"
#'
#' @name round5Rappen
#' @description rounds for swiss currency. Between values of 0.075 to 0.1249 will be round to 0.1.
#' From values of 0.025 to 0.0749 will be round to 0.1.
#' @param x numeric vector
#' @param round_digits how many digit need to be considered for rounding
#' @return numeric vector
#' @examples
#' round5Rappen(c( 0.02499, 0.025, 0.0749, 0.075))
#' round5Rappen(c(10.02499,10.025,10.0749,10.075))
#' round5Rappen(c(10.52499,10.525,10.5749,10.575))
#' round5Rappen(c(10.72499,10.725,10.7749,10.775))
#' @export

round5Rappen <- function(x, round_digits = 9) {
  result <- lapply(x, function(x){
    if(is.na(x)){
      return(NA)
    }else{
      y <- round(x-as.integer(x*10)/10,round_digits)
      if(y>=0.075){
        return((as.integer(x*10)/10)+0.1)
      }else {
        if(y>=0.025){
          return((as.integer(x*10)/10)+0.05)
        }else{
          return((as.integer(x*10)/10)+0.0)
        }
      }
    }
  })
  return(result|>
           unlist())
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
#' Check if package is loaded in R`s global environment.
#'
#' @name r_is.library_loaded
#' @description Function to Check if package is loaded R`s global environment.
#' @param  package_name package name character vector
#' @return TRUE or FALSE
#' @examples
#' r_is.library_loaded("ThisIsNotAlib")
#' library("slvwagner")
#' r_is.library_loaded("slvwagner")
#' @export
#'
r_is.library_loaded <- function(package_name) {
  is_loaded <- FALSE
  tryCatch({
    is_loaded <- requireNamespace(package_name, quietly = TRUE)
  }, error = function(e) {
    is_loaded <- FALSE
  })
  return(is_loaded)
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
#' Clear console or Terminal function
#'
#' @export
r_clear_terminal <- function(){
  ##  ------------------------------------------------------------
  ##  Detect RStudio Terminal or RStudio Console or Terminal macOS
  ##  --------------------------------------------------------------
  if (commandArgs()[1]=='RStudio'){
    ##  method print: \f: Form Feed
    print.cleanup <- function(cleanupObject) cat("\f")
  }else if(substr(commandArgs()[1], nchar(commandArgs()[1]), nchar(commandArgs()[1])) == "R"){
    print.cleanup <- function(cleanupObject) cat(c("\033[2J","\033[H"))
  }else{
    print(paste0("not support: ",commandArgs()[1]))
    }
  clc <- 0                                        ##  variable from class numeric
  class(clc) <- 'cleanup'
  print(clc)
}




