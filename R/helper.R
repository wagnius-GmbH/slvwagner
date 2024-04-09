###################################################################
#' Format number to defined signify(x,digits = 3)
#'
#' @name r_signif
#' @description
#' Formatting a numerical vector \code{x} to a character vector. The format will be defined with respect to the significant digits for each vector index \code{x[]}.
#' So the string length of an vector index may vary according to the max(\code{x}) and min(\code{x}) supplied.
#' @details
#'  creates the same result as the Base R function format(\code{x}, format = "g", digits = 3), it`s just a wrapper with my preferred parameters.
#' @param x numerical vector
#' @param significant_digits number of significant digits you prefer.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns character vector
#' @examples
#' set.seed(123456)
#' test <- c(25,signif(rnorm(10),3))
#' test|>paste0()|>writeLines()
#' test|>r_signif()|>writeLines()
#' test|>format(format = "g", digits = 3)|>writeLines()
#' c(25,9255666,0.0000034646465321)|>r_signif()|>writeLines()

#' @export

r_signif <- function (x, significant_digits = 3)
{
  format(x, format = "g", digits = significant_digits)
}


#######################################
#' Generate truth.table
#'
#' @name truth.table
#' @description
#' Create a truth table for the number of \code{bit}`s supplied. The Standard is a binary truth.table so the \code{exponent = 2}.
#' If  (\code{exponent} > 9) you can choose (\code{hex_style = TRUE}) to exchange any bigger number than 9 with letters,see examples for more information.
#' @param bit integer number of bits
#' @param exponent type of truth table
#' @param hex_style If exponent > 9 the number will be changed to letters.
#' @param stringsAsFactors logical specifying if character vectors are converted to factors.Check expand.grid() documentation for more information.
#' @param KEEP.OUT.ATTRS	a logical indicating the "out.attrs" attribute. Check out expand.grid() function for more detailed information.
#' @param fileName If a file name is supplied the created data frame will automatically saved in the actual working directory with file extension ".csv".
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' A data frame containing one row for each combination of the supplied factors(\code{bit}`s`) .
#' @examples
#' truth.table(3)
#' truth.table(3, 16)|>head(20)
#' truth.table(3, 16, hex_style = TRUE)|>head(20)
#' @export

truth.table <- function(bit, exponent = 2, hex_style = FALSE ,stringsAsFactors = TRUE, KEEP.OUT.ATTRS = TRUE, fileName = NA){
  # creat vector
  c_var <- 0:(exponent-1)
  # Hex style
  if(exponent > 9 & hex_style == T){
    c_var <- c(0:9,LETTERS[1:(exponent-10)])
  }
  # create list
  l_data <- list()
  for(ii in 1:bit){
    l_data[[ii]] <- c_var
  }
  names(l_data) <- 1:bit

  # The following code is from expand.grid()
  nargs <- length(args <- l_data)
  if (!nargs) {
    return(as.data.frame(list()))
  }
  if (nargs == 1L && is.list(a1 <- args[[1L]])) {
    nargs <- length(args <- a1)
  }
  if (nargs == 0L) {
    return(as.data.frame(list()))
  }
  cargs <- vector("list", nargs)
  iArgs <- seq_len(nargs)
  nmc <- paste0("Var", iArgs)
  nm <- names(args)
  if (is.null(nm)) {
    nm <- nmc
  }else if (any(ng0 <- nzchar(nm))){
    nmc[ng0] <- nm[ng0]
  }
  names(cargs) <- nmc
  rep.fac <- 1L
  d <- lengths(args)
  if (KEEP.OUT.ATTRS) {
    dn <- vector("list", nargs)
    names(dn) <- nmc
  }
  orep <- prod(d)
  if (orep == 0L) {
    for (i in iArgs) {
      cargs[[i]] <- args[[i]][FALSE]
    }
  }else{
    for (i in iArgs) {
      x <- args[[i]]
      if (KEEP.OUT.ATTRS)
        dn[[i]] <- paste0(nmc[i], "=", if (is.numeric(x)){ format(x)} else {x})
      nx <- length(x)
      orep <- orep/nx
      if (stringsAsFactors && is.character(x)){
        x <- factor(x, levels = unique(x))
      }
      x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac,nx)), orep)]
      cargs[[i]] <- x
      rep.fac <- rep.fac * nx
    }
  }
  if (KEEP.OUT.ATTRS) {
    attr(cargs, "out.attrs") <- list(dim = d, dimnames = dn)
    }
  rn <- .set_row_names(as.integer(prod(d)))

  df_data <- structure(cargs, class = "data.frame", row.names = rn)

  if(!is.na(fileName)) write.table(df_data[,bit:0], file = paste0(fileName, ".csv"), row.names = FALSE, sep = ";")

  return(df_data[,bit:0])
}






