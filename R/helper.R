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










