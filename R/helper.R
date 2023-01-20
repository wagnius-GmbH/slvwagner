###################################################################
#' Format number to defined signify(x,digits = 3)
#'
#' @name r_signif
#' @description creates the same result as the Base R function format(\code{x}, format = "g", digits = 3), it`s just a wrapper with my preferred parameters.
#' @param x numerical vector
#' @param significant_digits number of significant digits you prefer.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns character vector
#' @examples
#' library(stringi)
#' test <- c(25,signif(rnorm(10),3))
#' test|>  paste0("\n")|>writeLines()
#' test|>
#'   r_signif()|>
#'   paste0("\n")|>writeLines()
#' test|>
#'   format(format = "g", digits = 3)|>
#'   paste0("\n")|>writeLines()

#' @export

r_signif <- function (x, significant_digits = 3)
{
  format(x, format = "g", digits = significant_digits)
}










