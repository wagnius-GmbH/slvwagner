#####
#' Magnitued of a vector
#'
#' Get the magnitude of a vector of any lenght
#' @name math_betrag
#' @description Get the magnitude of a vector of any lenght

#' @param x numerical vector
#' @return numeriacal vector of lenght one
#' \code{x}
#' @examples math_betrag(c(1,1))
#' @examples math_betrag(c(1,1,1))
#' @author Florian Wagner slvwagner@gmail.com
#' @export


math_betrag <- function(x) {
  sqrt(sum(x^2))
}

