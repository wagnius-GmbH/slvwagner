#######################################
#' Calculate unit vector
#'
#' @name signal_center
#' @description centre data point around zero, that the range of the vector \code{x} is +/- the same value.
#' @param x vector
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns centred vector \code{x}
#' @examples
#' rnorm(100,mean = -0.5)|>signal_center()
#' @export


signal_center <- function (x)
{
  c_range <- range(x)
  c_center <- (diff(c_range)/2)+c_range[1]
  if(c_center > 0) return(x-c_center)
  else if(c_center == 0) return(x)
  else return(x+abs(c_center))
}
