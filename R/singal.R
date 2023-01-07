###################################################################
#' Center Signal around zero (AC cupling)
#'
#' @name signal_center
#' @description centre data point around zero, that the range(\code{x}) is +/- the same value.
#' @param x vector
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns centred vector \code{x}
#' @examples
#' x <- rnorm(100,mean = -2.5)
#' plot(x)
#' signal_center(x)|>plot()
#' library(tidyverse)
#' tibble(y = rnorm(100,mean = -2.5))|>
#'   mutate(x = row_number(y),
#'          y_centered = signal_center(y))|>
#' pivot_longer(cols = c("y", "y_centered"))|>
#' ggplot(aes(x,value, color = name))+
#'   geom_point()+
#'   geom_hline(yintercept = 0)

#' @export

signal_center <- function (x)
{
  c_range <- range(x)
  c_center <- (diff(c_range)/2)+c_range[1]
  if(c_center > 0) return(x-c_center)
  else if(c_center == 0) return(x)
  else return(x+abs(c_center))
}

