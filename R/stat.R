#######################################
#' Probability density function
#'
#' @name standard_norm_dist
#' @description Probability density function with parameter sigma(sd,Standard deviation) and mu(mean)
#' @param x data
#' @param mu mean
#' @param sd standard deviation
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns vector
#' @examples c_seq <- seq(-6,6,0.1)
#' plot(x = c_seq, standard_norm_dist(c_seq,mu = 0, sd = 1), type = "l")
#' @export

standard_norm_dist <- function(x,mu,sd){
  1/sqrt(2*pi*sd)*exp(-(x-mu)^2/2*sd)
}
