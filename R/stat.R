#######################################
#' Probability density function
#'
#' @name stat_norm_dist
#' @description Probability density function with parameter sigma(sd,Standard deviation) and mu(mean)
#' @param x data
#' @param mu mean
#' @param sd standard deviation
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns vector
#' @examples c_seq <- seq(-6,6,0.1)
#' plot(x = c_seq, stat_norm_dist(c_seq,mu = 0, sd = 1), type = "l")
#' @export

stat_norm_dist <- function(x,mu,sd){
  1/sqrt(2*pi*sd)*exp(-(x-mu)^2/2*sd)
}

#######################################
#' Process capability index
#'
#' @name stat_cpk
#' @description Calculation of Cpk Process capability index given parameter USL, LSL mean \eqn{\mu} and standard deviation \eqn{\sigma}.
#' @details Estimates what the process is capable of producing, considering that the process mean may not be centered between the specification (Upper and Lower) limits.
#' @details <https://en.wikipedia.org/wiki/Process_capability_index>
#' @param LSL lower specification limit parameter
#' @param USL upper specification limit parameter
#' @param mue the estimated mean of the process \eqn{\mu}
#' @param sigma standard deviation \eqn{\sigma}
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns vector
#' @examples stat_cpk(LSL = -0.03, USL = -0.01, mue = -0.02, sigma = 0.005)
#' @examples stat_cpk(LSL = -0.05, USL = 0.05, mue =c(-0.01, -0.015), sigma = c(0.001,0.015))
#' @export
stat_cpk <- function(LSL, USL, mue, sigma){
  if(length(USL)==1 & is.numeric(USL) & length(LSL)==1 & is.numeric(LSL)){
    return(min(mue-LSL, USL-mue)/(3*sigma))
  }else{
    writeLines("vector LSL and USL have grater lenght than 1")
    NULL
  }
}


