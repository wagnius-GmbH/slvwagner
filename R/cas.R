#######################################
#' CAS 2d rotation matrix
#'
#' @name math_rot_matrix2d
#' @description
#' Calculates a 2D rotation matrix with a given angle \code{x}
#' @details
#' \url{https://en.wikipedia.org/wiki/Rotation_matrix}
#' @param  x vector of an angel in radiant
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns rotation matrix for given angle
#' @examples
#' math_rot_matrix2d(c(pi))
#' math_rot_matrix2d(c(-pi))
#' @export

math_rot_matrix2d <- function(x){
  #Drehmatrix 2D
  matrix(c(cos(x),sin(x),-sin(x),cos(x)), ncol = 2)
}

#######################################
#' 2d rotation matrix
#'
#' @name cas_rot_matrix2d
#' @description
#' Calculates a 2D rotation matrix with a given angle \code{x}
#' @details
#' The calculation is done by Ryacas (yacas) and uses symbolic math. The package Ryacas and the software YACAS \url{http://www.yacas.org/}
#' needs to be installed.
#'
#' @param  x vector of an angel in radiant
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns rotation matrix for given angle
#' @examples
#' library(Ryacas)
#' cas_rot_matrix2d(c(pi))
#' rot_matrix <- cas_rot_matrix2d(c(-pi)|>ysym())
#' rot_matrix
#' @export

cas_rot_matrix2d <- function(x){
  if(is.list(x)){
    y_cos <- cos(x)
    y_sin <- sin(x)
    yac_str(paste0("{{",y_cos,",",-y_sin,"},{",y_sin,",",y_cos,"}}"))|>
      ysym()
  }else print("No yacs object supplied")
}
