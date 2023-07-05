#######################################
#' CAS 2d rotation matrix
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


#######################################
#' CAS 3d rotation matrix
#'
#' @name cas_rot_matrix3d
#' @description
#' Calculates a 3D rotation matrix from a given axis and angle. The axis is defined by \code{x|>ysym()}. The Rotation will be done around the axis defined by
#' \code{angle|>ysym()} and the root. The angle will be applied by the right hand rule.
#' @details
#' The calculation is done by Ryacas (yacas) and uses symbolic math. The package Ryacas and the software YACAS needs to be installed.
#' @details \url{http://www.yacas.org/}
#' @param  x unit vector c(x1,x2,x3)|>ysym()
#' @param  angle vector of angel in radiant c(angle)|>ysym()
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns 3D rotation matrix for given axis and angle.
#' @examples
#' library(Ryacas)
#' rot_matrix <- cas_rot_matrix3d(matrix(c(1,2,3))|>ysym(),c(-pi)|>ysym())
#' rot_matrix
#' @export

cas_rot_matrix3d <- function(x,angle){
  if(is.list(x) & is.list(angle)){
    y_cos <- cos(angle)
    cos1_cos <- yac_str(paste0("(",1,"-",y_cos,")"))
    y_sin <- sin(angle)
    yac_str(paste0(
      "{{",x[1],"^2       *(",cos1_cos,")+",y_cos,",
    ",x[1],"*",x[2],"*(",cos1_cos,")-",x[3],"*",y_sin,",
    ",x[1],"*",x[3],"*(",cos1_cos,")+",x[2],"*",y_sin,"},

    {",x[2],"*",x[1],"*(",cos1_cos,")+",x[3],"*",y_sin,",
     ",x[2],"^2       *(",cos1_cos,")+",y_cos,",
     ",x[2],"*",x[3],"*(",cos1_cos,")-",x[1],"*",y_sin,"},

   {",x[3],"*",x[1],"*(",1,"-",y_cos ,")-",x[2],"*",y_sin,",
    ",x[3],"*",x[2],"*(",1,"-",y_cos ,")+",x[1],"*",y_sin,",
    ",x[3],"^2       *(",1,"-",y_cos ,")+",y_cos,"}}"))|>
      ysym()
  }else stop(print("No yacs object supplied"))
}



