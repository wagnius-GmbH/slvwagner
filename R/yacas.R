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
#' cas_rot_matrix3d(matrix(c(1,2,3)),c(-pi))
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


#######################################
#' CAS coordinate function of a plane
#'
#' @name cas_plane_fun
#' @description
#' Function returning a cas function of a plane defined by postion vector \eqn{p} and the normal vector \eqn{n} of a plane.
#' @details
#' The calculation is done by Ryacas (yacas) and uses symbolic math. The package Ryacas and the software YACAS needs to be installed.
#' @details \url{http://www.yacas.org/}
#' @param  p position vector
#' @param  n normal vector of the plane
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Yacas function
#'
#' @examples
#' library(Ryacas)
#' cas_plane_fun(c(2,3,4), c(12,5,1)|>math_unit_vector())
#' @export

cas_plane_fun <- function(p,n){
  x <- paste0("x",1:3)|>
    Ryacas::ysym()

  EQ_Ebene <- Ryacas::ysym(n)*(x-Ryacas::ysym(p))
  EQ_Ebene <- EQ_Ebene[1]+EQ_Ebene[2]+EQ_Ebene[3]
  EQ_Ebene
}

