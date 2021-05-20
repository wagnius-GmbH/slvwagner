####################################
#' Magnitude of a vector
#'
#' @name math_betrag
#' @description Get the magnitude of a vector of any length

#' @param x numerical vector
#' @return numerical vector of length one
#' \code{x}
#' @examples math_betrag(c(1,1))
#' @examples math_betrag(c(1,1,1))
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @export


math_betrag <- function(x) {
  sqrt(sum(x^2))
}

#######################################
#' Circle from 3 points
#'
#' @name math_circle_from3points
#' @description
#' calculate circle using 3 points.The function returns a tibble (dataframe) or
#' a named vector with the center point and the radius.
#' @param x numerical point matrix
#' @param x numerical vector
#' @param type c("df,"m","v"), df = data.frame(), m = matrix(), v = c()
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' data frame, matrix or vector
#' \code{x}
#' @examples
#' x <- matrix(c(c(-2.23 , 4.389 ),
#'               c( 1.001,-3.23  ),
#'               c(  15.5,-7.2365)),
#'               ncol = 3, byrow = FALSE)
#' colnames(x) <- paste0("x",1:3)
#' rownames(x)<- c("x","y")
#'
#' math_circle_from3points(x)
#' math_circle_from3points(x, type = "v")
#' result <- math_circle_from3points(x, type = "df")
#'
#' library(tidyverse)
#' library(ggforce)
#' ggplot()+
#' geom_point(data = as.data.frame(t(x)),aes(x,y))+
#' geom_circle(data = result, aes(x0 = x_center , y0 = y_center,r = r))+
#' coord_fixed()
#' @export

math_circle_from3points<-function(x,type = "m"){
  if(sum(class(x)==c("matrix","array"))==2){
    A <- cbind(c(1,1,1), t(x))
    b <- c(-(A[1,2]^2+A[1,3]^2),
           -(A[2,2]^2+A[2,3]^2),
           -(A[3,2]^2+A[3,3]^2))

    c_result <- solve(A,b)
  }else{
    return(writeLines(past("only point matrix can be calculated => matrix[3][2] 3points and 2 coordinates")))
  }
  if(type == "m"){
    return(as.matrix(data.frame(x_center = -c_result[2]/2,
                                y_center = -c_result[3]/2,
                                r  = sqrt((-c_result[2]/2)^2+(-c_result[3]/2)^2- c_result[1])))
    )
  }else{
    if(type == "v"){
      c(x_center = -c_result[2]/2,
        y_center = -c_result[3]/2,
        r = sqrt((-c_result[2]/2)^2+(-c_result[3]/2)^2- c_result[1]))
    }else{
      if(type == "df"){
        data.frame(x_center = -c_result[2]/2,
                   y_center = -c_result[3]/2,
                   r  = sqrt((-c_result[2]/2)^2+(-c_result[3]/2)^2- c_result[1]))
      }else{
        writeLines(paste("math_circle_from3points:\nunknown type argument type:",type))
        }
    }
  }
}


#######################################
#' Angle between 2 vectors
#'
#' @name math_inbetweenAngle
#' @description
#' Get the angle between 2 vectors. Returns the smallest Angle between 2 vectors in radiant.
#' @param u fist vector
#' @param v second vector
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns the smallest Angle between 2 vectors (scalar) in radiant.
#' \code{x}
#' @examples
#' 2D
#' u <- c(-0,1)
#' v <- c( 1,0)
#' math_inbetweenAngle(u,v)
#'
#'3D
#' u <- c(-12, 13,   -2.56)
#' v <- c(  3,  5, -100   )
#' math_inbetweenAngle(u,v)
#'
#' @export

math_inbetweenAngle <- function(u,v){
  return(acos(sum(u*v)/(sqrt(sum(u^2))*sqrt(sum(v^2)))))
}

#######################################
#' linear function
#'
#' @name math_lf
#' @description
#' linear function with parameter slope and intercept given by data frame.
#' @details \eqn{f(x)=mx+b}
#' @param x vector
#' @param m slope
#' @param b intercept
#' @return vector \code{x}
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples
#' math_lf(-12:10,0.8,3.265)
#' plot(math_lf(-12:10,0.8,3.265))
#' @export

math_lf <- function(x,m,b){
  return(m*x+b)
}


#######################################
#' Find linear function perpendicular to linear function through given point
#'
#' @name math_lf_perpendicular
#' @description
#' Find linear function perpendicular to linear function through given point.
#' @param lf data.frame(slope = c(0.12,0.78),
#'                                  intercept = c(-25, 13))
#' @param point vector
#' @return data.frame(x,y) \code{x}
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples
#' math_lf_perpendicular(point = c(2,-9),
#'                       data.frame(slope =c(-2,5),intercept = c(3,5)))
#' @export
math_lf_perpendicular <- function(point,lf){ #point c(x,y) lf(intercept, slope)
  s=-1/lf$slope
  i =point[2]-(math_lf_rev_slope(lf$slope)*point[1])
  return(data.frame(slope = s,intercept = i))
}

#######################################
#' find perpendicular slope to linear function
#'
#' @name math_lf_rev_slope
#' @description
#' find perpendicular slope to linear function
#' @param lf data.frame(slope     = c(  0.12, 0.78),
#'                      intercept = c(-25   ,13   ))
#' @param point vector
#' @return data.frame(x,y) \code{x}
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples
#' math_lf_rev_slope(c(0.1,-0.89))
#' @export
#senkrechte zu einer geraden
math_lf_rev_slope <- function(slope){#
  return(-1/slope)
}

#######################################
#' linear function from 2 points
#'
#' @name math_lf_fromPoints
#' @description
#' get linear fuction from 2 points x1 and x2
#' @param x matrix or data frame
#' @return data.frame(slope, intercept) \code{x}
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples
#' x <- matrix(c(c(-2,5),
#'               c( 3,3)), nrow = 2, byrow = FALSE)
#'
#' y <- data.frame(x1 = c(x = -1, y = -5),
#'                 x2 = c(x =  3, y =  3))
#'
#' math_lf_fromPoints(x)
#'
#' result <- math_lf_fromPoints(y)
#'
#' library(tidyverse)
#' ggplot(as_tibble(t(y)),aes(x,y))+
#' geom_point()+
#' geom_abline(data = result,aes(slope = slope, intercept =intercept))

#' @export
math_lf_fromPoints <- function (x){
  delta <- x[,1]-x[,2]
  s <- delta[2]/delta[1]
  i <- x[1,2]-x[1,2]*delta[2]/delta[1]
  return(data.frame(slope = s,intercept = i))
}


#######################################
#' slerp  by 3 points and a given radius.
#'
#' @name math_slerp
#' @description
#' Radius interpolation by 3 points and a given radius.
#' The Radius interpolation will be calculated using the cp = conmen or centrer point and the lines defined by x1,x2 and the comon center point cp
#' The common point therefore is cp and will be used as position vector.
#' @details
#' \deqn{sin[(1-t)*phi] / sin(phi) * x1   +  sin(t*phi/sin(phi)  *  x2)}
#' phi ==> smallest angel between vector x1 and x2
#' \url{https://en.wikipedia.org/wiki/Slerp}
#' @param  R radius
#' @param  x1 first point vector
#' @param  x2 second point vector
#' @param  cp position vector, common centrer
#' @param  nb_points count of points to generate for the radius interpolation
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns point matrix with the calculated coordinates \code{x}
#' @examples
#' p <- t(as.matrix(data.frame(x1 = c(-10,-5,50),
#'                             x2 = c(-20,10,2),
#'                             cp = c(10,-10,10))))
#'
#' m <- math_slerp(R  =10,
#'                 x1 = p["x1",],
#'                 x2 = p["x2",],
#'                 cp = p["cp",],
#'                 nb_points = 20)
#' m <- rbind(p,m)
#' #Plot 3D
#' library(rgl)
#' plot3d( m[,1], m[,2], m[,3], type = "p", lwd = 2, top = TRUE,
#'         #col = rainbow(nrow(m)),
#'         aspect = c(diff(c(min(m[,1]),max(m[,1]))),
#'                    diff(c(min(m[,2]),max(m[,2]))),
#'                    diff(c(min(m[,3]),max(m[,3])))
#'                    ))
#' @export

math_slerp <- function(R,x1,x2,cp,nb_points = 10) { #slerp aus drei Punkten, Radius, Punkteanzahl
  #(cp => Ortsvektor)
  #Verschieben des Koordinatensystems: Neuer Ursprung cp
  sp <- x1-cp #Stützvektor
  ep <- x2-cp #Stützvektor

  #Stuetzvektoren
  s1l   <- sp-c(0,0,0) #Stuetzvektor aus Ortsvektor und Startpunkt
  s2l   <- ep-c(0,0,0) #Stuetzvektor aus Ortsvektor und Endpunkt
  #Stuetzvektoren gleich gross machen
  s1 <- s1l*math_betrag(s2l)
  s2 <- s2l*math_betrag(s1l)
  #Skallierung der Stuetzvektoren auf Betrag = 1 =>Einheisvektoren
  skalierung <- math_betrag(s1)
  s1 <- s1/skalierung
  s2 <- s2/skalierung
  #Zwischenwinkel der beiden Vektoren
  phi<-math_inbetweenAngle(s1,s2)
  #Slerp zwischen den Einheitsvektoren mit s1,s2
  t <- seq(0,1,1/nb_points)#Zahl zwischen Null und eins 0...1 die den Winkel aufteilt
  #slerp Berechnung
  slerp <- t(sapply(sin((1-t)*phi)/sin(phi), function(i) i*s1, simplify = TRUE)+
               sapply(sin(t*phi)/sin(phi), function(i) i*s2, simplify = TRUE ))
  #Vektoren strecken mit vorgegebenen Radius R
  slerp <- slerp*R
  #Koordinaten um cp (Ortsvektor) verschieben
  return(t(apply(slerp, 1, function(i) i + cp)))
}

#######################################
#' 2d rotation matrix
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
#' Basic rotation matrix
#'
#' @name math_rot_matrix_basic
#' @description
#' Calculates a 3D rotation matrix from a given principle axis and a given angle. The axis is defined by \code{ax}. The Rotation will be done around the axis defined by
#' \code{ax} and the root.
#' @details
#' \url{https://en.wikipedia.org/wiki/Rotation_matrix}
#' @param x angel in radiant
#' @param ax parameter to define which principle axis will be used to calculate the rotation matrix
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns 3D basic rotation matrix for given axis and angle.
#' @examples
#' math_rot_matrix_basic(pi, "x")
#' math_rot_matrix_basic(pi, "x2")
#' math_rot_matrix_basic(pi, 3)
#' @export

math_rot_matrix_basic <- function(x,ax){
  if(ax == "x" | ax == "x1" | ax == 1){
    #Axis x or the fist axis x1
    matrix(c(1,0,0,0,cos(x),sin(x),0,-sin(x),cos(x)), ncol = 3)
  }else if(ax == "y" | ax == "x2" | ax == 2){
    #Axis x or the fist axis x1
    matrix(c(cos(x),0,sin(x),0,1,0,-sin(x),0,cos(x)), ncol = 3)
  }else if(ax == "z" | ax == "x3" | ax == 3){
    #Axis x or the fist axis x1
    matrix(c(cos(x),-sin(x),0,sin(x),cos(x),0,0,0,1), ncol = 3)
  }
}

#######################################
#' 3d rotation matrix
#'
#' @name math_rot_matrix3d
#' @description
#' Calculates a 3D rotation matrix from a given axis and angle. The axis is defined by \code{x}. The Rotation will be done around the axis defined by
#' \code{angle} and the root. The angle will be appied by the right hand rule.
#' @details
#' \url{https://en.wikipedia.org/wiki/Rotation_matrix}
#' @param  angle vector of angel in radiant
#' @param  x unit vector c(x1,x2,x3)
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns 3D rotation matrix for given axis and angle.
#' @examples
#' math_rot_matrix3d(matrix(c(1,2,3)),c(-pi))
#' @export

math_rot_matrix3d <- function(x,angle){
  #Drehmatrix 3d um einen Vektor
  matrix(c(c(x[1]^2*(1-cos(angle))+cos(angle),
             x[1]*x[2]*(1-cos(angle))-x[3]*sin(angle),
             x[1]*x[3]*(1-cos(angle))+x[2]*sin(angle)),
           c(x[2]*x[1]*(1-cos(angle))+x[3]*sin(angle),
             x[2]^2*(1-cos(angle))+cos(angle),
             x[2]*x[3]*(1-cos(angle))-x[1]*sin(angle)),
           c(x[3]*x[1]*(1-cos(angle))-x[2]*sin(angle),
             x[3]*x[2]*(1-cos(angle))+x[1]*sin(angle),
             x[3]^2*(1-cos(angle))+cos(angle))),
         ncol = 3)
}


