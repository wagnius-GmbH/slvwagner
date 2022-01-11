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
#' @param x matrix with 3 rows of points and 2 colums with the coordinates.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns data frame with center coordinates and Radius.
#' @examples
#' x <- matrix(c(c(-2.23 , 4.389 ),
#'               c(-1.001,-3.23  ),
#'               c(  15.5,-7.2365)),
#'               dimnames = list(c("x","y"),paste0("x",1:3)),
#'               ncol = 3)|>t()
#' math_circle_from3points(x)
#'
#' library(tidyverse)
#' library(ggforce)
#' ggplot()+
#'   geom_point(data = as.data.frame(x),aes(x,y), color = "blue", size = 1.5)+
#'   geom_point(data = math_circle_from3points(x),aes(x_center,y_center))+
#'   geom_label(data = math_circle_from3points(x),aes(x_center,y_center,label = "center"), nudge_y = 1.5)+
#'   geom_circle(data = math_circle_from3points(x), aes(x0 = x_center , y0 = y_center,r = radius))+
#'   geom_label(data = as.data.frame(x),aes(x,y,label = paste("input:",row.names(as.data.frame(x)))), nudge_y = 1.5)+
#'   geom_hline(yintercept = 0)+
#'   geom_vline(xintercept = 0)+
#'   scale_x_continuous(limits = c(-5,22))+
#'   coord_fixed()
#' @export

math_circle_from3points<-function(x){
  if(is.matrix(x) & nrow(x)==3){
    A <- cbind(c(1,1,1), x)
    b <- c(-(A[1,2]^2+A[1,3]^2),
           -(A[2,2]^2+A[2,3]^2),
           -(A[3,2]^2+A[3,3]^2))

    c_result <- solve(A,b)
    return(data.frame(x_center = -c_result[2]/2,
                      y_center = -c_result[3]/2,
                      radius  = sqrt((-c_result[2]/2)^2+(-c_result[3]/2)^2- c_result[1]))
    )
  }else{
    return(writeLines(paste("only matrix[3][2] (3 points with 2 coordinates) can be used to calculat the circle")))
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
#' #2D
#' u <- c(-0,1)
#' v <- c( 1,0)
#' math_inbetweenAngle(u,v)
#'
#' #3D
#' u <- c(-12, 13,   -2.56)
#' v <- c(  3,  5, -100   )
#' math_inbetweenAngle(u,v)
#'
#' @export

math_inbetweenAngle <- function(u,v){
  return(acos(sum(u*v)/(sqrt(sum(u^2))*sqrt(sum(v^2)))))
}

#######################################
#' linear function with parameter mx+b
#'
#' @name math_lf
#' @description
#' linear function with parameter slope and intercept.
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
#' linear function but with data frame instead of single parameters
#'
#' @name math_lf_df_mb
#' @description
#' Calculate y for different linear functions.
#' The parameters slope and intercept shall be provided by data frame with columns "slope" and "intercept".
#' @details
#' f(x)= y = m_{slopes}* x + b_{intercepts}
#' @param x vector
#' @param df_mb data frame with "slope(s)" and "intercept(s)"
#' @return vector \code{y}
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples df <- data.frame(slope = 0.5, intercept = 1)
#' math_lf_df_mb(0:10,df)
#' df <- data.frame(slope = c(0.5,1,-0.5), intercept = c(1,-10,0))
#' math_lf_df_mb(10,df)
#' @export


math_lf_df_mb <- function(x,df_mb){
  x*df_mb[,"slope"]+df_mb[,"intercept"]
}

#######################################
#' Find linear function perpendicular to linear function through given point
#'
#' @name math_lf_perpendicular
#' @description
#' Find linear function`s perpendicular to supplied linear function through given point \code{x}.
#' @param x point vector
#' @param lf data.frame with the definiton of the given linear function containing the column slope and intercept
#' @return data.frame with the slope and intercept
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples
#' math_lf_perpendicular(x = c(2,-9),
#'                       data.frame(slope = -3,intercept = 5))
#' @export

math_lf_perpendicular <- function(x,lf){ #x point(s) lf(intercept, slope)
  s=-1/lf$slope
  i =x[2]-(math_lf_rev_slope(lf$slope)*x[1])
  return(data.frame(slope = s,intercept = i))
}

#######################################
#' find perpendicular slope to linear function
#'
#' @name math_lf_rev_slope
#' @description
#' find perpendicular slope to linear function
#' @param slope The slop or m of linear function f(x)= mx+b
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
#' y <- data.frame(p1 = c(x = -1, y = -5),
#'                 p2 = c(x =  3, y =  3))
#'
#' math_lf_fromPoints(x)
#'
#' result <- math_lf_fromPoints(y)
#'
#' library(tidyverse)
#' ggplot(as_tibble(t(y)),aes(x,y))+
#' geom_abline(data = result,aes(slope = slope, intercept =intercept), size = 1.5)+
#' geom_point(shape = 10, size = 10,color = "blue")+
#' geom_hline(yintercept = 0)+
#' geom_vline(xintercept = 0)
#' @export

math_lf_fromPoints <- function (x){
  delta <- (x[,1]-x[,2])
  m <- delta[2]/delta[1]
  b <- x[2,1]-m*x[1,1]
  return(data.frame(slope = m,intercept = b))
}

#######################################
#' Intersection point from two linear functions
#'
#' @name math_lf_intersect
#' @description
#' Calculate intersecting point(s) from two linear function \code{x} and linear function \code{y}.
#'
#' @details
#' Get the intersecting point of two linear Functions. If only \code{x} is supplied, \code{x} needs to be a matrix or data.frame containing both
#' linear functions. If \code{x} and or \code{y} have names you shall use the names (slope, intercept).
#' @param x matrix or data frame (column names (slope,intercept))
#' @param y matrix or data frame (column names (slope,intercept))
#' @return vector
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples
#' x <- data.frame(slope = 2.3, intercept = -2)
#' y <- data.frame(slope = 2.3, intercept = -2)
#' math_lf_intersect(x,y)
#' y <- y|>
#'   rbind(data.frame(slope = 1.25, intercept = 2))
#' math_lf_intersect(y)
#' names(y)<-c("slope","intercept")
#' math_lf_intersect(y)
#' math_lf_intersect(data.frame(slope = c(0.2,-1),intercept = c(2,3)))
#' math_lf_intersect(matrix(c(0.2,-1,2,3),ncol = 2))
#' y <- matrix(c(0.2,-1,2,3),ncol = 2)
#' math_lf_intersect(y)
#' @export

math_lf_intersect <- function(x,...){
  if(missing(...)){
    if(nrow(x)>1){
      if(is.null(names(x))){
        result_x = (x[2,2]-x[1,2])/(x[1,1]-x[2,1])
        result_y = x[2,1]*result_x+x[2,2]
        return(c(x = result_x, y = result_y))
      }else{
        result_x = (x[2,"intercept"]-x[1,"intercept"])/(x[1,"slope"]-x[2,"slope"])
        result_y = x[2,"slope"]*result_x+x[2,"intercept"]
        return(c(x = result_x, y = result_y))
      }
    }else{
      print("x contains only a singl function")
      return(NULL)
    }
  }else{
    if(is.null(names(x))){
      if(setequal(x,...)){
        print("x and y are identical: cannot calculate interception point")
        return(NULL)
      }else{
        result_x = (...[,2]-x[,2])/(x[,1]-y[,1])
        result_y = ...[,1]*result_x+...[,2]
        return(c(x = pull(result_x), y = pull(result_y)))
      }
    }else{
      if(setequal(x,...)){
        print("x and y are identical: cannot calculate interception point")
        return(NULL)
      }else{
        result_x = (...[,"intercept"]-x[,"intercept"])/(x[,"slope"]-...[,"slope"])
        result_y = ...[,"slope"]*result_x+...[,"intercept"]
        return(c(x = pull(result_x), y = pull(result_y)))
      }
    }
  }
}


#######################################
#' slerp  by 3 points and a given radius.
#'
#' @name math_slerp
#' @description
#' Radius interpolation by 3 points and a given radius.
#' The Radius interpolation will be calculated using the cp = common or center point and the lines defined by x1,x2
#' and the common center point cp.
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
  #Skallierung der Stuetzvektoren auf Betrag = 1 => Einheisvektoren
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


#######################################
#' 3d rotation matrix
#'
#' @name math_rot_matrix3d
#' @description
#' Calculates a 3D rotation matrix from a given axis and angle. The axis is defined by \code{x}. The Rotation will be done around the axis defined by
#' \code{angle} and the root. The angle will be appied by the right hand rule.
#' @details
#' \url{https://en.wikipedia.org/wiki/Rotation_matrix}
#' @param  x unit vector c(x1,x2,x3)
#' @param  angle vector of angel in radiant
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
         ncol = 3, byrow = TRUE)
}

#######################################
#' CAS 3d rotation matrix
#'
#' @name cas_rot_matrix3d
#' @description
#' Calculates a 3D rotation matrix from a given axis and angle. The axis is defined by \code{x|>ysym()}. The Rotation will be done around the axis defined by
#' \code{angle|>ysym()} and the root. The angle will be appied by the right hand rule.
#' @details
#' The calculation is done by Ryacas (yacas) and uses symbolic math. The package Ryacas and the software YACAS \url{http://www.yacas.org/}
#' needs to be installed.
#'
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
  }else print("No yacs object supplied")
}



#######################################
#' Transform vector by a rotation matrix
#'
#' @name math_rot_transform
#' @description transforms a given vecotr by a given rotation matrix using matirx multiplication
#' @details
#' \code{rot_matrix}%*%\code{x}
#' \url{https://en.wikipedia.org/wiki/Rotation_matrix}
#' @param x vector or matrix containing the coordinates
#' @param rot_matrix 2D or 3D rotation matrix
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns 3D rotation matrix for given axis and angle.
#' @examples
#' rot_matrix <- math_rot_matrix3d(c(1,1,1), pi)
#' math_rot_transform(c(1,1,1),rot_matrix)
#' @export

math_rot_transform <- function(x, rot_matrix){
  rot_matrix%*%x
}

#######################################
#' Find quadrant of single vector
#'
#' @name math_quadrant_vector
#' @description find the
#' @details
#' Finds the quadrant of vecotr. If it is on principle axi it returns the angle.
#' @param x vector containing the coordinates. e.g.first x, second y.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns Quadrant of vector(s)
#' @examples math_quadrant_vector(c(1,1))
#' @examples math_quadrant_vector(c(-1,1))
#' @examples math_quadrant_vector(c(-1,-1))
#' @examples math_quadrant_vector(c(1,-1))
#' @examples math_quadrant_vector(c(0,1))
#' @examples math_quadrant_vector(c(0, -1))
#' @examples math_quadrant_vector(c( 1, 0))
#' @examples math_quadrant_vector(c(-1, 0))
#' @examples math_quadrant_vector(c( 0, 0))
#' @examples math_quadrant_vector(c("12","sdf"))
#' @export

math_quadrant_vector  <- function(x){
  if(class(x)%in%c("numeric","integer")){
    if (x[1]== 0 || x[2]== 0){
      if     (x[1] > 0 & x[2] == 0) return(0)
      else if(x[2] > 0 & x[1] == 0) return(90)
      else if(x[1] < 0 & x[2] == 0) return(180)
      else if(x[2] < 0 & x[1] == 0) return(-90)
      else if(x[1]== 0 & x[2] == 0) print("Zero coordinates supplied")
      else print("math_quadrant_vector function error")
    }else if(x[1]== 0 & x[2] == 0) print("Zero coordinates supplied")
    else if (x[1] > 0 & x[2]  > 0) return(1)
    else if (x[1] < 0 & x[2]  > 0) return(2)
    else if (x[1] < 0 & x[2]  < 0) return(3)
    else if (x[1] > 0 & x[2]  < 0) return(4)

  }else print("data not compatible")
}

#######################################
#' Find quadrant of matrix or data frame
#'
#' @name math_quadrant
#' @description find the
#' @details
#' Find the quadrant(s) of vecotr(s). If it is on principle axis it returns the angel.
#' @param x vector or matrix containing the coordinates. First x, second y.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns Quadrant(s) of vector(s)
#' @examples
#' m <- matrix(c(c(1,-1,-1, 1,0,1),
#'             c(1, 1,-1,-1,1,0)),
#'             ncol = 2, byrow = FALSE)
#' math_quadrant(m)
#' data.frame(x = c(1,-1),y = c(1,-1))|>math_quadrant()
#' math_quadrant(c(1,0))
#' math_quadrant(c(1,0,12))
#' @export

math_quadrant  <- function(x){
  if(is.null(dim(x))){ # check if only single vector
    if (is.character(x)){
      print("character not compatible")
    }else{
      math_quadrant_vector(x)
    }
    math_quadrant_vector(x)
  }else if("matrix" %in% class(x) & typeof(x) %in%c("double","integer")){ # check if matrix and numerical values
    apply(x, 1, math_quadrant_vector)
  }else if("data.frame"%in%class(x) & ncol(x) == 2){
    x|>
      as.matrix()|>
      apply(1, math_quadrant_vector)
  }else{
    print("data not compatible")
  }
}



#######################################
#' Find the angle of a vector
#'
#' @name math_angle_quadrant_vector
#' @description find the
#' @details
#' Find the angle to the first axis "x" for given vector of coordinates.
#' @param x vector containing the coordinates. e.g. First x, second y.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns Quadrant(s) of given vector
#' @examples math_angle_quadrant_vector(c( 1, 1))/pi*180
#' @examples math_angle_quadrant_vector(c(-1, 1))/pi*180
#' @examples math_angle_quadrant_vector(c(-1,-1))/pi*180
#' @examples math_angle_quadrant_vector(c( 1,-1))/pi*180
#' @examples math_angle_quadrant_vector(c( 0, 1))/pi*180
#' @examples math_angle_quadrant_vector(c( 0,-1))/pi*180
#' @examples math_angle_quadrant_vector(c( 1, 0))/pi*180
#' @examples math_angle_quadrant_vector(c(-1, 0))/pi*180
#' @export

math_angle_quadrant_vector  <- function(x){
  type <- math_quadrant_vector(x)
  if      (type == 1) atan(x[2]/x[1])
  else if (type == 2) atan(x[2]/x[1])+pi
  else if (type == 3) atan(x[2]/x[1])-pi
  else if (type == 4) atan(x[2]/x[1])
  else if (type ==   0)   0
  else if (type ==  90)  90/180*pi
  else if (type == 180) 180/180*pi
  else if (type == -90) -90/180*pi
  else{
    print(type)
    print("function error: math_angle_quadrant_vector")
  }
}


#######################################
#' Find quadrant of matrix or data frame
#'
#' @name math_angle_quadrant
#' @description Find the angle(s) of vector(s) using the quadrant information.
#' @param x vector or matrix containing the coordinates. First x, second y.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns Quadrant(s) of vector(s)
#' @examples
#' m <- matrix(c(c(1,-1,-1, 1,0,1),
#'             c(1, 1,-1,-1,1,0)),
#'             ncol = 2, byrow = FALSE)
#' math_angle_quadrant(m)/pi*180
#' data.frame(x = c(1,-1),y = c(1,-1))|>math_angle_quadrant()
#' math_angle_quadrant(c(1,0))
#' math_angle_quadrant(c("1.25","test"))
#' @export

math_angle_quadrant  <- function(x){
  if(is.null(dim(x))){ # check if only single vector
    if (is.character(x)){
      print("character not compatible")
    }else{
      math_angle_quadrant_vector(x)
    }
    math_angle_quadrant_vector(x)
  }else if("matrix" %in% class(x) & typeof(x) %in%c("double","integer")){ # check if matrix and numerical values
    apply(x, 1, math_angle_quadrant_vector)
  }else if("data.frame"%in%class(x) & ncol(x) == 2){
    x|>
      as.matrix()|>
      apply(1, math_angle_quadrant)
  }else{
    print("data not compatible")
  }
}

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
#' @returns
#' density
#' @examples
#' c_seq <- seq(-6,6,0.1)
#' plot(x = c_seq, standard_norm_dist(c_seq,mu = 0, sd = 1), type = "l")
#' @export
standard_norm_dist <- function(x,mu,sd){
  1/sqrt(2*pi*sd)*exp(-(x-mu)^2/2*sd)
}

