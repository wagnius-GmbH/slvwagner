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
#'   geom_label(data = math_circle_from3points(x),
#'     aes(x_center,y_center,label = "center"), nudge_y = 1.5)+
#'   geom_circle(data = math_circle_from3points(x),
#'     aes(x0 = x_center , y0 = y_center,r = radius))+
#'   geom_label(data = as.data.frame(x),
#'     aes(x,y,label = paste("input:",row.names(as.data.frame(x)))), nudge_y = 1.5)+
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
#' Angle between two vectors
#'
#' @name math_inbetweenAngle
#' @description
#' Get the angle between 2 vectors. Returns the smallest Angle between two vectors in radiant.
#' @details <https://de.wikipedia.org/wiki/Skalarprodukt>
#' @param u fist vector
#' @param v second vector
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns the smallest angle between two vectors in radiant.
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
#' slerp  by 3 points and a given radius.
#'
#' @name math_slerp
#' @description
#' Radius interpolation by 3 points and a given radius.
#' The Radius interpolation will be calculated using the \code{cp} = common center point and the vector \code{x1} and \code{x2}.
#' @details
#' \deqn{sin((1 - t) * phi) / sin(phi) * x1   +  sin(t * phi / sin(phi)  *  x2)}
#' \url{https://en.wikipedia.org/wiki/Slerp}
#' @param  R radius
#' @param  x1 vector
#' @param  x2 vector
#' @param  cp vector
#' @param  nb_points count of points to be generate by the function.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Matrix with \code{nrow(nb_points)}
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
  s1l   <- sp-c(0,0,0) #Stützvektor aus Ortsvektor und Startpunkt
  s2l   <- ep-c(0,0,0) #Stützvektor aus Ortsvektor und Endpunkt
  #Stuetzvektoren gleich gross machen
  s1 <- s1l*math_betrag(s2l)
  s2 <- s2l*math_betrag(s1l)
  #Skallierung der Stuetzvektoren auf Betrag = 1 => Einheitsvektoren
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
#' Calculates a 2D rotation matrix for a given \code{angle}. The rotation will be applied by the right hand rule.
#' @details
#' \url{https://en.wikipedia.org/wiki/Rotation_matrix}
#' @param  angle in radiant
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns 3D rotation matrix for given axis (unit vector) and angle.
#' @examples
#' math_rot_matrix2d(30/180*pi)
#' @export

### Rotations_matrix
math_rot_matrix2d <- function(angle){
  matrix(c(cos(angle),-sin(angle),
           sin(angle),cos(angle)),
         nrow = 2, byrow = T)
}


#######################################
#' 3d rotation matrix
#'
#' @name math_rot_matrix3d
#' @description
#' Calculates a 3D rotation matrix for a given rotation axis \code{x}(unit vector) and an angle  \code{angle}. The rotation will be applied by the right hand rule.
#' @details
#' \url{https://en.wikipedia.org/wiki/Rotation_matrix}
#' \url{https://de.wikipedia.org/wiki/Drehmatrix#Drehmatrizen_des_Raumes_%E2%84%9D%C2%B3}
#' @param  x unit vector c(x1,x2,x3) defining the rotation axis
#' @param  angle in radiant
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns 3D rotation matrix for given axis (unit vector) and angle.
#' @examples
#' math_rot_matrix3d(matrix(c(0,0,1)),0.5)
#' matrix(c(1,2,3))|>
#'   math_unit_vector()|>
#'   math_rot_matrix3d(-pi)
#' math_rot_matrix3d(matrix(c(0,0,1)),0.5)|>
#'   det()
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
  rot_matrix%*%x|>
    t()
}

#######################################
#' Find quadrant of vector
#'
#' @name math_quadrant_vector
#' @description Finds the quadrant of vector. If the vector is a principle axis, it returns the angle in degrees.
#' @param x vector containing the coordinates
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns Quadrant of vector(s)
#' @examples math_quadrant_vector(c(1,1))
#' x <- expand.grid(x = -1:1, y = -1:1)
#' cbind(x, result = x|>apply(1,math_quadrant_vector))
#' @export

math_quadrant_vector  <- function(x){
  if(class(x)%in%c("numeric","integer")){
    if (x[1]== 0 || x[2]== 0){
      if     (x[1] > 0 & x[2] == 0) return(0)
      else if(x[2] > 0 & x[1] == 0) return(90)
      else if(x[1] < 0 & x[2] == 0) return(180)
      else if(x[2] < 0 & x[1] == 0) return(-90)
      else if(x[1]== 0 & x[2] == 0) {
        cat(r_colourise("Zero coordinates supplied", "red"), "\n")
        return(NA)}
      else {
        cat(r_colourise("math_quadrant_vector function error", "red"), "\n")
        return(NA)}
    }else if(x[1]== 0 & x[2] == 0) {
      cat(r_colourise("Zero coordinates supplied", "red"), "\n")
      return(NA)}
    else if (x[1] > 0 & x[2]  > 0) return(1)
    else if (x[1] < 0 & x[2]  > 0) return(2)
    else if (x[1] < 0 & x[2]  < 0) return(3)
    else if (x[1] > 0 & x[2]  < 0) return(4)

  }else {
    cat(r_colourise("vector is not numerical", "red"), "\n")
    return(NA)
    }
}

#######################################
#' Find quadrant(s) of vector(s)
#'
#' @name math_quadrant
#' @description Find the quadrant(s) of vector(s).
#' If the vector(s) is a principle axis, it returns the angle(s) in degrees.
#' @param x dataframe or matrix containing the coordinates.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns Quadrant(s) of vector(s)
#' @examples
#' data.frame(x = c(1,-1),y = c(1,-1))|>math_quadrant()
#' math_quadrant(c(1,0))
#' math_quadrant(c(1,0,12))
#' m <- expand.grid(x = -1:1, y = -1:1)
#' cbind(m, result = math_quadrant(m))
#' @export

math_quadrant  <- function(x){
  if(is.null(dim(x))){ # check if only single vector
    math_quadrant_vector(x)
  }else { # check if matrix and numerical values
    apply(x, 1, math_quadrant_vector)
    }
}



#######################################
#' Find the angle of a vector
#'
#' @name math_angle_quadrant_vector
#' @description
#' Find the angle to the first axis "x" for given vector of coordinates.
#' @param x vector containing the coordinates. e.g. First x, second y.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns Quadrant of vector \code{x} in radiants.
#' @examples
#' math_angle_quadrant_vector(c( 1, 1))/pi*180
#' math_angle_quadrant_vector(c(-1, 1))/pi*180
#' math_angle_quadrant_vector(c(-1,-1))/pi*180
#' math_angle_quadrant_vector(c( 1,-1))/pi*180
#' math_angle_quadrant_vector(c( 0, 1))/pi*180
#' math_angle_quadrant_vector(c( 0,-1))/pi*180
#' math_angle_quadrant_vector(c( 1, 0))/pi*180
#' math_angle_quadrant_vector(c(-1, 0))/pi*180
#' @export

math_angle_quadrant_vector  <- function(x){
  if(!(x[1]== 0 & x[2]==0)){
    type <- math_quadrant_vector(x)
    if      (type %in% c(1,4)) atan(x[2]/x[1])
    else if (type == 2) atan(x[2]/x[1])+pi
    else if (type == 3) atan(x[2]/x[1])-pi
    else if (type ==   0)   0
    else if (type ==  90)  90/180*pi
    else if (type == 180) 180/180*pi
    else if (type == -90) -90/180*pi
  }else{ # if the coordinates are c(0,0) the angle cannot be calculated, therefore return NA
    return(NA)
  }
}


#######################################
#' Find quadrant(s) of vector(s)
#'
#' @name math_angle_quadrant
#' @description Find the angle(s) of vector(s) using the quadrant information.
#' @param x data.frame or matrix containing the coordinates.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' matrix of angle(s)
#' @examples
#' math_angle_quadrant(c(1,1))/pi*180
#' x <- expand.grid(x = -1:1, y = -1:1)
#' cbind(x, result = math_angle_quadrant(x))
#' @export

math_angle_quadrant  <- function(x){
  if(is.null(dim(x))){ # check if only single vector
    math_angle_quadrant_vector(x)
  }else { # check if matrix and numerical values
    apply(x, 1, math_angle_quadrant_vector)
  }
}


#######################################
#' Sperical to cartesian coordinates
#'
#' @name math_sph2cart
#' @description Transform sperical to cartesian coordinates according to international physics convention:
#' \eqn{\theta} in range 0...pi (0...180 Deg) and \eqn{\phi} in range 0...2*pi (0...360Deg)
#' @details <https://de.wikipedia.org/wiki/Kugelkoordinaten#Umrechnungen>
#' @param tpr c(\eqn{t = \theta}, \eqn{p = \phi}, r=radius) as vector or matrix
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' named vector or matrix
#' @examples
#' math_sph2cart(c(1,0.5,0.1))
#' rbind(c(1,0.5,0.1),
#'       c(1,0.2,0.6))|>
#'       math_sph2cart()
#' @export

math_sph2cart <- function (tpr)
{
  stopifnot(is.numeric(tpr))
  if (is.vector(tpr) && length(tpr) == 3) {
    theta <- tpr[1]
    phi <- tpr[2]
    r <- tpr[3]
    m <- 1
  }
  else if (is.matrix(tpr) && ncol(tpr) == 3) {
    theta <- tpr[, 1]
    phi <- tpr[, 2]
    r <- tpr[, 3]
    m <- nrow(tpr)
  }
  else stop("Input must be a vector of length 3 or a matrix with 3 columns.")
  z <- r*cos(theta)
  tmp <- r*sin(theta)
  x <- tmp*cos(phi)
  y <- tmp*sin(phi)
  if (m == 1)
    xyz <- c(x = x, y = y, z = z)
  else xyz <- cbind(x, y, z)
  return(xyz)
}


#######################################
#' Cartesian to sperical coordinates
#'
#' @name math_cart2sph
#' @description Transform cartesian to sperical coordinatem according to international physics convention:
#' \eqn{\theta} in range 0...pi (0...180 Deg) and \eqn{\phi} in range 0...2*pi (0...360Deg)
#' @details <https://de.wikipedia.org/wiki/Kugelkoordinaten#Umrechnungen>
#' @param xyz cartesian coordinates as vector or matrix
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' named vector or matrix
#' @examples
#' math_cart2sph(c(1,1,1))
#' rbind(c(1,0.5,0.1),
#'       c(1,0.2,0.6))|>
#'    math_cart2sph()
#' @export

math_cart2sph <- function (xyz)
{
  stopifnot(is.numeric(xyz))
  if (is.vector(xyz) && length(xyz) == 3) {
    x <- xyz[1]
    y <- xyz[2]
    z <- xyz[3]
    r     <- math_betrag(xyz)
    theta <- acos(z/r)
    phi   <- atan2(y,x)
    return(c(theta = theta, phi = phi ,r = r))
  }
  else if (is.matrix(xyz) && ncol(xyz) == 3) {
    x <- xyz[, 1]
    y <- xyz[, 2]
    z <- xyz[, 3]
    r     <- xyz|>
      apply(1,math_betrag)|>
      t()|>
      as.vector()
    theta <- acos(z/r)
    phi   <- atan2(y,x)
    return(cbind(theta, phi, r))
  }
  else stop("Input must be a vector of length 3 or a matrix with 3 columns.")
}


#######################################
#' Calculate unit vector
#'
#' @name math_unit_vector
#' @description calculate unite vector from \code{x}
#' @param x vector of cartesian coordinates
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' unit vector
#' @examples
#' math_unit_vector(c(x = 1,y = 1,z = 1))
#' math_unit_vector(c(1,2,3))
#' @export

math_unit_vector <- function(x){
  x/math_betrag(x)
}

#######################################
#' Calculate unit vector
#'
#' @name math_unit_vector
#' @description calculate unite vector from \code{x}
#' @param x vector of cartesian coordinates
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' unit vector
#' @examples
#' math_unit_vector(c(x = 1,y = 1,z = 1))
#' math_unit_vector(c(1,2,3))
#' @export

math_unit_vector <- function(x){
  x/math_betrag(x)
}

#######################################
#' Conic section from five point
#'
#' @name math_conic_section_from_5points
#' @description calculate conic section from five given points \code{section_points}
#' @details \eqn{Ax^2+Bxy+Cy^2+Dx+Ey+F=0}
#' @details <https://en.wikipedia.org/wiki/Conic_section>
#' @details The algorithm determines fist the type of conic section. If the conic section results in an ellipse or a circle the function returns useful values
#' such as the equation and the centre of the ellipse or circle.
#' Additionally you can define the number of points returned \code{nb} to visualize the ellipse or the circle.
#' For an ellipse additionally the semi \eqn{b} and major axis \eqn{a} of the ellipse are provided.
#' @param section_points matrix of section points, one point per row.
#' @param nb number of points returned to plot the ellipse
#' @author Florian Wagner \email{florian.wagner@wagnius.ch}
#' @returns
#' list
#'
#' @examples
#' section_points <- matrix(c(-1,3,
#'                           2,4,
#'                           6,2,
#'                           8,-5,
#'                           1,-3),
#'                           nrow = 5, byrow = TRUE)
#' l_list <- math_conic_section_from_5points(section_points, nb = 20)
#' l_list
#' l_list$df|>plot(asp = 1)
#' abline(a = l_list$lf_a_b[1,2], b = l_list$lf_a_b[1,1])
#' abline(a = l_list$lf_a_b[2,2], b = l_list$lf_a_b[2,1])
#' points(x = l_list$center[1],y = l_list$center[2])
#'
#' # hyperbola
#' matrix(c(-2,1,
#'          -1,3,
#'          1,-1.5,
#'          0.8,0.8,
#'          2,2.8),
#'          nrow = 5, byrow = TRUE)|>
#'    math_conic_section_from_5points()
#'
#' #circle
#' l_list <- (matrix(c(-1,0,
#'          0,1,
#'          1,0,
#'          0,-1,
#'          cos(pi/4),sin(pi/4)),
#'          nrow = 5, byrow = TRUE)+0.2)|>
#'    math_conic_section_from_5points(nb = 20)
#' l_list
#' l_list$df|>plot(asp=1)
#'
#' #parabola
#' matrix(c(-2,3,
#'          -1,0,
#'          0,-1,
#'          1,0,
#'          2,3),
#'          nrow = 5, byrow = TRUE)|>
#'    math_conic_section_from_5points()
#' @export


math_conic_section_from_5points <- function(section_points, nb = 10){
  A <- matrix(c(section_points[1,1]^2,section_points[1,1]*section_points[1,2],section_points[1,2]^2,section_points[1,1],section_points[1,2],
                section_points[2,1]^2,section_points[2,1]*section_points[2,2],section_points[2,2]^2,section_points[2,1],section_points[2,2],
                section_points[3,1]^2,section_points[3,1]*section_points[3,2],section_points[3,2]^2,section_points[3,1],section_points[3,2],
                section_points[4,1]^2,section_points[4,1]*section_points[4,2],section_points[4,2]^2,section_points[4,1],section_points[4,2],
                section_points[5,1]^2,section_points[5,1]*section_points[5,2],section_points[5,2]^2,section_points[5,1],section_points[5,2]),
              nrow = 5, byrow = T)

  b <- matrix(rep(-1,5))

  P <- solve(A,b)
  P <- rbind(P,1)
  rownames(P)<- LETTERS[1:6]

  M0 <- matrix(c(P["F",]  , P["D",]/2, P["E",]/2,
                 P["D",]/2, P["A",]  , P["B",]/2,
                 P["E",]/2, P["B",]/2, P["C",]),
               nrow=3, byrow=TRUE)

  M <- matrix(c(P["A",],P["B",]/2,
                P["B",]/2,P["C",]),nrow = 2 ,byrow = T)

  lambda <- eigen(M)$values
  if((lambda[1]-P["A",])<=(lambda[2]-P["C",])){
    lambda <- c(lambda[2],lambda[1])
  }
  M_eigen <- eigen(M)

  if(det(M0) != 0){
    if(det(M)<0) {
      #writeLines("hyperbola")
      return(list(type = "hyperbola",
                  EQ = paste0(P["A",],"*x^2+(",P["B",],")*x*y+(",P["C",],")*y^2+(",P["D",],")*x+(",P["E",],")*y+(",P["F",],")=0"),
                  "section points" = section_points))
      }
    else if (det(M)==0) {
      #writeLines("parabola")
      return(list(type = "parabola",
                  EQ = paste0(P["A",],"*x^2+(",P["B",],")*x*y+(",P["C",],")*y^2+(",P["D",],")*x+(",P["E",],")*y+(",P["F",],")=0"),
                  "section points" = section_points))
      }
    else if (det(M)>0) { #either ellipse or circle

      a <- sqrt(-det(M0)/(det(M)*lambda[1]))
      b <- sqrt(-det(M0)/(det(M)*lambda[2]))

      center <- solve(M,c(-P["D",]/2,-P["E",]/2))|>as.vector()
      t <- seq(0, 2*pi, len = nb)

      if(a==b){
        #writeLines("circle")
        df <- data.frame(x = (a*cos(t)) + center[1],
                         y = (a*sin(t)) + center[2]
                         )
        return(list(type = "circle",
                    EQ = paste0(P["A",],"*x^2+(",P["B",],")*x*y+(",P["C",],")*y^2+(",P["D",],")*x+(",P["E",],")*y+(",P["F",],")=0"),
                    "x(t)" = paste0(a,"*cos(t) +",center[1]),
                    "y(t)" = paste0(a,"*sin(t) +",center[2]),
                    df = df,
                    center = center,
                    "Input: Section points" = section_points)
               )
      }else{
        #writeLines("ellipse")
        m <- M_eigen$vectors[2,]/M_eigen$vectors[1,]
        lf <- matrix(c(m, center[2]-m*center[1]),nrow = 2, byrow = F)|>
          as.data.frame()|>
          setNames(c("slope","intercept"))

        phi <- ifelse((lambda[1]-P["A",])<=(lambda[2]-P["C",]),
                      atan(m[2]),
                      atan(m[1]))
        names(phi)<-NULL

        df <- data.frame(x = center[1] + a*cos(t)*cos(phi) - b*sin(t)*sin(phi),
                         y = center[2] + a*cos(t)*sin(phi) + b*sin(t)*cos(phi)
                         )

        return(list(type = "ellipse",
                    EQ = paste0(P["A",],"*x^2+(",P["B",],")*x*y+(",P["C",],")*y^2+(",P["D",],")*x+(",P["E",],")*y+(",P["F",],")=0"),
                    "x(t)" = paste0(center[1],"+", a, "*cos(t)*cos(",phi,") - ",b,"*sin(t)*sin(",phi,")"),
                    "y(y)" = paste0(center[2],"+ ",a, "*cos(t)*sin(",phi,") + ",b,"*sin(t)*cos(",phi,")"),
                    df = df,
                    center = center,
                    a = a,
                    b = b,
                    phi = phi,
                    lf_a_b = lf,
                    "Input: Section points" = section_points)
               )
        }
    }
    else if (A == C & B == 0) {
      #writeLines("circle")
      df <- data.frame(x = (a*cos(t)) + center[1],
                       y = (a*sin(t)) + center[2]
      )
      return(list(type = "circle",
                  EQ = paste0(P["A",],"*x^2+(",P["B",],")*x*y+(",P["C",],")*y^2+(",P["D",],")*x+(",P["E",],")*y+(",P["F",],")=0"),
                  "x(t)" = paste0(a,"*cos(t) +",center[1]),
                  "y(t)" = paste0(a,"*sin(t) +",center[2]),
                  df = df,
                  center = center,
                  "Input: Section points" = section_points)
      )
      }
  }else {
    return(list(type = "degenerate",
                "Input: Section points" = section_points)
           )
  }
}

#######################################
#' Compute the dot product of two vectors
#'
#' @name math_dot_product
#' @description 'dot' or 'scalar' product of vectors or pairwise columns of matrices.
#' @details Returns the 'dot' or 'scalar' product of vectors or columns of matrices. Two vectors must be of same length, two matrices must be of the same size.
#' If x and y are column or row vectors, their dot product will be computed as if they were simple vectors.
#' @param x Vector or a Matrix of vectors
#' @param y Vector or a Matrix of vectors
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Vector with length of dth dimension
#' @examples
#' math_dot_product(c(1,0,0),c(0.1,5,0))
#' math_dot_product(c(1,0),c(0,1))
#' sqrt(math_dot_product(c(1,1,1), c(1,1,1)))  #=> 1.732051
#' @export

math_dot_product <- function (x, y)
{
  if (length(x) == 0 && length(y) == 0)
    return(0)
  if (!(is.numeric(x) || is.complex(x)) || !(is.numeric(y) ||
                                             is.complex(y)))
    stop("Arguments 'x' and 'y' must be real or complex.")
  x <- drop(x)
  y <- drop(y)
  if (any(dim(x) != dim(y)))
    stop("Matrices 'x' and 'y' must be of same size")
  if (is.vector(x) && is.vector(y)) {
    dim(x) <- c(length(x), 1)
    dim(y) <- c(length(y), 1)
  }
  x.y <- apply(Conj(x) * y, 2, sum)
  return(x.y)
}

#######################################
#' Compute Vector Cross Product
#'
#' @name math_cross_product
#' @description Computes the cross (or: vector) product of vectors in 3 dimensions.
#' In case of matrices it takes the first dimension of length 3 and computes the cross product between corresponding columns or rows.
#' @details The cross product of two vectors is the perpendicular vector to the plane spanned by the given vectors \code{x} and \code{y}.
#' @details <https://en.wikipedia.org/wiki/Cross_product>
#' @param x numeric vector or matrix
#' @param y numeric vector or matrix
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @seealso <math_dot_product>
#' @returns
#' A scalar or vector of length the number of columns of x and y.
#' @examples
#' math_cross_product(c(1,0,0), c(0,1,0))
#' math_dot_product(c(1,0),c(0,1))
#' math_dot_product(c(1,0),c(-1,0))
#' @export

math_cross_product <- function (x, y)
{
  if (!is.numeric(x) || !is.numeric(y))
    stop("Arguments 'x' and 'y' must be numeric vectors or matrices.")
  if (is.vector(x) && is.vector(y)) {
    if (length(x) == length(y) && length(x) == 3) {
      xxy <- c(x[2] * y[3] - x[3] * y[2], x[3] * y[1] -
                 x[1] * y[3], x[1] * y[2] - x[2] * y[1])
    }
    else {
      stop("Vectors 'x' and 'y' must be both of length 3.")
    }
  }
  else {
    if (is.matrix(x) && is.matrix(y)) {
      if (all(dim(x) == dim(y))) {
        if (ncol(x) == 3) {
          xxy <- cbind(x[,2] * y[,3] - x[,3] * y[,2], x[,3] * y[,1] - x[,1] * y[,3], x[,1] * y[,2] - x[,2] * y[,1])
        }
        else {
          if (nrow(x) == 3) {
            xxy <- rbind(x[2, ] * y[3, ] - x[3, ] * y[2,
            ], x[3, ] * y[1, ] - x[1, ] * y[3, ], x[1,
            ] * y[2, ] - x[2, ] * y[1, ])
          }
          else {
            stop("'x', 'y' must have one dimension of length 3.")
          }
        }
      }
      else {
        stop("Matrices 'x' and 'y' must be of same size.")
      }
    }
    else {
      if (is.vector(x) && is.matrix(y) || is.matrix(x) &&
          is.vector(y)) {
        stop("Arguments 'x', 'y' must be vectors/matrices of same size.")
      }
    }
  }
  return(xxy)
}



#######################################
#' Computes intersection point of a line and a sphere surface.
#'
#' @name math_interSec_sph_line
#' @param p position vector for the line  c(x = ,y = ,z = )
#' @param u direction vector for the line c(x = ,y = ,z = )
#' @param m position vector of the centre of the sphere c(x = ,y = ,z = )
#' @param r radius of the sphere
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @seealso <math_dot_product>
#' @returns
#' list of intersecting point(s) (x,y,z) and parameter \code{t}. Parameter \code{t} is giving the distance from \code{p} to \code{m}
#' @examples
#' p <- c(x = 1, y = -2, z = 3)
#' u <- c(x = 1, y = 0, z = 1)
#' m <- c(1,-2,4)
#' r <- 1
#' math_interSec_sph_line(p, u, m, r)
#' @export

math_interSec_sph_line <- function(p, u, m, r) {
  ## Formula
  t_expression <-expression(c((sqrt(((p[1]-m[1])*u[1]+u[1]*(p[1]-m[1])+(p[2]-m[2])*u[2]+u[2]*(p[2]-m[2])+(p[3]-m[3])*u[3]+u[3]*
                                       (p[3]-m[3]))^2-4*(u[1]^2+u[2]^2+u[3]^2)*((p[1]-m[1])^2+(p[2]-m[2])^2+(p[3]-m[3])^2-r^2))-
                                 ((p[1]-m[1])*u[1]+u[1]*(p[1]-m[1])+(p[2]-m[2])*u[2]+u[2]*(p[2]-m[2])+(p[3]-m[3])*u[3]+
                                    u[3] * (p[3] - m[3])))/(2 * (u[1]^2 + u[2]^2 + u[3]^2)),
                              (-((p[1]-m[1])*u[1]+u[1]*(p[1]-m[1])+(p[2]-m[2])*u[2]+u[2]*(p[2]-m[2])+(p[3]-m[3])*u[3]+u[3]*(p[3]-m[3])+
                                   sqrt(((p[1]-m[1])*u[1]+u[1]*(p[1]-m[1])+(p[2]-m[2])*u[2]+u[2]*(p[2]-m[2])+(p[3]-m[3])*u[3]+u[3]*
                                           (p[3]-m[3]))^2-4*(u[1]^2+u[2]^2+u[3]^2)*((p[1]-m[1])^2+(p[2]-m[2])^2+(p[3]-m[3])^2-r^2))))/
                                (2*(u[1]^2 + u[2]^2 + u[3]^2))))

  # Parameter t
  c_result <- eval(t_expression, list(p, u, m, r))
  names(c_result) <- rep("",length(c_result))

  # Intersection points
  l_Result <- list()
  for (ii in 1:length(c_result)) {
    l_Result[[ii]] <- c(p + (c_result[ii] * u),t = c_result[ii])
  }

  if(is.na(c_result)|>sum() == 0){
    return(l_Result)
  }else{
    print(warning())
    return(NULL)
  }
}

p <- c(x = 1, y = -2, z = 3)
u <- c(x = 1, y = 0, z = 1)
m <- c(1,-2,4)
r <- 1

math_interSec_sph_line(p, u, m, r)
