####################################
#' Magnitude of a vector
#'
#' @name math_betrag
#' @description
#' Get the magnitude of a vector of any length.
#'
#' This function is the same as [slvwagner::math_magnitude()]
#' @param x numerical vector
#' @return numerical vector of [length()] one
#' @examples math_betrag(c(1,1))
#' @examples math_betrag(c(1,1,1))
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @export

math_betrag <- function(x) {
  sqrt(sum(x^2))
}

####################################
#' Magnitude of a vector
#'
#' @name math_magnitude
#' @description
#' Get the magnitude of a vector of any length.
#'
#' This function is the same as [slvwagner::math_betrag()]
#' @param x numerical vector
#' @return numerical vector of [length()] one
#' @examples math_magnitude(c(1,1))
#' @examples math_magnitude(c(1,1,1))
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @export
#'
math_magnitude <- function(x){
  sqrt(sum(x^2))
}

#######################################
#' Circle from 3 points
#'
#' @name math_circle_from3points
#' @description
#' calculate circle using 3 points.The function returns a tibble (data frame) or
#' a named vector with the centre point and the radius.
#' @param x matrix with 3 rows of points and 2 columns with the coordinates.
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' Returns data frame with centre coordinates and Radius.
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
#' @description transforms a given vector by a given rotation matrix using matirx multiplication
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
#' rot_matrix <- math_rot_matrix2d(pi/3)
#' p <- cbind(c(1,0), c(0,1), c(1,1))
#' p
#' math_rot_transform(p,rot_matrix)
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
#' Spherical to cartesian coordinates
#'
#' @name math_sph2cart
#' @description Transform spherical to cartesian coordinates according to international physics convention:
#' \eqn{\theta} in range 0...pi (0...180 Deg) and \eqn{\varphi} in range 0...2*pi (0...360Deg)
#' @details <https://de.wikipedia.org/wiki/Kugelkoordinaten#Umrechnungen>
#' @param tpr c(\eqn{tilt = \theta}, \eqn{pan = \varphi}, radius = r) as vector or matrix
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
#' @description Transform Cartesian to spherical coordinates according to international physics convention:
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
#' Vector with length of input dimension
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
#' round polynomials
#'
#' @name math_polynom_round
#' @description The polynomial will be rounded according to \code{round_digits}.
#' @param poly polynomial character string
#' @param round_digits the number of digit used to round the polynomial coefficients
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' polynomial
#' @examples
#' "-x^5+0.0000000001*x^3+0.9999999999*x^2+x-0.2"|>math_polynom_round()
#' "0.1*x^5+0.0000000001*x^3+0.9999999999*x^2+x-0.2"|>math_polynom_round()
#' @export

math_polynom_round <- function(poly,round_digits = 9) {
  c_degree <- paste0("Degree(",poly,",","x",")")|>
    Ryacas::ysym()|>
    Ryacas::as_r()

  c_coef <- rep("",as.integer(c_degree))

  c_coef <- paste0("Coef(",poly,",x,0 .. ",c_degree,")")|>
    Ryacas::ysym()|>
    Ryacas::as_r()

  # Round coefficients
  c_coef <- c_coef|>
    as.numeric()|>
    round(round_digits)|>
    as.character()
  c_coef

  return(polynom::as.polynomial(c_coef))
}


#######################################
#' Compute polynomial from roots
#'
#' @name math_polynom_from_roots
#' @description The polynomial will be calculated according to the roots supplied in the list.
#' If roots are complex it will also include the complex conjugated root.
#' The resulting polynomial will be rounded according to \code{round_digits}.
#' @param roots list of roots
#' @param round_digits the number of digit used to round the polynomial coefficients
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' polynomial character string
#' @examples
#' c(-5,0,2i)|>math_polynom_from_roots()
#' c(-5,0,2i,-2i)|>math_polynom_from_roots()
#' c(3,2+1i)|>math_polynom_from_roots()
#' c(3,2+1i,2-1i)|>math_polynom_from_roots()
#' @export

math_polynom_from_roots <- function(roots,round_digits=9){
  ###################################################################
  # convert to list
  roots <- roots|>
    lapply(function(x){
      if(Im(x)==0) return(Re(x))
      else x
    })
  ###################################################################
  # Function to remove complex conjugated from roots list
  remove_complex_conjugated <- function(x) {
    # add complex conjugated to list
    add_Conj <- function(x){
      x|>lapply(function(x){
        if(is.complex(x)){
          c(x,Conj(x))
        }else x
      })
    }
    # Sort
    y <- add_Conj(x)|>
      lapply(function(x){
        sort(x)
      })
    # check if complex
    c_select <- y|>lapply(function(x){
      is.complex(x)
    })|>
      unlist()
    c_complex <- (1:length(x))[c_select]
    # find complex conjugated from list if already in the list
    l_matches <- list()
    cnt <- 1
    for (ii in c_complex){
      c_compare <- c_complex[c_complex != ii]
      for (jj in c_compare) {
        if(sum(unlist(y[ii]) == unlist(y[jj]))>0){
          c_match <- sort(c(ii,jj))
          if(length(l_matches)>0){ # check if matches to compare are available
            do_break <- F
            for (kk in 1:length(l_matches)) { # compare matches with actual match
              if(sum(l_matches[[kk]] == c_match) > 0){
                do_break <- T
                break
              }
            }
            if(do_break) { # if found brake
              break
            }
            else{ # if no match add to match list
              l_matches[[cnt]] <- c_match
              cnt <- cnt+1
            }
          }else{  # if no entry add to match list
            l_matches[[cnt]] <- c_match
            cnt <- cnt+1
          }
        }
      }
    }
    # remove found complex conjugated from list
    c_remove <- l_matches|>
      lapply(function(x){
        unlist(x)[2]
      })|>
      unlist()
    x[c_remove] <- NULL
    return(x)
  }
  ###################################################################
  # Function to add brackes
  add_brackets <- function(x)paste0("(",x,")")


  ###################################################################
  # Polynomial from roots
  ###################################################################
  # remove complex conjgated roots from roots list
  roots <- remove_complex_conjugated(roots)
  # Re/Im Factors
  c_factors_re <- list()
  c_factors_im <- list()
  cnt_im <- 1
  cnt_re <- 1
  # Factor and simplify to get the polynomial
  for (ii in 1:length(roots)) {
    if(is.complex(roots[[ii]])){ # is the root complex?
      input_Re <- Re(roots[[ii]])
      input_Im <- Im(roots[[ii]])|>abs()

      c_real <- ifelse(input_Re >= 0,
                       paste0("x-",input_Re),
                       paste0("x+",abs(input_Re)))|>
        add_brackets()
      # imaginary part and conjugated imaginary part
      c_factors_im[[cnt_im]] <- paste0("Expand((",c_real,"-I*",input_Im,")*(",c_real,"+I*",input_Im,"))")|>
        Ryacas::yac_str()|>
        add_brackets()
      cnt_im <- cnt_im+1

    }else{ # only real
      c_factors_re[[cnt_re]] <- ifelse(roots[[ii]] >= 0,
                                       paste0("x-",roots[[ii]]),
                                       paste0("x+",abs(roots[[ii]])))|>
        add_brackets()
      cnt_re <- cnt_re+1
    }
  }
  # select the imaginary terms
  c_select <- c_factors_im|>
    lapply(function(x)!is.null(x))|>
    unlist()
  # convert to polynomial
  if(length(c_factors_im[c_select])==0){
    c_poly <- paste0("Expand(",c_factors_re|>paste0(collapse = "*"),")")|>
      Ryacas::yac_str()
    c_poly <- paste0("Simplify(",c_poly,")")|>
      Ryacas::yac_str()
    return(math_polynom_round(c_poly,round_digits))
  }else{
    c_poly <- paste0("Expand(",c_factors_re|>paste0(collapse = "*"),"*",c_factors_im[c_select]|>unlist()|>paste0(collapse = "*"),")")|>
      Ryacas::yac_str()
    c_poly <- paste0("Simplify(",c_poly,")")|>
      Ryacas::yac_str()
    return(math_polynom_round(c_poly,round_digits))
  }
}


#######################################
#' Generate non-linear vector
#'
#' @name math_nonlinear_vector
#' @description
#' Function returns a vector of length \code{n}. The first value of the returned vector is \code{c_start} and the last value is \code{c_end}.
#' The calculation is done accordingly: \eqn{c_{end}=c_{start}a^{n−1}} => \eqn{a=\frac{c_{end}}{c_{start}}^{\frac{1}{n-1}}}
#' so the returned vector will be calculated with: \eqn{c_{start}\cdot a^{(0:(n-1))}}
#' @param c_start value of first value of returned vector
#' @param c_end value of last index of returned vector
#' @param n length of returned vector
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' vector of length \code{n}
#' @examples
#' math_nonlinear_vector(1000,100000,8)
#' math_nonlinear_vector(1000,100000,8)|>plot()
#' @export

math_nonlinear_vector <- function(c_start, c_end, n){
  a <- (c_end/c_start)^(1/(n-1))
  return(c_start*a^(0:(n-1)))
}

#######################################
#' Polynomial coefficients
#'
#' @name math_polyCoef
#' @description
#' Calculates the polynomial coefficients from a polynomial \code{equation}. The variable \code{var} can be chosen.
#' The order of the terms does not matter.
#' @param equation polynomial equation string
#' @param variable of the \code{equation}
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @returns
#' vector with polynomial coefficients.
#' @examples
#' "-20*x + 4*x^2 + 5*x^3 + x^4"|>math_polyCoef()
#' "4*x^2 -  20*x + 5*x^3 + x^4"|>math_polyCoef()
#' "4*x^8 -  5*x^3 + x^4-0.2"|>math_polyCoef()
#' "3*z^3"|>math_polyCoef("z")
#' @export

math_polyCoef <- function(equation, variable = "x"){
  deg <- Ryacas::yac_str(paste0("Degree(",equation,")"))|>as.integer();
  Ryacas::yac_str(paste0("Coef(",equation,",",variable,",{",paste0(0:deg,collapse = ","),"})"))|>
    Ryacas::as_r()|>
    as.numeric()
}

