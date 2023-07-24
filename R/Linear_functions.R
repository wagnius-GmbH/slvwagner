#######################################
#' Linear function: \eqn{f(x) = y = mx+b}
#'
#' @name lf_lf
#' @description
#' linear function with parameter slope \code{m} and intercept \code{b}.
#' @details
#' The function calculates \code{y} for a vector of \code{x}.
#' @param x vector
#' @param parameter Either vector, matrix or data.frame with parameter c(\code{m},\code{b}) or just the slope \code{m}. It is also possible to supply a matrix as a set of parameter where each row represents c(\code{m},\code{b}).
#' @param ... intercept \code{b}
#' @return vector \code{x}
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples
#' lf_lf(-5:5,0.8,3.265)
#' lf_lf(-5:5,c(1,3))
#' df <- data.frame(x = c(0.5,1,-0.5), y = c(1,0,-1))
#' lf_lf(-5:5,df)
#' @export

lf_lf <- function(x, parameter, ...) {
  lf_ <- function(x,m,b) m*x+b
  if (is.null(nrow(parameter))) {
    if (missing(...)) {
      return(lf_(x,parameter[1],parameter[2]))
    } else{
      return(parameter * x + ...)
    }
  }else{
    apply(parameter, 1, function(y){
      y_ <- as.vector(y)
      lf_(x,y_[1],y_[2])
    })
  }
}


#######################################
#' Find linear function perpendicular to linear function through given point
#'
#' @name lf_perpendicular
#' @description
#' Find linear function`s perpendicular to supplied linear function through given point \code{x}.
#' @param x point vector
#' @param lf data.frame with the definiton of the given linear function containing the column slope and intercept
#' @return data.frame with the slope and intercept
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples
#' lf_perpendicular(x = c(2,-9),
#'                       data.frame(slope = -3,intercept = 5))
#' @export

lf_perpendicular <- function(x,lf){ #x point(s) lf(intercept, slope)
  s=-1/lf$slope
  i =x[2]-(lf_rev_slope(lf$slope)*x[1])
  return(data.frame(slope = s,intercept = i))
}

#######################################
#' find perpendicular slope to linear function
#'
#' @name lf_rev_slope
#' @description
#' find perpendicular slope to linear function
#' @param slope The slop or m of linear function f(x)= mx+b
#' @return data.frame(x,y) \code{x}
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples
#' lf_rev_slope(c(0.1,-0.89))
#' @export
#senkrechte zu einer geraden
lf_rev_slope <- function(slope){#
  return(-1/slope)
}

#######################################
#' linear function from 2 points
#'
#' @name lf_fromPoints
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
#' lf_fromPoints(x)
#'
#' result <- lf_fromPoints(y)
#'
#' library(tidyverse)
#' ggplot(as_tibble(t(y)),aes(x,y))+
#' geom_abline(data = result,aes(slope = slope, intercept =intercept), size = 1.5)+
#' geom_point(shape = 10, size = 10,color = "blue")+
#' geom_hline(yintercept = 0)+
#' geom_vline(xintercept = 0)
#' @export

lf_fromPoints <- function (x){
  delta <- (x[,1]-x[,2])
  m <- delta[2]/delta[1]
  b <- x[2,1]-m*x[1,1]
  return(data.frame(slope = m,intercept = b))
}

#######################################
#' Intersection point from two linear functions
#'
#' @name lf_intersect
#' @description
#' Calculate intersecting point(s) from two linear function \code{x} and linear function \code{y}.
#'
#' @details
#' Get the intersecting point of two linear Functions. If only \code{x} is supplied, \code{x} needs to be a matrix or data.frame containing both
#' linear functions. If \code{x} and or \code{y} have names you shall use the names (slope, intercept).
#' @param x vector, matrix or data frame with slope and intercept
#' @param ... vector with slope and intercept
#'
#' @return vector
#' @author Florian Wagner
#' \email{florian.wagner@wagnius.ch}
#' @examples
#' x <- c(slope = 0.5, intercept = 1)
#' y <- c(slope = -1, intercept = 2)
#' lf_intersect(x,y)
#' rbind(x,y)|>lf_intersect()
#' matrix(c(x,y),ncol = 2, byrow = TRUE)|>lf_intersect()
#' matrix(c(0.2,-1,2,3),ncol = 2)|>lf_intersect()
#' @export

lf_intersect <- function(x, ...) {
  if (missing(...)) {
    x <- rbind(x,...)
    if (nrow(x) > 1) {
      if (x[1, 1] != x[2, 1]) {
        result_x = (x[2, 2] - x[1, 2]) / (x[1, 1] - x[2, 1])
        result_y = x[2, 1] * result_x + x[2, 2]
        return(c(x = result_x, y = result_y))

      } else{
        stop("x and y are slopes identical: cannot calculate interception point")
      }
    } else{
      stop("x contains only a single function")
    }
  }
}

