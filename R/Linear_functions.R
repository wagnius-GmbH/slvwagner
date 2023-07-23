#######################################
#' linear function with parameter mx+b
#'
#' @name lf
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
#' lf(-12:10,0.8,3.265)
#' plot(lf(-12:10,0.8,3.265))
#' @export

lf <- function(x,m,b){
  return(m*x+b)
}

#######################################
#' linear function but with data frame instead of single parameters
#'
#' @name lf_df_mb
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
#' lf_df_mb(0:10,df)
#' df <- data.frame(slope = c(0.5,1,-0.5), intercept = c(1,-10,0))
#' lf_df_mb(10,df)
#' @export


lf_df_mb <- function(x,df_mb){
  x*df_mb[,"slope"]+df_mb[,"intercept"]
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
#' y <- y|>
#'   rbind(data.frame(slope = 1.25, intercept = 2))
#' lf_intersect(y)
#' names(y)<-c("slope","intercept")
#' lf_intersect(y)
#' lf_intersect(data.frame(slope = c(0.2,-1),intercept = c(2,3)))
#' lf_intersect(matrix(c(0.2,-1,2,3),ncol = 2))
#' y <- matrix(c(0.2,-1,2,3),ncol = 2)
#' lf_intersect(y)
#' @export

lf_intersect <- function(x,...){
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
      stop("x contains only a singl function")
    }
  }else{
    if(is.null(names(x))){
      if(x[1] == ...[1]){
        stop("x and y are slopes identical: cannot calculate interception point")
      }else{
        result_x = (...[2]-x[2])/(x[1]-y[1])
        result_y = ...[1]*result_x+...[2]
        return(c(x = result_x, y = result_y))
      }
    }else{
      if(x[1] == ...[1]){
        stop("x and y slopes are identical: cannot calculate interception point")
      }else{
        result_x = (...["intercept"]-x["intercept"])/(x["slope"]-...["slope"])
        result_y = ...["slope"]*result_x+...["intercept"]
        return(c(result_x, result_y))
      }
    }
  }
}
