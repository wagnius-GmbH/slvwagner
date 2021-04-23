####################################
#' Magnitude of a vector
#'
#' Get the magnitude of a vector of any lenght
#' @name math_betrag
#' @description Get the magnitude of a vector of any length

#' @param x numerical vector
#' @return numerical vector of length one
#' \code{x}
#' @examples math_betrag(c(1,1))
#' @examples math_betrag(c(1,1,1))
#' @author Florian Wagner
#' @export


math_betrag <- function(x) {
  sqrt(sum(x^2))
}

#######################################
#' Circle from 3 points
#'
#' get center and radius from point matrix
#' @name math_circle_from3points
#' @description
#' Get the circle using 3 points.The function returns a tibble (dataframe) or
#' a named vector with the center point and the radius.
#' @param x numerical point matrix
#' @param type c("tibble","vector")  to choose the return class
#' @author Florian Wagner
#' @returns
#' tibble or vector
#' \code{x}
#' @examples
#' Matrix
#' x <- matrix(c(c(-2.23 , 4.389 ),
#'               c( 1.001,-3.23  ),
#'               c(  15.5,-7.2365)),
#'             nrow = 3, byrow = TRUE)
#'
#' math_circle_from3points(x)
#'
#' math_circle_from3points(x, type = "vector")
#' @export

math_circle_from3points<-function(x,type = "tibble"){
  if(sum(class(A)==c("matrix","array"))==2){
    A <- cbind(c(1,1,1), x)
    b <- c(-(A[1,2]^2+A[1,3]^2),
           -(A[2,2]^2+A[2,3]^2),
           -(A[3,2]^2+A[3,3]^2))

    c_result <- solve(A,b)
  }else{
    return(writeLines(past("only point matrix can be calculated => matrix[3][2] 3points and 2 coordinates")))
  }
  if(type == "tibble"){
    return(tibble(x_center = -c_result[2]/2,
                  y_center = -c_result[3]/2,
                  r  = sqrt((-c_result[2]/2)^2+(-c_result[3]/2)^2- c_result[1]))
    )
  }else{
    if(type == "vector"){
      c(x_center = -c_result[2]/2,
        y_center = -c_result[3]/2,
        r = sqrt((-c_result[2]/2)^2+(-c_result[3]/2)^2- c_result[1]))
    }else{
      writeLines("unknown type")
    }
  }
}


#######################################
#' Angle between 2 vectors
#'
#' @name math_inbetweenAngle
#' @description
#' Get the angle between 2 vectors. Returns the smallest Angle between 2 vectors (scalar) in radiant.
#' @param u fist vector
#' @param v second vector
#' @author Florian Wagner
#' @returns
#' Returns the smallest Angle between 2 vectors (scalar) in radiant.
#' \code{x}
#' @examples
#' 2D
#' u <- c(-0,1)
#' v <- c(1,0)
#' math_inbetweenAngle(u,v)
#'
#'3D
#' u <- c(-12,13, -2.56)
#' v <- c(3,5,-100)
#' math_inbetweenAngle(u,v)
#' @export

math_inbetweenAngle <- function(u,v){
  return(acos(sum(u*v)/(sqrt(sum(u^2))*sqrt(sum(v^2)))))
}
