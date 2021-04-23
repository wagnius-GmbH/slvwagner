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
#' Get the angle between 2 vectors. Returns the smallest Angle between 2 vectors in radiant.
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
#' @
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
#' The centrer point is used to for linear function through x1 and cp as well as for linear function through x2 and cp.
#' The common point therefore is cp and will be used as position vector.
#' @param  R radius
#' @param  x1 first point vector
#' @param  x2 second point vector
#' @param  cp position vector
#' @param  nb_points cout of points to generate each radius vector
#' @author Florian Wagner
#' @returns
#' Returns point matrix with the coordinates for every generated point \code{x}
#' @examples
#' Testing
#' m <- math_slerp(R  =1,
#'                x1 = c(-10,-5,50),
#'                x2 = c(-20,10,2),
#'                cp = c(10,-10,10),
#'                nb_points = 10)
#'
#' Plot 3D
#' library(rgl)
#' plot3d( m[,1], m[,2], m[,3], type = "p", lwd = 2, top = TRUE,
#'         #col = rainbow(nrow(m)),
#'         aspect = c(diff(c(min(m[,1]),max(m[,1]))),
#'                    diff(c(min(m[,2]),max(m[,2]))),
#'                    diff(c(min(m[,3]),max(m[,3])))
#'                    ))
#' @export

math_slerp <- function(R,x1,x2,cp,nb_points = 10) { #slerp aus drei Punkten, Radius, Punktanzahl
  #(CenterPoint ist der Ortsvektor)
  #Verschieben des Koordinatensystems: Neuer Ursprung cp
  sp <- x1-cp
  cp <- cp-cp
  ep <- x2-cp
  #Stuetzvektoren
  s1l   <- sp-cp #Stuetzvektor aus Ortsvektor und Startpunkt
  s2l   <- ep-cp #Stuetzvektor aus Ortsvektor und Endpunkt
  #Stuetzvektoren gleich gross machen
  s1 <- s1l*math_betrag(s2l)
  s2 <- s2l*math_betrag(s1l)
  #Skallierung der Stuetzvektoren auf Betrag = 1 =>Einheisvektoren
  skalierung <- math_betrag(s1)
  s1 <- s1/skalierung
  s2 <- s2/skalierung
  #Zwischenwinkel der beiden Vektoren
  phi<-math_inbetweenAngle(s1,s2)
  print(paste0("Zwischenwinkel s1,s2: ",round((phi/pi)*180,digits = 3),"°"))
  ###############################################################################
  #Slerp Einheitsvektoren mit s1,s2
  t <- seq(0,1,1/nb_points)#Zahl zwischen Null und eins 0...1 die den Winkel aufteilt
  slerp <- data.frame(x=t,y=t,z=t)
  slerp_transposed <- t(slerp)
  for(i in 1:length(t)){
    slerp_transposed[,i] <- (sin((1-t[i])*phi)/sin(phi))*(s1)+(sin(t[i]*phi)/sin(phi)*(s2))
    #slerp_transposed[,i] <- slerp_transposed[,i]+cp_input
  }
  #Vektoren strecken mit vorgegebenen Radius R
  slerp <- t(slerp_transposed*R)
  #Data conversion
  df_points <- matrix(c(cp),ncol = 3,byrow = TRUE)
  names(df_points) <- paste0("x",1:ncol(df_points))
  #Daten zusammenfuegen
  df_points <- rbind(df_points,slerp)
  #Koordinaten auf cp zurueckverschieben
  for(i in 1:nrow(df_points)){
    df_points[i,]<-df_points[i,]+cp
  }
  return(df_points)
}

