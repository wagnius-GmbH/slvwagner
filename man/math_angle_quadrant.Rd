% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math.R
\name{math_angle_quadrant}
\alias{math_angle_quadrant}
\title{Find quadrant of matrix or data frame}
\usage{
math_angle_quadrant(x)
}
\arguments{
\item{x}{vector or matrix containing the coordinates. First x, second y.}
}
\value{
Returns Quadrant(s) of vector(s)
}
\description{
Find the angle(s) of vector(s) using the quadrant information.
}
\examples{
m <- matrix(c(c(1,-1,-1, 1,0,1),
            c(1, 1,-1,-1,1,0)),
            ncol = 2, byrow = FALSE)
math_angle_quadrant(m)/pi*180
data.frame(x = c(1,-1),y = c(1,-1))|>math_angle_quadrant()
math_angle_quadrant(c(1,0))
math_angle_quadrant(c("1.25","test"))
}
\author{
Florian Wagner
\email{florian.wagner@wagnius.ch}
}