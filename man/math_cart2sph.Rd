% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math.R
\name{math_cart2sph}
\alias{math_cart2sph}
\title{Cartesian to spherical coordinates}
\usage{
math_cart2sph(xyz, DEG = FALSE)
}
\arguments{
\item{xyz}{c(x, y, z) cartesian coordinates as vector or matrix}

\item{DEG}{return angles \eqn{\theta} and  \eqn{\varphi} in degree instead of radians}
}
\value{
named vector or matrix
}
\description{
Transform Cartesian to spherical coordinates according to international physics convention:
\eqn{\theta} in range \eqn{0...\pi} (0...180 Deg) and \eqn{\varphi} in range \eqn{0...2\pi} (0...360Deg)
}
\details{
\url{https://de.wikipedia.org/wiki/Kugelkoordinaten#Umrechnungen}
}
\examples{
math_cart2sph(c(1,1,1), DEG = TRUE)
rbind(c(1,0.5,0.1),
      c(1,0.2,0.6))|>
   math_cart2sph()
}
\author{
Florian Wagner
\email{florian.wagner@wagnius.ch}
}
