% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math.R
\name{math_polynom_round}
\alias{math_polynom_round}
\title{round polynomials}
\usage{
math_polynom_round(poly, round_digits = 9)
}
\arguments{
\item{poly}{polynomial character string}

\item{round_digits}{the number of digit used to round the polynomial coefficients}
}
\value{
polynomial
}
\description{
The polynomial will be rounded according to \code{round_digits}.
}
\examples{
"-x^5+0.0000000001*x^3+0.9999999999*x^2+x-0.2"|>math_polynom_round()
"0.1*x^5+0.0000000001*x^3+0.9999999999*x^2+x-0.2"|>math_polynom_round()
}
\author{
Florian Wagner
\email{florian.wagner@wagnius.ch}
}
