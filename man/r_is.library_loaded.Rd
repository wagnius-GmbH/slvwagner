% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{r_is.library_loaded}
\alias{r_is.library_loaded}
\title{Check if package is loaded in R`s global environment.}
\usage{
r_is.library_loaded(package_name)
}
\arguments{
\item{package_name}{package name character vector}
}
\value{
TRUE or FALSE
}
\description{
Function to Check if package is loaded R`s global environment.
}
\examples{
r_is.library_loaded("ThisIsNotAlib")
library("slvwagner")
r_is.library_loaded("slvwagner")
}
