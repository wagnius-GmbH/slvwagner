% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat.R
\name{stat_norm_dist}
\alias{stat_norm_dist}
\title{Probability density function}
\usage{
stat_norm_dist(x, mu, sd)
}
\arguments{
\item{x}{data}

\item{mu}{mean}

\item{sd}{standard deviation}
}
\value{
vector
}
\description{
Probability density function with parameter sigma(sd,Standard deviation) and mu(mean)
}
\examples{
c_seq <- seq(-6,6,0.1)
plot(x = c_seq, stat_norm_dist(c_seq,mu = 0, sd = 1), type = "l")
}
\author{
Florian Wagner
\email{florian.wagner@wagnius.ch}
}
