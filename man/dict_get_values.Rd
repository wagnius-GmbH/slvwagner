% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dict.R
\name{dict_get_values}
\alias{dict_get_values}
\title{Get values from fast dictionary}
\usage{
dict_get_values(
  x,
  pos = -1L,
  envir = as.environment(pos),
  mode = "any",
  inherits = TRUE
)
}
\arguments{
\item{x}{key (character)}

\item{pos}{= -1L}

\item{envir}{= as.environment(pos)}

\item{mode}{= "any"}

\item{inherits}{= TRUE}
}
\value{
named vector
}
\description{
The reason for using dictionaries in the first place is performance.
Although it is correct that you can use named vectors and lists for the task,
the issue is that they are becoming quite slow and memory hungry with more data.
Yet what many people don't know is that R has indeed an inbuilt dictionary data structure
environments with the option hash = TRUE
}
\details{
\url{https://blog.ephorie.de/hash-me-if-you-can}
}
\examples{
df <- data.frame(key   = c("ch","se","de","it"),
                 value = c(41L,46L,49L,39L))
# initialize hash
hash <- dict_from_data.frame(df)
dict_get_values(c("ch", "it"), hash)
}
\author{
Florian Wagner
\email{florian.wagner@wagnius.ch}
}
