% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom.R
\name{geometcdf}
\alias{geometcdf}
\title{Geometric cumulative density function}
\usage{
geometcdf(p, x, less = TRUE)
}
\arguments{
\item{p}{probability of success for an independent trial}

\item{x}{number of trials}

\item{less}{boolean TRUE if probability of choosing less than x}
}
\value{
cumulative probability of geometric series
}
\description{
Geometric cumulative density function
}
\examples{
geometcdf(1,1)
geometcdf(1/13, 9)
geometcdf(1/13, 12)

}
