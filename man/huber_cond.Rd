% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loss.R
\name{huber_cond}
\alias{huber_cond}
\title{Conditional for Huber Loss function}
\usage{
huber_cond(i, delta)
}
\arguments{
\item{i}{ith value of a one dimensional vector}

\item{delta}{const parameter for determining conditional}
}
\value{
boolean vector for conditional statement
}
\description{
Determines which summation will be used
within the Huber Loss function. Meant for
use with \code{\link{sapply}} and
\code{\link{huber_loss}}.
}
\examples{
huber_cond(0, 0.01) == TRUE

}
