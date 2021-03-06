% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loss.R
\name{quadratic_loss}
\alias{quadratic_loss}
\title{Loss functions from hw1 -- related to linear regression
equations provided in question 2.
Compute the Quadratic Loss}
\usage{
quadratic_loss(e, delta = 0)
}
\arguments{
\item{e}{vector of error terms Yi - Y^hat_i}
}
\value{
quadratic loss error
}
\description{
Related to Mean Squared Error, L2 norm
}
\examples{
e <- matrix(data=0, nrow=10,ncol=1)
quadratic_loss(e)

e <- rnorm(10, 0, 1)
quadratic_loss(e)

}
