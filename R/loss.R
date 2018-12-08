#' Loss functions from hw1 -- related to linear regression
#' equations provided in question 2.


#' Compute the Quadratic Loss
#' 
#' Related to Mean Squared Error, L2 norm
#'
#' @param e vector of error terms Yi - Y^hat_i
#'
#' @return quadratic loss error
#'
#' @examples
#' e <- matrix(data=0, nrow=10,ncol=1)
#' quadratic_loss(e)
#'
#' e <- rnorm(10, 0, 1)
#' quadratic_loss(e)
#'
#' @export                                     
quadratic_loss <- function(e, delta=0.0){ sum(e^2) }


#' Compute Mean Absolute Error (L1 norm)
#'
#' @param e vector of error terms Yi - Y^hat_i
#'
#' @return mean absolute error
#'
#' @examples
#' e <- matrix(data=0, nrow=10,ncol=1)
#' mae(e)
#'
#' e <- rnorm(10, 0, 1)
#' mae(e)
#'
#' @export                                     
mae <- function(e, delta=0.0){ sum(abs(e)) }

#' Conditional for Huber Loss function
#' 
#' Determines which summation will be used
#' within the Huber Loss function. Meant for
#' use with \code{\link{sapply}} and
#' \code{\link{huber_loss}}.
#'
#' @param i ith value of a one dimensional vector
#' @param delta const parameter for determining conditional
#'
#' @return boolean vector for conditional statement
#'
#' @examples
#' huber_cond(0, 0.01) == TRUE
#'
#' @export
huber_cond <- function(i, delta) { all(abs(i) <= delta) }

#' Huber Loss function (smooth mean absolute error) with parameter delta
#' 
#' @param e vector of error terms Yi - Y^hat_i
#' @param delta const for determining huber loss conditional
#'
#' @return huber loss function error
#' 
#' @examples
#' e <- matrix(data=0, nrow=10,ncol=1)
#' delta <- 1.0
#' huber_loss(e, delta)
#'
#' e <- rnorm(10, 0, 1)
#' huber_loss(e, delta)
#'
#' @export
huber_loss <- function(e, delta){

    fil <- sapply(e, huber_cond, delta)

    l <- sum((1/2) * e[fil]^2)
    l <- l + sum(delta * abs(e[!fil]) - (1/2) * delta^2)

    return(l)
}



