#' Implement local linear regression with a Gaussian kernel,
#' as a function of tuning parameter lambda.

#' Weighted Least Squares
#'
#' Analytic solution using Strang 5th ed.
#'
#' @param X feature matrix without intercept column
#' @param y target vector
#' @param w weight vector
#' @return coefficients theta
#'
#' @export
wls <- function(x,y, w){
    X <- cbind(1, x)

    A <- t(X) %*% diag(w) %*% X
    b <- t(X) %*% diag(w) %*% y
    theta <- solve(A) %*% b
    
    return(list(theta=theta))
}

#' Gaussian Kernel
#'
#' Solution using HT Ch. 6 Kernel Smoothing Methods (6.23)
#'
#' @param x feature vector
#' @param mu mean
#' @param lambda adjustable parameter, usually standard deviation
#' @return transformed feature vector using gaussian distribution
#'
#' @export
gauss_kern <- function(x, mu, lambda){
    w <- exp(- (x - mu)^2 / (2 * lambda^2) ) / (lambda * sqrt(2 * pi))
    return(c(w))
}

#' Local Linear Regression
#'
#' Referencing HT (6.7)
#'
#' @param X attribute vector
#' @param y target vector
#' @return y hat target vector
#'
#' @export
local_lm <- function(X, y, lambda=1.5){

    pred <- c()
    
    for (i in 1:length(y)){

        w <- gauss_kern(X, X[i], lambda)
        reg <- wls(x,y, w)
        theta <- reg$theta
        pred <- c(pred, theta[1,] + theta[2,] * X[i])
    }
    return(pred)
}
