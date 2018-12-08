#' Functions associated with ridge regression

#' Analytic solution for Ridge Regression
#'
#' @param X attribute matrix with intercept coefficient
#' @param y target vector
#' @param lambda regularization parameter
#' @return list with theta vector
#'
#' @export
analyticridge <- function(X, y, lambda){
    theta <- solve(t(X) %*% X + lambda * diag(ncol(X))) %*% t(X) %*% y
    return(list(theta=theta))
}


