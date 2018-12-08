
#' Least squares objective
least_squares <- function(X, y, theta){
    (0.5/2 * length(y)) * sum((t(X) %*% theta - y)^2)
}

#' Least squares gradient
#' @export
least_squares_gradient <- function(X,y,theta, alpha){
    alpha * ((t(X) %*% (X %*% theta - y)) / length(y))
}

#' Least squares gradient with different handling of alpha
lsg <- function(X,y,theta){
    ((t(X) %*% (X %*% theta - y)) / length(y))
}


#' Least Squares with Quadratic Loss gradient
#' 
#' Related to Mean Squared Error, L2 norm
#' 
#' @export
least_squares_ql_gradient <- function(X, y, theta, alpha){
    alpha * (lsg(X, y, theta) + 2 * alpha * theta)
}

#' Least Squares with Mean Absolute Error (L1 norm) gradient
#'
#' @export
least_squares_l1_gradient <- function(X, y, theta, alpha){
    alpha * (lsg(X, y, theta) + alpha * 1)
}

#' Least Squares with Huber Loss Function gradient
#'
#' @export
least_squares_huber_gradient <- function(X, y, theta, alpha, ...){

    delta <- list(...)$delta # approach keeps the api consistent

    if (huber_cond(theta, delta)){
        alpha * (lsg(X, y, theta) + alpha * theta)
    }
    else {
        alpha * (lsg(X, y, theta) + alpha * delta)
    }
}


#' Analytic solution for least squares
#'
#' @param X attribute matrix with intercept coefficient
#' @param y target vector
#' @return list with theta vector
#'
#' @export
analyticlm <- function(X, y){
    theta <- solve(t(X) %*% X) %*% t(X) %*% y
    return(list(theta=theta))
}
