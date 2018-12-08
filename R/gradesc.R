#' Gradient descent algorithms
#' 
#' Meant for use with loss functions in supml


#' Batch Gradient Descent
#' 
#' @param alpha learning rate constant
#' @param X matrix of predictors including 1 for beta 0
#' @param y vector of dependent variable
#' @param grad gradient function
#' @param N maximum number of iterations
#' @param precision constant used for approximation condition
#'
#' @return list object with theta, convergence status, and number of iterations
#'
#' @examples
#' x <- runif(1000, -5, 5)
#' y <- x + rnorm(1000) + 3
#' X <- cbind(1, matrix(x))
#' h <- bgd(0.1, X, y, least_squares_gradient, 1000)
#' h$theta
#' h$converged == TRUE
#' h$iterations
#'
#' @export
bgd <- function(alpha, X, y, grad, N, precision=0.0001, ...){

    theta <- matrix(data=0.0, nrow=ncol(X), ncol=1)
    prev_theta <- theta
    converged <- FALSE
    iterations <- 0

    for (i in 1:N){
        g <- grad(X, y, theta, alpha, ...)
        
        theta <- theta - g

        if(any(is.na(theta))){
            theta[is.na(theta)] <- 0
        }

        if(all(abs(prev_theta - theta) <= precision)){
            converged <- TRUE
            return(list(theta=theta, converged=converged, iterations=iterations))
        }
        
        prev_theta <- theta
        iterations = iterations + 1
    }

    return(list(theta=theta, converged=converged, iterations=iterations))
}

#' Stochastic Gradient Descent
#' 
#' @param alpha learning rate constant
#' @param X matrix of predictors including 1 for beta 0
#' @param y vector of dependent variable
#' @param grad gradient function
#' @param N maximum number of iterations
#' @param precision constant used for approximation condition
#'
#' @return list object with theta, convergence status, and number of iterations
#'
#' @examples
#' x <- runif(1000, -5, 5)
#' y <- x + rnorm(1000) + 3
#' X <- cbind(1, matrix(x))
#' h <- sgd(0.1, X, y, least_squares_gradient, 1000)
#' h$theta
#' h$converged == TRUE
#' h$iterations
#'
#' @export
sgd <- function(alpha, X, y, grad, N, precision=0.0001){
    
    theta <- matrix(data=0.0, nrow=ncol(X), ncol=1)
    prev_theta <- theta
    converged <- FALSE
    iterations <- 0

    shuffle <- sample(1:length(y))
    Xs <- X[shuffle, ]
    ys <- y[shuffle]

    for (i in 1:N){
        g <- grad(Xs, ys, theta, alpha)
        theta <- theta - g

        if(any(is.na(theta))){
            theta[is.na(theta)] <- 0
        }

        if(all(abs(prev_theta - theta) <= precision)){
            converged <- TRUE
            return(list(theta=theta, converged=converged, iterations=iterations))
        }
        prev_theta <- theta
        iterations = iterations + 1

        shuffle <- sample(1:length(y))
        Xs <- Xs[shuffle, ]
        ys <- ys[shuffle]
    }

    return(list(theta=theta, converged=converged, iterations=iterations))
}

