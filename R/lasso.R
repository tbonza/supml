#' Functions associated with lasso regression
#'
#' https://www.coursera.org/lecture/ml-regression/deriving-the-lasso-coordinate-descent-update-6OLyn
#'


#' Lasso Gradient
#'
#' For use in gradient descent
#'
#' @param X attribute vector
#' @param y target vector
#' @param theta theta vector 
#' @param alpha learning rate constant
#' @param ... (lambda) include lambda regularization parameter
#'
#' @return theta vector
#'
#' @export
lasso_gradient <- function(X, y, theta, alpha, ...){

    lambda <- list(...)$lambda # approach keeps the api consistent

    least_squares_subgradient <- lsg(X, y, theta)
    
    l1_subgradient <- (-lambda) * sign(theta)
 
    step <- alpha * (least_squares_subgradient + alpha * l1_subgradient)

    return(step)
}
