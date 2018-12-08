#' Implementations for Logistic Regression


#' Compute yhat for logistic regression
#'
#' @param w vector of weights
#' @param X matrix of predictors
#' @return dot product of X and w with sigmoid function 
#'
#' @export
lgm_yhat <- function(w, X){ e1071::sigmoid(X %*% w) }

#' Logistic gradient computation
#'
#' @param w vector of weights
#' @param X matrix of predictors
#' @param t vector of target
#' @return w vector of weights
lgm_gradient <- function(w, X, t){ t(X) %*% (lgm_yhat(w, X) - t) }

#' Compute the logistic gradient with learning rate alpha
#'
#' @param w vector of weights
#' @param X matrix of predictors
#' @param t vector of target
#' @param alpha float, learning rate
#'
#' @return w vector of weights
#'
#' @examples
#'
#' @export
logistic_gradient <- function(X, t, w, alpha){
    alpha * (lgm_gradient(w, X, t) / nrow(X))
}




