#' Kernels used for localized estimation

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
