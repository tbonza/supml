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

#' K-Block Kernel
#'
#' Special case of a kernel used for estimating spatial
#' relationships to assess similarity of geographically
#' proximate neighbors. 
#'
#' Current implementation is used in Bayesian classification.
#' NOTE that the author is currently unaware of this 
#' kernel being available in the literature.
#'
#' For more details see the help vignette:
#' \code{vignette("homophily-kernel", package = "supml")}
#'
#' @param i row position in matrix
#' @param j column position in matrix
#' @param m boolean spatial matrix
#' @param k integer, size of window
#' @return TRUE if window exists, otherwise FALSE
#'
#' @export
kblock_kern <- function(i,j, m, k){
    nrow_m <- nrow(m)
    ncol_m <- ncol(m)

    # Check inputs 

    if (nrow_m != ncol_m) stop("m must be a square matrix")
    if (i > nrow_m | i < 1) stop("i must be a row index of m")
    if (j > ncol_m | j < 1) stop("j must be a column index of m")

    # Compute window boundaries

    rh <- ifelse(i + k <= nrow_m, i + k, i)
    rl <- ifelse(i - k >= 1, i - k, i)
    ch <- ifelse(j + k <= ncol_m, j + k, j)
    cl <- ifelse(j - k >= 1, j - k, j)

    # Compute window status

    windw <- m[rl:rh, cl:ch]
    total <- sum(windw)

    if (m[i,j] == TRUE & total > sum(m[i,j])){ return(TRUE) }
    else { return(FALSE) }
}
