# Discrete random variables -- geometric random variables utilites

#' Geometric probability density function for first success in x trials
#'
#' @param p probability of success for independent trial
#' @param x number of trials, x >= 1
#'
#' @return probability of success after x number of trials
#'
#' @examples
#' geometpdf(1,1)
#' geometpdf(1/13, 5)
#'
#' @export
geometpdf <- function(p, x){
    if (x == 0){ stop("x must be greater than or equal to one") }
    
    (1-p)^(x-1)*p^(1)
}

#' Geometric cumulative density function
#'
#' @param p probability of success for an independent trial
#' @param x number of trials
#' @param less boolean TRUE if probability of choosing less than x
#'
#' @return cumulative probability of geometric series
#'
#' @examples
#' geometcdf(1,1)
#' geometcdf(1/13, 9)
#' geometcdf(1/13, 12)
#'
#' @export
geometcdf <- function(p, x, less=TRUE){
    probs <- c()
    for (i in 1:x){
        probs <- c(probs, geometpdf(p, i))
    }

    if (less) { return(sum(probs)) }
    else { return( 1 - sum(probs)) }
}
