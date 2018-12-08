#' Discrete random variables -- binomial probability

#' Compute the number of combinations of for n, k
#'
#' @param n number of trials
#' @param k number of successes
#'
#' @return The number of combinations for n choose k
#'
#' @export
combs <- function(n, k){
    factorial(n) / (factorial(k) * factorial(n-k))
}

#' Compute the binomial probability density function
#' 
#' @param n number of trials
#' @param k number of successes
#' @param p probability of success for an independent trial
#' 
#' @return The probability of having exactly k successes
#'
#' @examples
#' binompdf(1,1,1)
#' binompdf(7, 4, 0.35) # probability of making 7 out of 4 free throws in bball
#'
#' @export
binompdf <- function(n, k, p) {
    combs(n,k) * p^(k) * (1-p)^(n-k)
}

#' Compute the binomial cumulative density function
#'
#' @param n number of trials
#' @param k number of successes
#' @param p probability of success for an independent trial
#'
#' @return binomial cumulative probability of k successes
#'
#' @examples
#' binomcdf(1,1,1)
#' binomcdf(7,4,0.35)
#'
#' @export
binomcdf <- function(n, k, p) {
    probs <- c()

    for (i in 0:k){
        probs <- c(probs, binompdf(n, i, p))
    }

    return(sum(probs))
}

#' Mean of binomial variable
#'
#' @param n number of trials
#' @param p probability of success
#'
#' @return mean value of success for n trials
#'
#' @examples
#' binom_mean(1,1)
#' binom_mean(10, 0.35)
#'
#' @export
binom_mean <- function(n, p) { n * p }

#' Standard deviation of binomial variable
#'
#' @param n number of trials
#' @param p probability of success
#'
#' @return standard deviation of success after n trials
#'
#' @examples
#' binom_sd(1, 1)
#' binom_sd(10, 0.35)
#'
#' @export
binom_sd <- function(n, p) { sqrt(n * p * (1 - p)) }

