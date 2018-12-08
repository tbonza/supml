#` Functions used for simulating various distributions

#' Simulate a random normal distribution on an interval
#'
#' @import stats
#' @export
nsim <- function(low, high, N){
    m <- ((high - low) / 2) + low
    s <- (high - m) / 3
    converged <- FALSE
    iterations <- 0

    while (converged == FALSE){
        d <- stats::rnorm(n=N, mean=m, sd=s)

        if (length(d[d > low & d < high]) == N) {
            converged <- TRUE
        }
    }
    return(d)
}

#' Create outliers for 1-d vector X from hw1 specs
#' @export
create_outliers <- function(x){
    xlen <- length(x)
    modify <- 0.1
    increase <- 0.5

    extractidx <- round(modify * xlen)
    
    # shuffle the data randomly
    x <- sample(x)

    # remove modify percent
    outs <- x[1:extractidx]
    keep <- x[(extractidx + 1):xlen]

    outs <- sample(outs)
    idx <- round(increase * length(outs))

    # increase half by 50%    
    # decrease half by 50%
    xmod <- c(outs[1:idx] * 1.50, outs[(idx+1):length(outs)] * 0.50, keep)

    # shuffle data randomly
    xmod <- sample(xmod)
    
    return(xmod)
}

#' Simulate data for hw
#'
#' Cleaner implementation from the TA's answer key
#'
#' @import stats
#' @return list, attribute matrix, target vector
#' @export
data_simulator <- function() {
    X <- stats::runif(50, -2, 2)
    Y <- 2 + 3*X + stats::rnorm(50, 0, 2)
    return(list(X=X, Y=Y))
}


#' Simulate Bernoulli distribution from HW3
#'
#' @import stats
#' @return list, attribute matrix, target vector
#' @export
bernoulli_sim <- function(){
    N <- 50
    X1 <- stats::runif(N, 0, 3)
    X2 <- stats::runif(N, 0, 3)

    p <- 1 / (1 + exp(1) - (-3 + X1 + X2))

    Y <- stats::rbinom(N, 1, p)

    return(list(X1=X1, X2=X2, Y=Y))    
}

