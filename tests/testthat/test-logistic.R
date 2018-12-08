context("logistic")

set.seed(0)

data <- bernoulli_sim()

test_that("lgm_yhat works", {

    X <- matrix(c(data$X1, data$X2), ncol=2)
    w <- matrix(data=0.0, nrow=ncol(X), ncol=1)

    ans <- lgm_yhat(w, X)
    expect_equal(nrow(ans), 50)
    expect_equal(ans[1], 0.5)
})

test_that("logistic gradient works", {

    X <- matrix(c(data$X1, data$X2), ncol=2)
    w <- matrix(data=0.0, nrow=ncol(X), ncol=1)
    t <- data$Y

    ans <- lgm_gradient(w, X, t)
    expect_equal(nrow(ans), 2)
})

test_that("batch gradient descent with logistic gradient works", {

    intercept <- 1:length(data$X1)
    intercept <- sapply(intercept, function(x) { x = 1 })
    X <- matrix(c(intercept, data$X1, data$X2), ncol=3)
    
    w <- matrix(data=0.0, nrow=ncol(X), ncol=1)
    t <- data$Y
    alpha <- 0.08 # hw3 question 5 specification

    h <- bgd(alpha, X, t, logistic_gradient, 10000)

    expect_true(h$converged)
    expect_equal(h$iterations, 5540)

    cls <- ifelse(lgm_yhat(h$theta, X) > 0.5, 1, 0)
    expect_equal(sum(cls), 6)

    # this is pretty different then another implementation
    # using a different approach but the math looks right
    #
    # m1 <- glm(venture_capital ~ lnnum_shares + lev_buyout,
    #           family= binomial, data = train)
})

test_that("Stochastic gradient descent with logistic gradient works", {

    intercept <- 1:length(data$X1)
    intercept <- sapply(intercept, function(x) { x = 1 })
    X <- matrix(c(intercept, data$X1, data$X2), ncol=3)
    
    w <- matrix(data=0.0, nrow=ncol(X), ncol=1)
    t <- data$Y
    alpha <- 0.08 # hw3 question 5 specification

    h <- sgd(alpha, X, t, logistic_gradient, 10000)

    expect_true(h$converged)
    expect_equal(h$iterations, 5540)

    cls <- ifelse(lgm_yhat(h$theta, X) > 0.5, 1, 0)
    expect_equal(sum(cls), 6)

})
