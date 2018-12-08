context("test-gradesc")

set.seed(0)

test_that("batch gradient descent works for least squares", {

    x <- runif(1000, -5, 5)
    y <- x + rnorm(1000) + 3
    X <- cbind(1, matrix(x))
    h <- bgd(0.1, X, y, least_squares_gradient, 1000)

    k <- lm(y ~ x)
    kc <- matrix(coef(k))

    # intercept
    expect_equal(round(kc[1,], 2), 2.96)
    expect_equal(round(h$theta[1,], 2), 2.96)

    # beta 1 coefficient
    expect_equal(round(kc[2,], 2), round(h$theta[2,], 2))
    
})

test_that("stochastic gradient descent works for least squares", {

    x <- runif(1000, -5, 5)
    y <- x + rnorm(1000) + 3
    X <- cbind(1, matrix(x))
    h <- sgd(0.1, X, y, least_squares_gradient, 1000)

    k <- lm(y ~ x)
    kc <- matrix(coef(k))

    # intercept
    expect_equal(round(kc[1,], 2), 3.07)
    expect_equal(round(h$theta[1,], 2), 3.07)

    # beta 1 coefficient
    expect_equal(round(kc[2,], 2), round(h$theta[2,], 2))
    
})

