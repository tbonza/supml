context("test-least-squares")

set.seed(0)

test_that("analytic least squares implementation works", {
    
    x <- runif(1000, -5, 5)
    y <- x + rnorm(1000) + 3
    X <- cbind(1, matrix(x))

    h <- analyticlm(X, y)
    
    k <- lm(y ~ x)
    kc <- matrix(coef(k))

    expect_equal(round(kc[1,], 2), round(h$theta[1,], 2))
    expect_equal(round(kc[2,], 2), round(h$theta[2,], 2))

})

# this batch of tests looks at different loss functions invoving gradient descent

test_that("batch gradient descent with quadratic loss gradient works.", {

    x <- runif(1000, -5, 5)
    y <- x + rnorm(1000) + 3
    X <- cbind(1, matrix(x))
    h <- bgd(0.1, X, y, least_squares_ql_gradient, 1000)

    k <- lm(y ~ x)
    kc <- matrix(coef(k))

    # default linear model

    expect_equal(round(kc[1,], 2), 3.07)
    expect_equal(round(kc[2,], 2), 1)

    # loss function

    expect_true(round(h$theta[1,],1) <= 2.6)
    expect_true(round(h$theta[1,],1) > 2.5)

    expect_true(round(h$theta[2,],1) == 1)

    expect_true(h$converged)
    expect_equal(h$iterations, 63)
})

test_that("batch gradient descent with L1 norm works.", {

    x <- runif(1000, -5, 5)
    y <- x + rnorm(1000) + 3
    X <- cbind(1, matrix(x))
    h <- bgd(0.1, X, y, least_squares_l1_gradient, 1000)

    k <- lm(y ~ x)
    kc <- matrix(coef(k))

    # default linear model

    expect_equal(round(kc[1,], 2), 2.99)
    expect_equal(round(kc[2,], 2), 0.99)

    # loss function

    expect_true(round(h$theta[1,],1) <= 3.0)
    expect_true(round(h$theta[1,],1) > 2.8)

    expect_true(round(h$theta[2,],1) == 1)

    expect_true(h$converged)
    expect_equal(h$iterations, 76)
})

test_that("batch gradient descent with huber loss function works.", {

    delta <- 0.01
    
    x <- runif(1000, -5, 5)
    y <- x + rnorm(1000) + 3
    X <- cbind(1, matrix(x))
    h <- bgd(0.1, X, y, least_squares_huber_gradient, 1000, delta)

    k <- lm(y ~ x)
    kc <- matrix(coef(k))

    # default linear model

    expect_equal(round(kc[1,], 2), 3.02)
    expect_equal(round(kc[2,], 2), 1)

    # loss function

    expect_true(round(h$theta[1,],1) <= 3.0)
    expect_true(round(h$theta[1,],1) > 2.6)

    expect_true(round(h$theta[2,],1) == 1)

    expect_true(h$converged)
    expect_equal(h$iterations, 30)
})
