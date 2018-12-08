context("test-loss")

set.seed(0)

test_that("quadratic loss base case works.", {

    e <- matrix(data=0,nrow=10, ncol=1)
    
    # base case is the minimum
    expect_equal(quadratic_loss(e), 0)
})

test_that("quadratic loss works.", {
    e <- rnorm(10, 0, 1)

    expect_equal(round(quadratic_loss(e), 1), 14.4)
})

test_that("mean absolute error base case works.", {

    e <- matrix(data=0,nrow=10, ncol=1)

    # base case is the minimum
    expect_equal(mae(e), 0)
})

test_that("mean absolute error works.", {
    e <- rnorm(10, 0, 1)

    expect_equal(round(mae(e), 1), 6.5)
})

test_that("huber loss conditional works.", {
    expect_equal(huber_cond(0,0.01), TRUE)
})

test_that("huber loss function base case works.", {
    e <- matrix(data=0, nrow=10,ncol=1)
    delta <- 1.0
    expect_equal(huber_loss(e, delta), 0)
})

test_that("huber loss function works.", {
    e <- rnorm(10, 0, 1)
    delta <- 1.0
    expect_equal(round(huber_loss(e, delta),1), 2.2)

    delta <- 0.1
    expect_equal(round(huber_loss(e, delta),1), 0.5)
})
