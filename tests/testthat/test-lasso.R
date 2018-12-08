context("test-lasso")

library(glmnet)

set.seed(0)

test_that("batch gradient descent with lasso regression works", {

    alpha <- 0.01
    lambda <- 2
    N <- 1000

    x <- runif(1000, -5, 5)
    y <- x + rnorm(1000) + 3
    X <- cbind(1, matrix(x))
    h <- bgd(alpha, X, y, lasso_gradient, N, lambda=lambda)

    model_lasso <- glmnet(X, y, alpha=1, lambda=lambda)
    cxf <- matrix(coef(model_lasso))


    expect_equal(round(cxf[1], 1), round(h$theta[1], 1))

    # seems like similar behavior to ridge regression
})
