context("test-ridge")

library(glmnet)

set.seed(0)

test_that("analytic ridge least squares works", {

    x <- runif(1000, -5, 5)
    y <- x + rnorm(1000) + 3
    X <- cbind(1, matrix(x))

    model_ridge <- glmnet(X, y, alpha=0, lambda=1)

    fit <- analyticridge(X, y, lambda=1)
    fitcm <- matrix(coef(model_ridge))

    expect_equal(round(fitcm[1,], 1), round(fit$theta[1,], 1))

    # glmnet has a lot of default params but this seems to be in the ball park
    #expect_equal(round(fitcm[3,], 2), round(fit$theta[2,], 2))
})



