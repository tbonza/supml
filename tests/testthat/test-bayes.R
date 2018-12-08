context("bayes")


# Simulated test data

y <- factor(c("0","0","1","1","0"))
X <- data.frame(list(a = 1:5,
                     b = factor(c("1","0","1","1","0"))
                     ))

# Tests

test_that("bayes object constructor", {
    b <- bayes(0.5, "gaussian")

    expect_equal(b$lambda, 0.5)
    expect_equal(b$kernel, "gaussian")
})

test_that("bayes gaussian kernel density", {

    # bayes object
    lambda <- 0.5
    b <- bayes(lambda, "gaussian")
    x0 <- 1
    point <- abs(X$a - x0)/lambda

    ans <- kernelDensity(b, point)

    expect_equal(round(ans, 2), 0.16)
})

test_that("bayesian model can be fitted", {

    lambda <- 0.5
    b <- bayes(lambda, "gaussian")

    ans <- fit(b, X, y)

    expect_equal(class(ans), "bayes")
    expect_equal(ans$lambda, lambda)
    expect_equal(ans$kernel, "gaussian")
    expect_equal(ans$classes, levels(y))
    expect_equal(length(ans$models), 2)
    expect_equal(length(ans$logpriors), 2)

    # check specifics

    expect_equal(length(ans$models[['1']]), ncol(X) + 1)
    expect_equal(length(ans$logpriors[['1']]), ncol(X))

})
