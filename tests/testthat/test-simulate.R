context("test-simulate")

test_that("outliers created correctly", {
    N <- 50
    alpha <- 0.01
    b0 <- 2
    b1 <- 3
    x <- nsim(-2, 2, N)
    e <- nsim(0, 4, N)

    xmod <- create_outliers(x)

    expect_equal(length(xmod), N)
})
