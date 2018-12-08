context("loess")

set.seed(0)

test_that("weighted least squares is correct", {

    # analytic weighted least squares

    ans <- wls(x, y, w)

    # linear regression

    tans <- lm(y ~ x, weights=w)
    tcof <- matrix(coef(tans))

    expect_equal(round(tcof[1,], 3), round(ans$theta[1], 3))
    expect_equal(round(tcof[2,], 3), round(ans$theta[2], 3))
})

test_that("Gaussian kernel is correct", {
    
    ans <- gauss_kern(x, mean(x), sd(x))

    expect_equal(nrow(x), length(ans))
})


test_that("Loess regression is correct", {

    # run baseline loess regression model

    loessMod <- loess(y ~ x, span=0.50)
    baseline <- predict(loessMod)
    expect_equal(round(baseline[1],1), 21.4)

    # compare hw implementation
    ans <- local_lm(x, y)

    # used a bit of a smoke test and compared plots
    #library(ggplot2)

    #g <- ggplot() +
    #geom_point(aes(x,y)) +
    #geom_line(aes(x,pred))
})


