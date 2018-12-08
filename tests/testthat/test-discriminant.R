context("discriminant")

set.seed(0)

data <- bernoulli_sim()

test_that("group indices works", {

    Y <- factor(data$Y)

    ans <- grp_indices(levels(Y), Y)
    
    expect_equal(length(ans), 2)
    expect_equal(sum(ans[[1]]), sum(which(Y == "0")))
})

test_that("group covariance matrix works",{

    Y <- factor(data$Y)
    X <- matrix(c(data$X1, data$X2), ncol=2)
    grpidx <- grp_indices(levels(Y), Y)

    predictors <- X
    indices <- grpidx[[1]]

    ans <- grp_covar(indices, predictors)

    expect_equal(round(ans[1,1],1),24.2)
    expect_equal(round(ans[1,2],1),1.6)
    expect_equal(round(ans[2,1],1),1.6)
    expect_equal(round(ans[2,2],1),24.5)
})

test_that("pooled covariance works", {

    y <- data$Y
    X <- matrix(c(data$X1, data$X2), ncol=2)

    ans <- pooled_covar(X, y)

    expect_equal(nrow(ans), ncol(X))
    expect_equal(ncol(ans), ncol(X))

    expect_equal(round(ans[1,1],2), 0.6)
    expect_equal(round(ans[1,2],2), -0.01)
    expect_equal(round(ans[2,1],2), -0.01)
    expect_equal(round(ans[2,2],2), 0.62)
})

test_that("prior probabilities pi_k are computed correctly", {

    y <- data$Y
    ans <- pi_k(y)

    expect_equal(ans[[1]], sum(y == 0)/length(y))
})

test_that("Mean values mu_k for class K are correct", {

    y <- data$Y
    X <- matrix(c(data$X1, data$X2), ncol=2)

    Y <- factor(y)
    grpidx <- grp_indices(levels(Y), Y)
    ans <- lapply(grpidx,grp_mean,predictors=X)
    
    expect_equal(round(ans[[1]][1],2), 1.44)
    expect_equal(round(ans[[1]][2],2), 1.43)
})

test_that("Linear discriminant function is being sensible", {

    y <- data$Y
    X <- matrix(c(data$X1, data$X2), ncol=2)

    all_mu <- mu_k(X, y)
    all_pi <- pi_k(y)
    sig <- solve(pooled_covar(X, y))

    kmu <- all_mu[[1]]
    kpi <- all_pi[[1]]

    ans <- ldf(X, y, kpi, kmu, sig)

    expect_equal(nrow(ans), nrow(X))

})

test_that("Linear discriminant analysis is working", {

    y <- data$Y
    X <- matrix(c(data$X1, data$X2), ncol=2)

    ans <- linda(X, y)

    expect_equal(length(ans$yhat), nrow(X))
    expect_equal(sum(ans$yhat), 37)
    expect_equal(sum(y), 11)
})
