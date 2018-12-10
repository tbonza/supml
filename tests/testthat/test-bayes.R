context("bayes")


# Simulated test data

y <- factor(c("0","0","1","1","0"))
X <- data.frame(list(a = 1:5,
                     b = factor(c("1","0","1","1","0"))
                     ))

# Simulated spatial test data

ys <- factor(c("0","0","1"))

m1 <- matrix(FALSE, nrow=3, ncol=3)
m1[,2] <- TRUE

m2 <- matrix(FALSE, nrow=3, ncol=3)
m2[2,2] <-  TRUE; m2[2,3] <- TRUE

m3 <- matrix(FALSE, nrow=3, ncol=3)
m3[2,2] <- TRUE

m4 <- matrix(FALSE, nrow=3, ncol=3)
m4[c(1,3), 1] <- TRUE; m4[c(1,3), 3] <- TRUE


# Tests related to Gaussian kernel

test_that("bayes gaussian kernel density", {

    # bayes object
    lambda <- 0.5
    kernel <- "gaussian"
    b <- bayes(map=list())
    x0 <- 1
    point <- abs(X$a - x0)/lambda

    ans <- kernelDensity(b, point, kernel, lambda=lambda, mu=0)

    expect_equal(round(ans, 2), 0.16)
})

# Tests related to K-Block kernel

test_that("bayes kblock kernel density, scenario 1",{

    # bayes object
    kernel <- "kblock"
    b = bayes(map=list())
    i <- 1:nrow(m1)
    j <- 2
    k <- 1

    ans <- kernelDensity(b, x=i, kernel=kernel, j=j,m=m1,k=k)
    
    expect_equal(sum(ans), 3)
})

test_that("bayes kblock kernel density, scenario 2",{

    # bayes object
    kernel <- "kblock"
    b = bayes(map=list())
    i <- 1:nrow(m2)
    j <- 2
    k <- 1

    ans <- kernelDensity(b, x=i, kernel=kernel, j=j,m=m2,k=k)
    
    expect_equal(sum(ans), 1)
})

test_that("bayes kblock kernel density, scenario 3",{

    # bayes object
    kernel <- "kblock"
    b = bayes(map=list())
    i <- 1:nrow(m3)
    j <- 2
    k <- 1

    ans <- kernelDensity(b, x=i, kernel=kernel, j=j,m=m3,k=k)
    
    expect_equal(sum(ans), 0)
})

test_that("bayes kblock kernel density, scenario 4",{

    # bayes object
    kernel <- "kblock"
    b = bayes(map=list())
    i <- 1:nrow(m4)
    j <- 2
    k <- 1

    ans <- kernelDensity(b, x=i, kernel=kernel, j=j,m=m4,k=k)
    
    expect_equal(sum(ans), 0)
})

# Tests related to intermediate model fitting steps

test_that("bayes can fit spatial data", {

    # bayes object
    map = list(spatial=c(1:ncol(m1)), continuous=c(), categorical=c(),
               kernels=c(spatial="kblock", continuous="gaussian"),
               spatial_priors=list("1"=0.5,"0"=0.5), kblocks=1)
    b = bayes(map)
    b$classes <- levels(ys)

    ans <- spatialProbs(b, X=m1, y=ys)

    expect_equal(length(ans), 2)

    # check log probabilities (should be equal)

    expect_equal(ans[[b$classes[1]]], ans[[b$classes[1]]])
    expect_equal(ans[[b$classes[1]]], log(0.9999) + log(0.5))
})


# Tests related to fitting model

test_that("bayesian model can be fitted", {

    lambda <- 0.5
    b <- bayes(map=list())

    #ans <- fit(b, X, y)

    skip("Not yet ready")
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

