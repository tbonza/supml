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
    b <- bayes()
    x0 <- 1
    point <- abs(X$a - x0)/lambda

    ans <- kernelDensity(b, point, kernel, lambda=lambda, mu=0)

    expect_equal(round(ans, 2), 0.16)
})

# Tests related to K-Block kernel

test_that("bayes kblock kernel density, scenario 1",{

    # bayes object
    kernel <- "kblock"
    b = bayes()
    i <- 1:nrow(m1)
    j <- 2
    k <- 1

    ans <- kernelDensity(b, x=i, kernel=kernel, j=j,m=m1,k=k)
    
    expect_equal(ans, 3)
})

test_that("bayes kblock kernel density, scenario 2",{

    # bayes object
    kernel <- "kblock"
    b = bayes()
    i <- 1:nrow(m2)
    j <- 2
    k <- 1

    ans <- kernelDensity(b, x=i, kernel=kernel, j=j,m=m2,k=k)
    
    expect_equal(ans, 1)
})

test_that("bayes kblock kernel density, scenario 3",{

    # bayes object
    kernel <- "kblock"
    b = bayes()
    i <- 1:nrow(m3)
    j <- 2
    k <- 1

    ans <- kernelDensity(b, x=i, kernel=kernel, j=j,m=m3,k=k)
    
    expect_equal(ans, 0)
})

test_that("bayes kblock kernel density, scenario 4",{

    # bayes object
    kernel <- "kblock"
    b = bayes()
    i <- 1:nrow(m4)
    j <- 2
    k <- 1

    ans <- kernelDensity(b, x=i, kernel=kernel, j=j,m=m4,k=k)
    
    expect_equal(ans, 0)
})

# Tests related to fitting model






# Tests related to fitting model

test_that("bayesian model can be fitted", {

    lambda <- 0.5
    b <- bayes()

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

