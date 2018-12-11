context("bayes")


# Constants used for bayesian classifier

LOGMIN <- 0.0001
LOGMAX <- 0.9999


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

# Tests related to conditional probability intermediate model fitting steps

test_that("bayes can fit spatial data", {

    # bayes object
    map = list(spatial=c(1:ncol(m1)), continuous=c(), categorical=c(),
               kernels=c(spatial="kblock", continuous="gaussian"),
               spatial_priors=list("1"=0.5,"0"=0.5),
               hyperparameters=c(lambda=0.5, kblocks=1))
    b = bayes(map)
    b$classes <- levels(ys)

    ans <- spatialProbs(b, X=m1, y=ys)

    expect_equal(length(ans), 4)

    # check log probabilities (should be equal)

    expect_equal(ans[[b$classes[1]]], ans[[b$classes[1]]])
    expect_equal(ans[[paste0(b$classes[1], "|k")]], log(0.9999))
})

test_that("bayes can fit continuous data", {

    # bayes object
    map = list(spatial=c(), continuous=c(1), categorical=c(2),
               kernels=c(spatial="kblock", continuous="gaussian"),
               spatial_priors=list("1"=0.5,"0"=0.5),
               hyperparameters=c(lambda=0.5, kblocks=1))
    b <- bayes(map)
    b$classes <- levels(y)

    ans <- continuousProbs(b, X, y)

    expect_equal(ans[['mu|1|0']], mean(X[y==b$classes[1], 1]))
    expect_equal(ans[['mu|1|1']], mean(X[y==b$classes[2], 1]))
})

test_that("bayes can fit categorical data", {

    # bayes object
    map = list(spatial=c(), continuous=c(1), categorical=c(2),
               kernels=c(spatial="kblock", continuous="gaussian"),
               hyperparameters=c(lambda=0.5, mu=0, kblocks=1))
    b = bayes(map)
    b$classes <- levels(y)

    ans <- categoricalProbs(b,X,y)

    expect_equal(length(ans), 4)
    expect_equal(ans[['0|2|0']], log(2) - log(3))
    expect_equal(ans[['1|2|0']], log(LOGMIN))
    expect_equal(ans[['0|2|1']], log(LOGMIN))
    expect_equal(ans[['1|2|1']], log(LOGMAX))
})

# Tests related to prior probability intermediate model fitting steps

test_that("find prior probabilities for continuous/categorical data", {

    # Check LOGMAX

    b <- bayes(map=list())
    y1 <- factor(c("1","1"))
    ans1 <- priorProbs(b,y1)
    expect_equal(ans1[1,2], log(LOGMAX))

    # Check LOGMIN when only one class is present

    b <- bayes(map=list())
    y2 <- factor(c("0"))
    ans2 <- priorProbs(b,y2)
    expect_equal(ans2[1,2], log(LOGMIN))

    # Check general case

    b <- bayes(map=list())
    y3 <- factor(c("0","1","1"))
    ans3 <- priorProbs(b,y3)
    expect_equal(ans3[1,2], log(LOGMIN))
    expect_equal(ans3[2,2], log(2) - log(3))
})


# Tests related to fitting model

test_that("bayesian model can be fitted", {

    # Combine dataframe to include all three
    # types of variables. 

    df <- as.data.frame(m1)
    df <- cbind(df, c(1:nrow(m1)), factor(c("1","0","0")))
    colnames(df) <-  paste0("V", as.character(1:ncol(df)))


    # construct bayes object

    map = list(spatial=c(1:ncol(m1)), continuous=c(ncol(m1) + 1),
               categorical=c(ncol(m1) + 2),
               kernels=c(spatial="kblock", continuous="gaussian"),
               spatial_priors=list("1"=0.5,"0"=0.5),
               hyperparameters=c(lambda=0.5, kblocks=1))

    b <- bayes(map)

    ans <- fit(b, X=df, y=ys)

    expect_equal(length(ans), 4)
    expect_equal(length(ans$logpriors$spatial), 4)
    expect_equal(length(ans$logpriors$priors), 2)
    expect_equal(length(ans$logpriors$categorical), 4)
    expect_equal(length(ans$logpriors$continuous), 2)
})

# Tests related to spatial probability predictions

test_that("spatial probability predictions are correctly computed", {

    # bayes object
    
    map = list(spatial=c(1:ncol(m1)), continuous=c(), categorical=c(),
               kernels=c(spatial="kblock", continuous="gaussian"),
               spatial_priors=list("1"=0.5,"0"=0.5),
               hyperparameters=c(lambda=0.5, kblocks=1))
    b = bayes(map)

    b <- fit(b, m1, ys)

    bans <- postSpatialProbs(b, m1)

    expect_equal(sum(bans$models$spatial$resolve), 6)

    # 0 is the dominant class

    expect_true(bans$models$spatial$kblocks[1,1] > 
                bans$models$spatial$wblocks[1,1])
})

