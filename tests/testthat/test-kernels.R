context("kernels")

# 
# Gaussian Kernel
#

test_that("Gaussian kernel is correct", {
    
    ans <- gauss_kern(x, mean(x), sd(x))

    expect_equal(nrow(x), length(ans))
})

#
# Homophily kernel
#

test_that("valid homophily kernel scenario 1 with k=1", {
    # Scenario 1, center column is all true
    m1 <- matrix(FALSE, nrow=3, ncol=3)
    m1[,2] <- TRUE

    expect_true(homophily_kern(2,2,m1,1))
})

test_that("valid homophily kernel scenario 2 with k=1", {
    # Scenario 2, center and right middle component is true
    m2 <- matrix(FALSE, nrow=3, ncol=3)
    m2[2,2] <-  TRUE; m2[2,3] <- TRUE

    expect_true(homophily_kern(2,2,m2,1))
})

test_that("valid homophily kernel scenario 3 with k=1", {
    # Scenario 3, only center is true
    m3 <- matrix(FALSE, nrow=3, ncol=3)
    m3[2,2] <- TRUE

    expect_false(homophily_kern(2,2,m3,1))
})

test_that("valid homophily kernel scenario 4 with k=1", {
    # Scenario 4, only corners are true
    m4 <- matrix(FALSE, nrow=3, ncol=3)
    m4[c(1,3), 1] <- TRUE; m4[c(1,3), 3] <- TRUE

    expect_false(homophily_kern(2,2,m4,1))
})






