context("geom-test")

test_that("geometric probabiity density function base case", {
    expect_equal(geometpdf(1,1), 1)
})

test_that("geometric probability density function works", {
    expect_equal(round(geometpdf(1/13, 5), 3), 0.056)
})

test_that("geometric cumulative density function base case", {
    expect_equal(geometcdf(1,1), 1)
})

test_that("geometric cumulative density function less than case", {
    expect_equal(round(geometcdf(1/13, 9), 3), 0.513)
})

test_that("geometric cumulative density function greater than case", {
    expect_equal(round(geometcdf(1/13, 12, less=FALSE), 3), 0.383)
})
