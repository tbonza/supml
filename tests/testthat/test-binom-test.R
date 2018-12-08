context("binom-test")

test_that("combinations base case work", {
    expect_equal(combs(1,1), 1)
})

test_that("combinations work", {
  expect_equal(combs(6,2), 15)
})

test_that("binomial probability density function base case", {
    expect_equal(binompdf(1,1,1), 1)
})

test_that("binomial probability density function works", {
    expect_equal(binompdf(7,4,0.35), 0.1442382)
})

test_that("binomial cumulative density function base case", {
    expect_equal(binomcdf(1,1,1), 1)
})

test_that("binomial cumulative density function works", {
    expect_equal(round(binomcdf(7,4,0.35), 3), 0.944)
})

test_that("binomial mean base case", {
    expect_equal(binom_mean(1,1), 1)
})

test_that("binomial mean function works", {
    expect_equal(binom_mean(500, 0.02), 10)
})

test_that("binomial standard deviation base case", {
    expect_equal(binom_sd(1,1), 0)
})

test_that("binomial standard deviation works", {
    expect_equal(round(binom_sd(500, 0.02), 2), 3.13)
})
