context("proportion() tests")

p5   <- attack_rate(5, 10)
p50  <- case_fatality_rate(50, 100)
p500 <- attack_rate(500, 1000)

test_that("Rates work with missing data", {

  pna5 <- attack_rate(c(5, NA, 5), c(NA, 10, 10))
  expect_is(pna5, "data.frame")

  expect_identical(pna5$ar   , c(NA, NA, p5$ar))
  expect_identical(pna5$lower, c(NA, NA, p5$lower))
  expect_identical(pna5$upper, c(NA, NA, p5$upper))
  expect_identical(pna5$ar   , c(NA, NA, p5$ar))

})

test_that("mismatched data are rejected", {

  err <- "the length of the population vector (2) does not match the length of the cases/deaths vector (1)"
  expect_error(attack_rate(5, c(10, 11)), err, fixed = TRUE) 

})

test_that("CI gets narrower with increasing sample size", {
  # All are data frames ------
  expect_is(p5, "data.frame")
  expect_is(p50, "data.frame")
  expect_is(p500, "data.frame")

  # Proportions are equal ------
  expect_identical(p5$ar, p50$cfr)
  expect_identical(p50$cfr, p500$ar)

  # Lower CI increases ------
  expect_lt(p5$lower, p50$lower)
  expect_lt(p50$lower, p500$lower)

  # Upper CI decreases ------
  expect_gt(p5$upper, p50$upper)
  expect_gt(p50$upper, p500$upper)
})

test_that("numbers are not rounded", {
  expect_equal(p5$lower, 23.659309, tol = 1e-6)
  expect_equal(p50$lower, 40.383153, tol = 1e-6)
})

test_that("mortality rates work", {
  # see https://www.cdc.gov/ophss/csels/dsepd/ss1978/lesson3/section3.html
  accidentals   <- 106742
  US_population <- 288357000
  mr <- mortality_rate(accidentals, US_population, multiplier = 10^5)
  expect_named(mr, c("deaths", "population", "mortality per 100 000", "lower", "upper"))
  expect_equal(mr$"mortality per 100 000", 37.02, tol = 0.01)
})

