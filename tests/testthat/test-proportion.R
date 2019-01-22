context("proportion() tests")

test_that("proportion() CI gets narrower with increasing sample size", {
  p5 <- proportion(5, 10)
  p50 <- proportion(50, 100)
  p500 <- proportion(500, 1000)
  # All are data frames ------
  expect_is(p5, "data.frame")
  expect_is(p50, "data.frame")
  expect_is(p500, "data.frame")
  # Proportions are equal ------
  expect_identical(p5$prop, p50$prop)
  expect_identical(p50$prop, p500$prop)
  # Lower CI increases ------
  expect_lt(p5$lower, p50$lower)
  expect_lt(p50$lower, p500$lower)
  # Upper CI decreases ------
  expect_gt(p5$upper, p50$upper)
  expect_gt(p50$upper, p500$upper)
})
