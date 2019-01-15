context("zcurve tests")
set.seed(2019-01-15)
dat <- rnorm(2974) - 0.4
zc  <- zcurve(dat)

test_that("zcurve returns a ggplot2 object", {
  expect_is(zc, "ggplot")
})

test_that("zcurve data is equal to the number of observations", {
  expect_equal(nrow(zc$data), 2974)
})

test_that("zcurve will throw an error if the input is not numeric", {
  expect_error(zcurve(letters))
})
