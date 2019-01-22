context("fmt_ci tests")

cfr <- case_fatality_rate(10, 50)
cfr_expected <- "20.00% (CI 11.24--33.04)"
pro <- proportion(5, 50)
pro_expected <- "10.00% (CI 4.35--21.36)"
test_that("fmt_ci.default only accepts numbers", {
  expect_error(fmt_pci(0, 0, 0, "A"))
  expect_error(fmt_ci(0, 0, "A", 0))
  expect_error(fmt_ci(0, "A", 0, 0))
  expect_error(fmt_ci("A", 0, 0, 0))
})

test_that("fmt_ci gives expected results", {
  expect_identical(fmt_ci(pi, pi, pi, 2), "3.14% (CI 3.14--3.14)")
  expect_identical(fmt_ci(pi, pi, pi, 3), "3.142% (CI 3.142--3.142)")
})

test_that("fmt_p*ci_df can take data frames", {
  cfr100       <- data.frame(lapply(cfr, `/`, 100))
  expect_identical(fmt_ci_df(cfr),        cfr_expected)
  expect_identical(fmt_ci_df(cfr[-1], 2), cfr_expected)
  expect_identical(fmt_pci_df(cfr100),    cfr_expected)
})

test_that("fmt_ci_df will produce multiple results when given multiple rows", {
  cfro <- rbind(cfr, setNames(pro, names(cfr)))
  expect_length(fmt_ci_df(cfro), 2)
  expect_identical(fmt_ci_df(cfro), c(cfr_expected, pro_expected))
})

