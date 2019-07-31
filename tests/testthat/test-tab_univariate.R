
# generate a fake dataset
samp_tf <- function(n = 2000) sample(c(TRUE, FALSE), n, replace = TRUE)
a       <- tibble::tibble(case_def   = samp_tf(),
                          riskA      = samp_tf(),
                          riskB      = samp_tf(),
                          stratifier = samp_tf(),
                          perstime   = sample(150:250, 2000, replace = TRUE)
)

## read this article for details of calculations
# http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704-ep713_confounding-em/BS704-EP713_Confounding-EM7.html

# get counts table crude
counts <- table(a[c("case_def", "riskA")])

# get counts table stratified
counts_strat <- table(a[c("case_def", "riskA", "stratifier")])

## For odds ratios

# get odds of exposure among cases
expo_cases_odds             <- counts[4]/counts[2]
expo_cases_odds_strat_true  <- counts_strat[8]/counts_strat[6]
expo_cases_odds_strat_false <- counts_strat[4]/counts_strat[2]

# get odds of exposure among controls
expo_controls_odds            <- counts[3]/counts[1]
expo_controls_odds_strat_true <- counts_strat[7]/counts_strat[5]
expo_control_odds_strat_false <- counts_strat[3] / counts_strat[1]

# calculate odds ratio by using odds above
or_from_odds             <- expo_cases_odds / expo_controls_odds
or_from_odds_strat_true  <- expo_cases_odds_strat_true / expo_controls_odds_strat_true
or_from_odds_strat_false <- expo_cases_odds_strat_false / expo_control_odds_strat_false

# calculate odds ratio by cross multiplying counts
or_from_cross             <- (counts[4] * counts[1]) / (counts[2] * counts[3])
or_from_cross_strat_true  <- (counts_strat[8] * counts_strat[5]) / (counts_strat[6] * counts_strat[7])
or_from_cross_strat_false <- (counts_strat[4] * counts_strat[1]) / (counts_strat[2] * counts_strat[3])


# calculate mantel-haeszel ORs

# get sums of strata
sum_strat_true  <- sum(counts_strat[ , ,2])
sum_strat_false <- sum(counts_strat[ , , 1])

# sigma(a_i * d_i / n_i) / sigma(b_i * c_i / n_i) where n_i is the sum of respective strata
or_mh <- (((counts_strat[8] * counts_strat[5]) / sum_strat_true) +
              ((counts_strat[4] * counts_strat[1]) / sum_strat_false) ) /
  (((counts_strat[6] * counts_strat[7]) / sum_strat_true) +
       ((counts_strat[2] * counts_strat[3]) / sum_strat_false) )


# get the results from tab_univariate function
func_res <- tab_univariate(a, case_def, riskA, strata = stratifier, digits = 6)


# Tests ------------------------------------------------------------------------


test_that("function OR is equal to OR from odds", {

  expect_equal(func_res$est[1], or_from_odds, tol = 1e-3)

})

test_that("function OR is equal to OR from cross multiplying", {

  expect_equal(func_res$est[1], or_from_cross, tol = 1e-3)

})

test_that("function case_odds is equal to count odds", {

  expect_equal(func_res$cases_odds[1],    expo_cases_odds,    tol = 1e-3)
  expect_equal(func_res$controls_odds[1], expo_controls_odds, tol = 1e-3)

})

## strata
test_that("function OR is equal to OR from odds for strata", {

  expect_equal(func_res$est[2], or_from_odds_strat_true,  tol = 1e-3)
  expect_equal(func_res$est[3], or_from_odds_strat_false, tol = 1e-3)

})

test_that("function OR is equal to OR from cross multiplying for strata", {

  expect_equal(func_res$est[2], or_from_cross_strat_true,  tol = 1e-3)
  expect_equal(func_res$est[3], or_from_cross_strat_false, tol = 1e-3)

})

test_that("function case odds are equal to count odds for strata", {

  expect_equal(func_res$cases_odds[2],    expo_cases_odds_strat_true,    tol = 1e-3)
  expect_equal(func_res$cases_odds[3],    expo_cases_odds_strat_false,   tol = 1e-3)
  expect_equal(func_res$controls_odds[2], expo_controls_odds_strat_true, tol = 1e-3)
  expect_equal(func_res$controls_odds[3], expo_control_odds_strat_false, tol = 1e-3)

})


## Mantel-haeszel
test_that("function mh odds are equal to calculated mh", {
  expect_equal(func_res$est[4], or_mh, tol = 1e-3)
})

