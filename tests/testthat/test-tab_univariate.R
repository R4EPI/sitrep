# Sample data ------------------------------------------------------------------
# generate a fake dataset
samp_tf <- function(n = 2000) sample(c(TRUE, FALSE), n, replace = TRUE)
a       <- tibble::tibble(case_def   = samp_tf(),
                          riskA      = samp_tf(),
                          riskB      = samp_tf(),
                          stratifier = samp_tf(),
                          perstime   = sample(150:250, 2000, replace = TRUE)
)
# generate a real data set from 
# http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704-ep713_confounding-em/BS704-EP713_Confounding-EM7.html
`%>%` <- dplyr::`%>%`
arr <- c(10, 35, 90, 465, 36, 25, 164, 175)
arr <- array(arr, dim = c(2, 2, 2),
             dimnames = list(risk = c(TRUE, FALSE),
                             outcome = c(TRUE, FALSE),
                             old = c(FALSE, TRUE))
       )

or_expect <- matrix(
  c(1.93175853018373, 1.47619047619048,  1.53658536585366,
    1.28113688091955, 0.705624518294847, 0.883931958442549,
    2.91279649701376, 3.08824065136142,  2.67112707488908),
  ncol = 3, nrow = 3, dimnames = list(
    c("crude", "FALSE", "TRUE"), 
    c("ratio", "lower", "upper")
  )
)

rr_expect <- matrix(
  c(1.78888888888889, 1.42857142857143,  1.44,
    1.24868722508264, 0.731610579575668, 0.898998784981615,
    2.56279025884831, 2.78948443817513,  2.30656596498337),
  ncol = 3, nrow = 3, dimnames = list(
    c("crude", "FALSE", "TRUE"), 
    c("ratio", "lower", "upper")
  )
)

irr_expect <- matrix(
  c(1.93175853018373, 1.47619047619048,  1.53658536585366,
    1.2863352909086,  0.651952666109562, 0.89726180699843,
    2.88430390374334, 3.04596159719188,  2.67042872585681),
  ncol = 3, nrow = 3, dimnames = list(
    c("crude", "FALSE", "TRUE"), 
    c("ratio", "lower", "upper")
  )
)

arrt <- as.data.frame.table(arr) %>%
  dplyr::summarise(res = list(data.frame(
    risk    = rep(risk, Freq),
    outcome = rep(outcome, Freq),
    old = rep(old, Freq)
  ))) %>%
  tidyr::unnest() %>%
  lapply(as.logical) %>%
  tibble::as_tibble()

test_that("internal estimate functions works", {

  expect_equivalent(get_ratio_est(arr, "OR")[1:3, 1:3],  (or_expect))
  expect_equivalent(get_ratio_est(arr, "RR")[1:3, 1:3],  (rr_expect))
  expect_equivalent(get_ratio_est(arr, "IRR")[1:3, 1:3], (irr_expect))

})

test_that("MH estimate works" , {

  MH_OR <- c(est = 1.51612903225806, lower = 0.973921554763186, upper = 2.3601975243424)
  MH_IRR <- c(est = 1.51729552694154, lower = 1.00321956498486, upper = 2.2947974665063)

  expect_equivalent(mh_rr(arr), c(1.43636363636364, 0.97698703277564, 2.11173784979146))
  expect_equivalent(mh_or(arr), MH_OR)
  expect_equivalent(mh_irr(arr), MH_IRR)

})

test_that("woolf p-values work", {

  RR_woolf <- data.frame(test.statistic = 0.000364164132934994, 
                         df = 1, 
                         p.value = 0.984774825192526)
  OR_woolf <- data.frame(
                         test.statistic = c(6.56694874232559e-05),
                         df = c(1),
                         p.value = c(0.993534276605677))
  expect_equivalent(get_woolf_pval(arr, "RR"), RR_woolf)
  expect_equivalent(get_woolf_pval(arr, "OR"), OR_woolf)

})



# get the results from tab_univariate function
func_res <- tab_univariate(a, case_def, riskA, strata = stratifier, digits = 6)
## read this article for details of calculations
# http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704-ep713_confounding-em/BS704-EP713_Confounding-EM7.html

# a <- dplyr::mutate_if(a, is.factor, factor, levels = c("TRUE", "FALSE"))

# Odds Ratios ------------------------------------------------------------------
# get counts table crude
counts <- table(a[c("case_def", "riskA")])

# get counts table stratified
counts_strat <- table(a[c("case_def", "riskA", "stratifier")])

## For odds ratios

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
sum_strat_true  <- sum(counts_strat[ , , 2])
sum_strat_false <- sum(counts_strat[ , , 1])

# sigma(a_i * d_i / n_i) / sigma(b_i * c_i / n_i) where n_i is the sum of respective strata
or_mh <- (((counts_strat[8] * counts_strat[5]) / sum_strat_true) +
              ((counts_strat[4] * counts_strat[1]) / sum_strat_false) ) /
  (((counts_strat[6] * counts_strat[7]) / sum_strat_true) +
       ((counts_strat[2] * counts_strat[3]) / sum_strat_false) )




# Tests ------------------------------------------------------------------------


test_that("function OR is equal to OR from odds", {

  expect_equal(func_res$est[1], or_from_odds, tol = 1e-6)

})

test_that("function OR is equal to OR from cross multiplying", {

  expect_equal(func_res$est[1], or_from_cross, tol = 1e-6)

})

test_that("function case_odds is equal to count odds", {

  expect_equal(func_res$cases_odds[1],    expo_cases_odds,    tol = 1e-2)
  expect_equal(func_res$controls_odds[1], expo_controls_odds, tol = 1e-2)

})

## strata
test_that("function OR is equal to OR from odds for strata", {

  skip("in progress")
  expect_equal(func_res$est[2], or_from_odds_strat_true,  tol = 1e-6)
  expect_equal(func_res$est[3], or_from_odds_strat_false, tol = 1e-6)

})

test_that("function OR is equal to OR from cross multiplying for strata", {
  skip("in progress")
  expect_equal(func_res$est[2], or_from_cross_strat_true,  tol = 1e-6)
  expect_equal(func_res$est[3], or_from_cross_strat_false, tol = 1e-6)

})

test_that("function case odds are equal to count odds for strata", {

  skip("refactoring at the momen")
  expect_equal(func_res$cases_odds[2],    expo_cases_odds_strat_true,    tol = 1e-6)
  expect_equal(func_res$cases_odds[3],    expo_cases_odds_strat_false,   tol = 1e-6)
  expect_equal(func_res$controls_odds[2], expo_controls_odds_strat_true, tol = 1e-6)
  expect_equal(func_res$controls_odds[3], expo_control_odds_strat_false, tol = 1e-6)

})


## Mantel-haeszel
test_that("function mh odds are equal to calculated mh", {
  expect_equal(func_res$est[4], or_mh, tol = 1e-6)
})


# Risk Ratios ------------------------------------------------------------------
## Tests for RRs
# get counts table crude
counts       <- table(a[c("riskA", "case_def")])
# get counts table stratified
counts_strat <- table(a[c("riskA", "case_def", "stratifier")])

## For risk ratios
# get risk of being a case among exposed
expo_cases_risk             <- counts[4]/sum(counts[2,]) * 100
expo_cases_risk_strat_true  <- counts_strat[8] / sum(counts_strat[2, , 2]) * 100
expo_cases_risk_strat_false <- counts_strat[4] / sum(counts_strat[2, , 1]) * 100

# get risk of being a case among unexposed
nonexpo_cases_risk             <- counts[3]/sum(counts[1,]) * 100
nonexpo_cases_risk_strat_true  <- counts_strat[7] / sum(counts_strat[1, , 2]) * 100
nonexpo_cases_risk_strat_false <- counts_strat[3] / sum(counts_strat[1, , 1]) * 100

# calculate risk ratio by using risks above
rr_from_risk             <- expo_cases_risk / nonexpo_cases_risk
rr_from_risk_strat_true  <- expo_cases_risk_strat_true / nonexpo_cases_risk_strat_true
rr_from_risk_strat_false <- expo_cases_risk_strat_false / nonexpo_cases_risk_strat_false

# calculate mantel-haeszel RRs
# get sums of strata
sum_strat_true  <- sum(counts_strat[ , ,2])
sum_strat_false <- sum(counts_strat[ , , 1])

# sigma(a_i * c_i + d_i / n_i) / sigma(c_i * a_i + b_i / n_i) where n_i is the sum of respective strata
rr_mh <- (
  ((counts_strat[8] * (counts_strat[5] + counts_strat[7])) / sum_strat_true) +
    ((counts_strat[4] * (counts_strat[1] + counts_strat[3])) / sum_strat_false)) /
  (
    ((counts_strat[7] * (counts_strat[6] + counts_strat[8])) / sum_strat_true) +
      ((counts_strat[3] * (counts_strat[2] + counts_strat[4])) / sum_strat_false))

# get the results from tab_univariate function
func_res <- tab_univariate(a, case_def, riskA, strata = stratifier, digits = 6, measure = "RR")

# Tests ------------------------------------------------------------------------

## crude
test_that("function RR is equal to RR from risks", {
  expect_equal(func_res$est[1], rr_from_risk, tol = 1e-6)
})


test_that("function risk among exposed/unexposed is equal to count risks", {
  expect_equal(func_res$exp_risk[1], expo_cases_risk, tol = 1e-3)
  expect_equal(func_res$unexp_risk[1], nonexpo_cases_risk, tol = 1e-3)
})


## strata
test_that("function RR is equal to RR from risks for strata", {

  skip("refactoring at the momen")
  expect_equal(func_res$est[2], rr_from_risk_strat_true, tol = 1e-6)
  expect_equal(func_res$est[3], rr_from_risk_strat_false, tol = 1e-6)
})


test_that("function risks are equal to count risks for strata", {

  skip("refactoring at the momen")
  expect_equal(func_res$exp_risk[2], expo_cases_risk_strat_true, tol = 1e-6)
  expect_equal(func_res$exp_risk[3], expo_cases_risk_strat_false, tol = 1e-6)
  expect_equal(func_res$unexp_risk[2], nonexpo_cases_risk_strat_true, tol = 1e-6)
  expect_equal(func_res$unexp_risk[3], nonexpo_cases_risk_strat_false, tol = 1e-6)
})


## Mantel-haeszel
test_that("function mh risks are equal to calculated mh", {
  expect_equal(func_res$est[4], rr_mh, tol = 1e-6)
})



# Incidence Rate Ratios --------------------------------------------------------
## Tests for IRRs

# get counts and person time crude
counts <- aggregate(
  formula(cbind(case_def, perstime) ~ riskA),
  FUN = sum,
  data = a)


# get counts and person time stratified
counts_strat <- aggregate(
  formula(cbind(case_def, perstime) ~ riskA + stratifier),
  FUN = sum,
  data = a)



## For incidence rate ratios
# get incidence of being a case among exposed

expo_cases_inc             <- counts$case_def[2]/counts$perstime[2] * 100
expo_cases_inc_strat_true  <- counts_strat$case_def[counts_strat$riskA == TRUE &
                                                      counts_strat$stratifier == TRUE] /
                              counts_strat$perstime[counts_strat$riskA == TRUE &
                                                      counts_strat$stratifier == TRUE] * 100
expo_cases_inc_strat_false <- counts_strat$case_def[counts_strat$riskA == TRUE &
                                                      counts_strat$stratifier == FALSE] /
                              counts_strat$perstime[counts_strat$riskA == TRUE &
                                                      counts_strat$stratifier == FALSE] * 100


# get incidence of being a case among unexposed
nonexpo_cases_inc             <- counts$case_def[1]/counts$perstime[1] * 100
nonexpo_cases_inc_strat_true  <- counts_strat$case_def[counts_strat$riskA == FALSE &
                                                      counts_strat$stratifier == TRUE] /
                                  counts_strat$perstime[counts_strat$riskA == FALSE &
                                                          counts_strat$stratifier == TRUE] * 100
nonexpo_cases_inc_strat_false <- counts_strat$case_def[counts_strat$riskA == FALSE &
                                                      counts_strat$stratifier == FALSE] /
                                  counts_strat$perstime[counts_strat$riskA == FALSE &
                                                          counts_strat$stratifier == FALSE] * 100


# calculate risk ratio by using risks above
irr_from_inc             <- expo_cases_inc / nonexpo_cases_inc
irr_from_inc_strat_true  <- expo_cases_inc_strat_true / nonexpo_cases_inc_strat_true
irr_from_inc_strat_false <- expo_cases_inc_strat_false / nonexpo_cases_inc_strat_false


# calculate mantel-haeszel IRRs
# get sums of strata
sum_strat_true  <- sum(counts_strat$perstime[counts_strat$stratifier == TRUE])
sum_strat_false <- sum(counts_strat$perstime[counts_strat$stratifier == FALSE])

# sigma(a_i * PTo / PTt) / sigma(c_i * PTe / PTt)
# where PTt is the sum of respective strata
# and the table is set out as follows:
#           outcome+   | person time
# exposure+     a      |      PTe
# exposure-     c      |      PTo
# Total                |      PTt

irr_mh <- (((counts_strat$case_def[counts_strat$riskA == TRUE &
                                  counts_strat$stratifier == TRUE] *
            counts_strat$perstime[counts_strat$riskA == FALSE &
                                    counts_strat$stratifier == TRUE]) / sum_strat_true) +
          ((counts_strat$case_def[counts_strat$riskA == TRUE &
                                    counts_strat$stratifier == FALSE] *
              counts_strat$perstime[counts_strat$riskA == FALSE &
                                      counts_strat$stratifier == FALSE]) / sum_strat_false)) /

          (((counts_strat$case_def[counts_strat$riskA == FALSE &
                                     counts_strat$stratifier == TRUE] *
               counts_strat$perstime[counts_strat$riskA == TRUE &
                                       counts_strat$stratifier == TRUE]) / sum_strat_true) +
             ((counts_strat$case_def[counts_strat$riskA == FALSE &
                                       counts_strat$stratifier == FALSE] *
                 counts_strat$perstime[counts_strat$riskA == TRUE &
                                         counts_strat$stratifier == FALSE]) / sum_strat_false))


# get the results from tab_univariate function
func_res <- tab_univariate(a, case_def, riskA, strata = stratifier, perstime = perstime, digits = 6, measure = "IRR")



# Tests ------------------------------------------------------------------------

## crude
test_that("function IRR is equal to IRR from incidence", {
  expect_equal(func_res$est[1], irr_from_inc, tol = 1e-6)
})


test_that("function incidence among exposed/unexposed is equal to count incidence", {

  expect_equal(func_res$exp_incidence[1], expo_cases_inc, tol = 1e-3)
  expect_equal(func_res$unexp_incidence[1], nonexpo_cases_inc, tol = 1e-3)
})


## strata
test_that("function IRR is equal to IRR from incidence for strata", {

  skip("refactoring at the momen")
  expect_equal(func_res$est[2], irr_from_inc_strat_true, tol = 1e-6)
  expect_equal(func_res$est[3], irr_from_inc_strat_false, tol = 1e-6)
})


test_that("function incidence are equal to count incidence for strata", {
  skip("refactoring at the momen")
  expect_equal(func_res$exp_incidence[2], expo_cases_inc_strat_true, tol = 1e-6)
  expect_equal(func_res$exp_incidence[3], expo_cases_inc_strat_false, tol = 1e-6)
  expect_equal(func_res$unexp_incidence[2], nonexpo_cases_inc_strat_true, tol = 1e-6)
  expect_equal(func_res$unexp_incidence[3], nonexpo_cases_inc_strat_false, tol = 1e-6)
})


## Mantel-haeszel
test_that("function mh risks are equal to calculated mh", {
  expect_equal(func_res$est[4], irr_mh, tol = 1e-6)
})

