# generate a real data set from 
# http://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704-ep713_confounding-em/BS704-EP713_Confounding-EM7.html
`%>%` <- dplyr::`%>%`
arr <- c(10, 35, 90, 465, 36, 25, 164, 175)
arr <- array(arr, dim = c(2, 2, 2),
             dimnames = list(risk    = c(TRUE, FALSE),
                             outcome = c(TRUE, FALSE),
                             old     = c(FALSE, TRUE))
       )

# testing incidence rate
iarr[, 2, ] <- c(4, 4, 2, 10) * 100


or_expect <- data.frame(
       ratio = c(1.93175853018373, 1.47619047619048, 1.53658536585366),
       lower = c(1.28113688091955, 0.705624518294847, 0.883931958442549),
       upper = c(2.91279649701376, 3.08824065136142, 2.67112707488908)
)

rr_expect <- data.frame(
       ratio = c(1.78888888888889, 1.42857142857143, 1.44),
       lower = c(1.24868722508264, 0.731610579575668, 0.898998784981615),
       upper = c(2.56279025884831, 2.78948443817513, 2.30656596498337)
)

irr_expect <- data.frame(
       ratio = c(1.93175853018373, 1.47619047619048, 1.53658536585366),
       lower = c(1.2863352909086, 0.651952666109562, 0.89726180699843),
       upper = c(2.88430390374334, 3.04596159719188, 2.67042872585681)
)

MH_RR  <- data.frame(est = 1.43636363636364, lower = 0.97698703277564,  upper = 2.11173784979146)
MH_OR  <- data.frame(est = 1.51612903225806, lower = 0.973921554763186, upper = 2.3601975243424)
MH_IRR <- data.frame(est = 1.51729552694154, lower = 1.00321956498486,  upper = 2.2947974665063)

RR_woolf <- data.frame(test.statistic = 0.000364164132934994, 
                       df = 1, 
                       p.value = 0.984774825192526)
OR_woolf <- data.frame(
                       test.statistic = c(6.56694874232559e-05),
                       df = c(1),
                       p.value = c(0.993534276605677))

the_pval <- c(0.00145682488649103, 0.298455836637157, 0.126045957638772)

arrt <- as.data.frame.table(arr) %>%
  dplyr::summarise(res = list(data.frame(
    risk    = rep(risk, Freq),
    outcome = rep(outcome, Freq),
    old = rep(old, Freq)
  ))) %>%
  tidyr::unnest() %>%
  lapply(as.logical) %>%
  tibble::as_tibble()

arrt$pt <- 2 # person time

test_that("internal estimate functions works", {

  expect_equivalent(get_ratio_est(arr, "OR")[1:3, 1:3],  (or_expect))
  expect_equivalent(get_ratio_est(arr, "RR")[1:3, 1:3],  (rr_expect))
  expect_equivalent(get_ratio_est(arr, "IRR")[1:3, 1:3], (irr_expect))

})

test_that("MH estimate works" , {


  expect_equivalent(mh_rr(arr),  MH_RR)
  expect_equivalent(mh_or(arr),  MH_OR)
  expect_equivalent(mh_irr(arr), MH_IRR)

})

test_that("woolf p-values work", {

  expect_equivalent(get_woolf_pval(arr, "RR"), RR_woolf)
  expect_equivalent(get_woolf_pval(arr, "OR"), OR_woolf)

})


OR_strata <- tab_univariate(arrt, outcome, risk, strata = old, measure = "OR", woolf_test = TRUE)
RR_strata <- tab_univariate(arrt, outcome, risk, strata = old, measure = "RR", woolf_test = TRUE)
IRR_strata <- tab_univariate(arrt, outcome, risk, strata = old, perstime = pt, measure = "IRR", woolf_test = TRUE)


test_that("tab_univariate OR works with strata", {

  expect_identical(OR_strata$est_type, c("crude", "old: TRUE", "old: FALSE", "MH", "woolf"))

  # Testing cases and basic ratios
  expect_equal(OR_strata$exp_cases  , c(36 + 10, 36     , 10     , NA, NA))
  expect_equal(OR_strata$unexp_cases, c(25 + 35, 25     , 35     , NA, NA))
  expect_equal(OR_strata$cases_odds , c(46 / 60, 36 / 25, 10 / 35, NA, NA))

  # Testing controls and basic ratios
  expect_equal(OR_strata$exp_controls  , c(164 +  90, 164      , 90      , NA, NA))
  expect_equal(OR_strata$unexp_controls, c(175 + 465, 175      , 465     , NA, NA))
  expect_equal(OR_strata$controls_odds , c(254 / 640, 164 / 175, 90 / 465, NA, NA))

  # Testing odds ratios
  expected <- or_expect[c(1, 3, 2), ] # correct for alphabetical sorting
  this_pval <- the_pval[c(1, 3, 2)]

  expect_equal(OR_strata$ratio  , c(expected$ratio, MH_OR$est  , NA))
  expect_equal(OR_strata$lower  , c(expected$lower, MH_OR$lower, NA))
  expect_equal(OR_strata$upper  , c(expected$upper, MH_OR$upper, NA))
  expect_equal(OR_strata$p.value, c(this_pval     , NA         , OR_woolf$p.value))


}) 

test_that("tab_univariate RR works with strata", {

  expect_identical(RR_strata$est_type, c("crude", "old: TRUE", "old: FALSE", "MH", "woolf"))

  # Testing cases and basic ratios
  expect_equal(RR_strata$exp_cases, c(36 + 10           , 36      , 10      , NA, NA))
  expect_equal(RR_strata$exp_total, c(36 + 10 + 90 + 164, 36 + 164, 10 + 90 , NA, NA))
  expect_equal(RR_strata$exp_risk , c(46 / 300          , 36 / 200, 10 / 100, NA, NA) * 100)

  # Testing controls and basic ratios
  expect_equal(RR_strata$unexp_cases, c(25 + 35            , 25      , 35      , NA, NA))
  expect_equal(RR_strata$unexp_total, c(25 + 35 + 175 + 465, 25 + 175, 35 + 465, NA, NA))
  expect_equal(RR_strata$unexp_risk , c(60 / 700           , 25 / 200, 35 / 500, NA, NA) * 100)

  # Testing odds ratios
  expected <- rr_expect[c(1, 3, 2), ] # correct for alphabetical sorting
  this_pval <- the_pval[c(1, 3, 2)]

  expect_equal(RR_strata$ratio  , c(expected$ratio, MH_RR$est  , NA))
  expect_equal(RR_strata$lower  , c(expected$lower, MH_RR$lower, NA))
  expect_equal(RR_strata$upper  , c(expected$upper, MH_RR$upper, NA))
  expect_equal(RR_strata$p.value, c(this_pval     , NA         , RR_woolf$p.value))


}) 


test_that("tab_univariate works with IRR strata", {

  expect_equal(IRR_strata$exp_cases      , c(36 + 10  , 36      , 10       , NA))
  expect_equal(IRR_strata$exp_perstime   , c(600      , 400     , 200      , NA))
  expect_equal(IRR_strata$exp_incidence  , c(46 / 600 , 36 / 400, 10 / 200 , NA) * 100)

  expect_equal(IRR_strata$unexp_cases    , c(25 + 35  , 25      , 35       , NA))
  expect_equal(IRR_strata$unexp_perstime , c(1400     , 400     , 1000     , NA))
  expect_equal(IRR_strata$unexp_incidence, c(60 / 1400, 25 / 400, 35 / 1000, NA) * 100)

})
