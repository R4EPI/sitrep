# vim foldmethod:marker
`%>%` <- dplyr::`%>%`

# Data setup (type :zo to unroll and :zm to roll up) {{{
#
# tf <- function(n) sample(c("Yes", "No"), n, replace = TRUE) 
# i  <- function(x, n) sample(c("", x), n, replace = TRUE, prob = c(0.75, 0.25))
# set.seed(2019-07-18)
# d <- tibble(
#   itch       = tf(100),
#   fever      = tf(100),
#   bleeding   = tf(100),
#   CHOICE_001 = i("itch", 100),
#   CHOICE_002 = i("fever", 100),
#   CHOICE_003 = i("bleeding", 100),
#   weights    = round(rnorm(100, 4)),
# )
d <- tibble::tribble(
  ~itch, ~fever, ~bleeding, ~CHOICE_001, ~CHOICE_002, ~CHOICE_003, ~weights,
   "No",  "Yes",      "No",          "",          "",          "",        4,
   "No",  "Yes",      "No",          "",          "",          "",        3,
   "No",  "Yes",     "Yes",          "",          "",          "",        5,
  "Yes",   "No",      "No",          "",          "",          "",        4,
   "No",  "Yes",      "No",      "itch",          "",          "",        3,
   "No",  "Yes",      "No",          "",          "",  "bleeding",        3,
   "No",   "No",      "No",          "",     "fever",  "bleeding",        3,
  "Yes",  "Yes",      "No",      "itch",     "fever",  "bleeding",        4,
  "Yes",  "Yes",      "No",      "itch",     "fever",          "",        5,
   "No",   "No",      "No",      "itch",          "",          "",        5,
  "Yes",   "No",     "Yes",          "",          "",          "",        2,
  "Yes",   "No",      "No",          "",          "",          "",        5,
   "No",  "Yes",     "Yes",          "",          "",          "",        4,
  "Yes",   "No",     "Yes",          "",          "",  "bleeding",        4,
   "No",  "Yes",      "No",          "",          "",          "",        3,
  "Yes",  "Yes",     "Yes",      "itch",          "",          "",        3,
   "No",   "No",      "No",      "itch",     "fever",  "bleeding",        6,
   "No",   "No",     "Yes",          "",          "",          "",        4,
   "No",   "No",     "Yes",          "",          "",          "",        3,
   "No",   "No",     "Yes",      "itch",          "",          "",        4,
   "No",   "No",     "Yes",          "",          "",          "",        4,
  "Yes",  "Yes",      "No",          "",          "",          "",        4,
  "Yes",   "No",     "Yes",          "",     "fever",          "",        6,
   "No",   "No",      "No",          "",          "",          "",        5,
   "No",   "No",      "No",          "",          "",          "",        4,
   "No",  "Yes",      "No",          "",          "",          "",        4,
  "Yes",  "Yes",      "No",          "",          "",          "",        4,
   "No",   "No",     "Yes",          "",          "",          "",        2,
  "Yes",   "No",      "No",          "",     "fever",          "",        4,
   "No",  "Yes",     "Yes",      "itch",          "",          "",        4,
   "No",   "No",      "No",      "itch",     "fever",          "",        4,
   "No",   "No",      "No",          "",     "fever",          "",        5,
   "No",   "No",     "Yes",      "itch",          "",          "",        3,
  "Yes",   "No",      "No",          "",     "fever",          "",        3,
   "No",   "No",      "No",          "",          "",          "",        4,
   "No",  "Yes",      "No",          "",          "",          "",        5,
   "No",   "No",      "No",      "itch",          "",          "",        2,
  "Yes",   "No",     "Yes",          "",          "",          "",        4,
  "Yes",  "Yes",     "Yes",          "",          "",          "",        2,
   "No",   "No",      "No",          "",          "",          "",        5,
  "Yes",  "Yes",     "Yes",      "itch",          "",          "",        3,
  "Yes",   "No",      "No",          "",          "",          "",        5,
  "Yes",   "No",     "Yes",          "",          "",          "",        4,
  "Yes",   "No",      "No",          "",          "",  "bleeding",        5,
  "Yes",   "No",      "No",          "",     "fever",          "",        3,
   "No",  "Yes",     "Yes",      "itch",     "fever",          "",        3,
   "No",  "Yes",     "Yes",          "",          "",          "",        3,
  "Yes",  "Yes",     "Yes",          "",          "",          "",        3,
  "Yes",   "No",     "Yes",      "itch",          "",  "bleeding",        3,
  "Yes",   "No",      "No",          "",          "",  "bleeding",        4,
   "No",   "No",     "Yes",      "itch",          "",          "",        3,
   "No",  "Yes",     "Yes",          "",          "",          "",        2,
   "No",   "No",     "Yes",          "",          "",          "",        3,
  "Yes",  "Yes",      "No",          "",          "",          "",        4,
  "Yes",   "No",     "Yes",          "",          "",          "",        4,
   "No",   "No",      "No",          "",          "",          "",        3,
   "No",  "Yes",      "No",      "itch",          "",          "",        6,
  "Yes",   "No",     "Yes",      "itch",          "",  "bleeding",        6,
   "No",   "No",      "No",          "",     "fever",          "",        4,
   "No",   "No",     "Yes",          "",     "fever",  "bleeding",        5,
   "No",   "No",     "Yes",          "",          "",  "bleeding",        4,
   "No",  "Yes",     "Yes",          "",          "",  "bleeding",        4,
   "No",  "Yes",     "Yes",      "itch",          "",  "bleeding",        3,
   "No",  "Yes",     "Yes",          "",     "fever",          "",        3,
  "Yes",  "Yes",     "Yes",          "",          "",          "",        5,
  "Yes",   "No",      "No",          "",     "fever",          "",        5,
  "Yes",   "No",     "Yes",          "",          "",          "",        4,
   "No",   "No",     "Yes",          "",     "fever",          "",        2,
   "No",   "No",      "No",          "",          "",          "",        6,
   "No",  "Yes",      "No",          "",          "",          "",        5,
   "No",   "No",     "Yes",          "",          "",  "bleeding",        4,
  "Yes",  "Yes",     "Yes",      "itch",     "fever",          "",        4,
   "No",   "No",     "Yes",          "",          "",          "",        4,
  "Yes",   "No",     "Yes",      "itch",     "fever",  "bleeding",        3,
  "Yes",  "Yes",      "No",          "",     "fever",          "",        4,
  "Yes",  "Yes",      "No",          "",          "",  "bleeding",        4,
  "Yes",  "Yes",     "Yes",          "",          "",  "bleeding",        3,
   "No",  "Yes",     "Yes",          "",          "",          "",        3,
  "Yes",  "Yes",      "No",          "",     "fever",          "",        7,
  "Yes",  "Yes",      "No",          "",     "fever",          "",        4,
   "No",   "No",     "Yes",          "",          "",          "",        4,
   "No",   "No",      "No",          "",          "",          "",        6,
  "Yes",   "No",     "Yes",          "",          "",          "",        5,
  "Yes",  "Yes",      "No",          "",          "",          "",        4,
   "No",  "Yes",     "Yes",      "itch",          "",  "bleeding",        4,
   "No",   "No",     "Yes",          "",          "",          "",        4,
   "No",   "No",      "No",          "",          "",          "",        5,
   "No",   "No",     "Yes",          "",          "",          "",        6,
   "No",  "Yes",     "Yes",      "itch",     "fever",          "",        4,
  "Yes",   "No",     "Yes",          "",     "fever",          "",        5,
   "No",  "Yes",      "No",      "itch",          "",          "",        4,
   "No",   "No",     "Yes",          "",          "",          "",        5,
   "No",  "Yes",     "Yes",          "",          "",          "",        6,
  "Yes",  "Yes",     "Yes",          "",          "",  "bleeding",        2,
   "No",  "Yes",      "No",          "",          "",          "",        3,
   "No",  "Yes",     "Yes",          "",          "",          "",        4,
   "No",   "No",      "No",          "",     "fever",  "bleeding",        4,
   "No",  "Yes",     "Yes",          "",          "",          "",        3,
   "No",   "No",     "Yes",          "",          "",          "",        3,
  "Yes",  "Yes",      "No",          "",          "",          "",        3
  )

d$strata <- factor(rep(letters[1:3], length.out = 100))
# with strata
s <- srvyr::as_survey_design(d, weights = weights, strata = strata)
# without strata
nss <- srvyr::as_survey_design(d, weights = weights)

# }}}

# Expectations setup {{{


md_expect_symptom <- tibble::tribble(
    ~variable,     ~value,  ~n, ~proportion,
       "itch",      "Yes",  40,    40,
      "fever",      "Yes",  45,    45,
   "bleeding",      "Yes",  53,    53
  )

md_expect_choice <- tibble::tribble(
     ~variable,     ~value,  ~n, ~proportion,
  "CHOICE_001",     "itch",  23,    23,
  "CHOICE_002",    "fever",  24,    24,
  "CHOICE_003", "bleeding",  20,    20
  )


tbs_expect_choice <- tibble::tribble(
     ~variable,     ~value,  ~n,                  ~ci,
  "CHOICE_001",     "itch",  89, "22.5% (15.1--32.2)",
  "CHOICE_002",    "fever", 100, "25.3% (17.2--35.4)",
  "CHOICE_003", "bleeding",  78, "19.7% (12.8--29.1)"
  )

tbs_expect_symptom <- tibble::tribble(
   ~variable, ~value,  ~n,                  ~ci,
      "itch",  "Yes", 160, "40.4% (30.8--50.8)",
     "fever",  "Yes", 170, "42.9% (33.2--53.3)",
  "bleeding",  "Yes", 197, "49.7% (39.6--59.9)"
  )



# }}}

SYMPTOMS <- c("itch", "fever", "bleeding")


# Basic tests---no strata {{{

# These tests will check that tab_survey and tab_linelist can both handle the
# same types of data and will return tables sensibly. 

test_that("tab_survey can give results similar to the old tabulate_binary_survey", {

  res_choice  <- tab_survey(nss, tidyselect::starts_with("CHOICE"), drop = "")
  res_symptom <- tab_survey(nss, SYMPTOMS, keep = "Yes")

  expect_identical(res_choice, tbs_expect_choice)
  expect_identical(res_symptom, tbs_expect_symptom)

})

test_that("survey---transposition without strata will give the same order and values", {

  n_choice  <- tab_survey(s, tidyselect::starts_with("CHOICE"), drop = "", transpose = "value") %>%
    dplyr::select(dplyr::ends_with(" n")) %>%
    unlist(use.names = FALSE)
  n_symptom <- tab_survey(s, SYMPTOMS, keep = "Yes", transpose = "variable") %>%
    dplyr::select(dplyr::ends_with(" n")) %>%
    unlist(use.names = FALSE)

  expect_equal(n_choice,  tbs_expect_choice$n)
  expect_equal(n_symptom, tbs_expect_symptom$n)

})

test_that("tab_linelist can give results similar to the old multi_descriptive", {
  
  res_choice  <- tab_linelist(d, tidyselect::starts_with("CHOICE"), drop = "")
  res_symptom <- tab_linelist(d, SYMPTOMS, keep = "Yes")
  res_symptom_transpose <- tab_linelist(d, SYMPTOMS, transpose = "value")

  expect_identical(res_choice, md_expect_choice)
  expect_identical(res_symptom, md_expect_symptom)

})


test_that("transposition can happen without strata by value", {
  
  res_symptom_transpose <- tab_linelist(d, SYMPTOMS, transpose = "value")

  expect_identical(md_expect_symptom$n,          res_symptom_transpose[["Yes n"]])
  expect_identical(md_expect_symptom$proportion, res_symptom_transpose[["Yes proportion"]])

})

test_that("transposition can happen without strata by variable", {
  
  res_symptom_transpose <- tab_linelist(d, SYMPTOMS, transpose = "variable")
  expect_equivalent(md_expect_symptom$n, unlist(res_symptom_transpose[2, c(2, 4, 6)]))
  expect_equivalent(md_expect_symptom$proportion, unlist(res_symptom_transpose[2, c(3, 5, 7)]))

})

test_that("linelist---transposition without strata will give the same order and values", {

  n_choice  <- tab_linelist(d, tidyselect::starts_with("CHOICE"), drop = "", transpose = "value") %>%
    dplyr::select(dplyr::ends_with(" n")) %>%
    unlist(use.names = FALSE)
  n_symptom <- tab_linelist(d, SYMPTOMS, keep = "Yes", transpose = "variable") %>%
    dplyr::select(dplyr::ends_with(" n")) %>%
    unlist(use.names = FALSE)

  expect_equal(n_choice,  md_expect_choice$n)
  expect_equal(n_symptom, md_expect_symptom$n)

})
# }}}

# Tests with strata {{{

test_that("survey---adding strata works", {

  s_res_choice  <- tab_survey(s, tidyselect::starts_with("CHOICE"), strata = strata, drop = "")
  s_res_symptom <- tab_survey(s, SYMPTOMS, strata = strata, keep = "Yes")

  expect_equal(ncol(s_res_choice), 8L)
  expect_equal(ncol(s_res_symptom), 8L)

  sum_choice  <- s_res_choice %>% dplyr::select(dplyr::ends_with(" n")) %>% rowSums()
  sum_symptom <- s_res_symptom %>% dplyr::select(dplyr::ends_with(" n")) %>% rowSums()

  expect_equal(sum_choice, tbs_expect_choice$n)
  expect_equal(sum_symptom, tbs_expect_symptom$n)
  
})

test_that("linelist---adding strata works", {

  s_res_choice  <- tab_linelist(d, tidyselect::starts_with("CHOICE"), strata = strata, drop = "")
  s_res_symptom <- tab_linelist(d, SYMPTOMS, strata = strata, keep = "Yes")

  expect_equal(ncol(s_res_choice), 8L)
  expect_equal(ncol(s_res_symptom), 8L)

  sum_choice  <- s_res_choice %>% dplyr::select(dplyr::ends_with(" n")) %>% rowSums()
  sum_symptom <- s_res_symptom %>% dplyr::select(dplyr::ends_with(" n")) %>% rowSums()

  expect_equal(sum_choice, md_expect_choice$n)
  expect_equal(sum_symptom, md_expect_symptom$n)
  
})

# }}}
