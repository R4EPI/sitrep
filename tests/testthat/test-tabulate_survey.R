# Setup data -------------------------------------------------------------------

data('api', package = 'survey')


# Expected values for yr.rnd stratified by stype from Alex Spina. 
# 
# This shows the manual calculation to make sure things are running
# smoothly
`%<%` <- dplyr::`%>%`

counts <- apistrat %>%
  dplyr::group_by(stype) %>%                       # stratify data
  dplyr::mutate(denom = sum(pw)) %>%               # create column for denominator
  dplyr::group_by(stype, yr.rnd) %>%               # create the stratification by var of interest
  dplyr::summarise(counts = sum(pw, na.rm = TRUE), # create weighted count
                   denom  = unique(denom)) %>%     # isolate denominator
  dplyr::mutate(wgtprop = counts/denom) %>%        # calculate stratified props for factor levels
  dplyr::arrange(yr.rnd, stype)


# Default workflow
s <- srvyr::as_survey_design(apistrat, strata = stype, weights = pw)

# Adding in missing data
aps            <- rbind(apistrat, NA)
aps$pw[201]    <- aps$pw[200]
aps$stype[201] <- aps$stype[200]
sm             <- srvyr::as_survey_design(aps, strata = stype, weights = pw)
sm2            <- srvyr::as_survey_design(aps, strata = stype, weights = pw)

# with the above example
yr_rnd <- tabulate_survey(s, yr.rnd, stype, wide = FALSE, pretty = FALSE)

# with out proptotal
sa_crd_p  <- tabulate_survey(s,
                             awards,
                             stype, 
                             coltotals = TRUE,
                             rowtotals = TRUE,
                             deff      = TRUE)

# with proptotal
sa_pcrd_p <- tabulate_survey(s,
                             awards,
                             stype, 
                             proptotal = TRUE,
                             coltotals = TRUE,
                             rowtotals = TRUE,
                             deff      = TRUE)



# Testing ----------------------------------------------------------------------

test_that("manual calculation matches ours", {

  expect_equal(yr_rnd$proportion, counts$wgtprop)
  expect_equal(yr_rnd$n,          counts$counts)

})

test_that("numeric data are converted to factors", {

  expect_warning(pct <- tabulate_survey(s, pcttest), 
                 "converting `pcttest` to a factor", fixed = TRUE)

  pcf <- cut(apistrat$pcttest, 
             breaks = pretty(range(apistrat$pcttest, na.rm = TRUE)), 
             include.lowest = TRUE)

  expect_identical(levels(pct$pcttest), levels(pcf))

})


test_that("character data works", {

  char <- s %>%
    mutate(yr = as.character(yr.rnd)) %>%
    tabulate_survey(yr, stype, wide = FALSE, pretty = FALSE)

  names(char)[1] <- "yr.rnd"
  expect_identical(char, yr_rnd)

})

test_that("logical data are converted to factors", {

  summer <- s %>%
    mutate(summer = yr.rnd == "No") %>%
    tabulate_survey(summer, stype, wide = FALSE, pretty = FALSE)

  expect_identical(summer[-1], yr_rnd[-1])
  expect_equal(levels(summer[[1]]), c("TRUE", "FALSE"))

})

test_that("integer categorical data are converted to factors", {

  expect_warning({
  summer <- s %>%
    mutate(summer = as.integer(yr.rnd == "Yes")) %>%
    tabulate_survey(summer, stype, wide = FALSE, pretty = FALSE)
  }, "converting `summer` to a factor", fixed = TRUE)

  expect_identical(summer[-1], yr_rnd[-1])
  expect_equal(levels(summer[[1]]), c("0", "1"))

})

test_that("a warning is thrown for missing data", {

  # na.rm = TRUE: WARNING ----------------------
  expect_warning(miss <- tabulate_survey(sm, 
                                         yr.rnd, 
                                         stype, 
                                         na.rm = TRUE,
                                         wide = FALSE, 
                                         pretty = FALSE),
                 "removing 1 missing value(s) from `yr.rnd`", fixed = TRUE)                             

  # na.rm = FALSE: NO WARNING ------------------
  expect_failure({
  expect_warning(miss2 <- tabulate_survey(sm2, 
                                         yr.rnd, 
                                         stype, 
                                         na.rm = FALSE,
                                         wide = FALSE, 
                                         pretty = FALSE),
                 "removing 1 missing value(s) from `yr.rnd`", fixed = TRUE)                             
  })

  # when comparing to known data, the proportions are identical and the
  # confidence intervals are ~nearly~ identical
  expect_equal(miss$proportion,     yr_rnd$proportion)
  expect_equal(miss$proportion_low, yr_rnd$proportion_low, tol = 1e-04)
  expect_equal(miss$proportion_upp, yr_rnd$proportion_upp, tol = 1e-04)
  
  # not dropping missing will result in extra rows to account for the
  # new missing category
  expect_equal(nrow(miss2), nrow(miss) + 3)
  expect_equal(miss2$n[1:nrow(miss)], miss$n)
  
  # the new proportions should be less because of the extra weight
  expect_lt(miss2$proportion[miss2$yr.rnd == "Yes" & miss2$stype == "H"], 
             miss$proportion[ miss$yr.rnd == "Yes" &  miss$stype == "H"])

  expect_lt(miss2$proportion[miss2$yr.rnd == "No"  & miss2$stype == "H"], 
             miss$proportion[ miss$yr.rnd == "No"  &  miss$stype == "H"])

  # The missing row we inserted had a schooltype of "H", so we should expect
  # that the count of the missing values should be identical to the weight:
  expect_equal(miss2$n[miss2$yr.rnd == "(Missing)" & miss2$stype == "H"],
               tail(apistrat$pw, 1))

})

test_that("tabulate_survey will throw an error if the stratification is not correct", {
  
  msg <- paste("The stratification present in the survey object (stype) does",
               "not match the user-specified stratification (awards). If you",
               "want to assess the survey tabulation stratifying by 'awards',",
               "re-specify the survey object with this",
               "strata and the appropriate weights.")
  expect_error(tabulate_survey(s, stype, awards), msg, fixed = TRUE)
  
})

test_that("tabulations return pretty results by default", {
  expect_is(sa_crd_p, "tbl_df")
  expect_is(sa_pcrd_p, "tbl_df")

  expect_named(sa_crd_p, c("awards", "E n", "E ci", "E deff", "H n", "H ci", 
                           "H deff", "M n", "M ci", "M deff", "Total n"))
  
  expect_named(sa_pcrd_p, c("awards", "E n", "E ci", "E deff", "H n", "H ci", 
                           "H deff", "M n", "M ci", "M deff", "Total n"))

  expect_is(sa_crd_p$'E ci' , "character")
  expect_is(sa_crd_p$'E n'  , "numeric")

  expect_is(sa_pcrd_p$'E ci', "character")
  expect_is(sa_pcrd_p$'E n' , "numeric")
})

no_proptot <- tabulate_survey(s,
                              awards,
                              stype,
                              proptotal = FALSE,
                              coltotals = FALSE,
                              rowtotals = FALSE,
                              pretty    = FALSE,
                              wide      = FALSE,
                              deff      = TRUE)

proptot <- tabulate_survey(s,
                           awards,
                           stype,
                           proptotal = TRUE,
                           coltotals = FALSE,
                           rowtotals = FALSE,
                           pretty    = FALSE,
                           wide      = FALSE,
                           deff      = TRUE)

test_that("Proportions are correct", {

  expect_named(proptot, c('awards', 'stype', 'n', 'deff', 'proportion', 'proportion_low', 'proportion_upp'))
  expect_named(no_proptot, c('awards', 'stype', 'n', 'deff', 'proportion', 'proportion_low', 'proportion_upp'))
  expect_equal(sum(proptot$proportion)   , 1)
  expect_equal(sum(no_proptot$proportion), 3)

})




# tabulate_binary_survey tests -------------------------------------------------



test_that("tabulate_binary_survey needs a 'keep' argument", {

  expect_error({
    bin_tot <- tabulate_binary_survey(s,
                                      awards,
                                      yr.rnd,
                                      sch.wide,
                                      proptotal = TRUE,
                                      pretty    = FALSE,
                                      deff      = TRUE)
  }, "Please provide a list of values to keep in the output")

})

bin_tot <- tabulate_binary_survey(s,
                                  awards,
                                  yr.rnd,
                                  sch.wide,
                                  proptotal = TRUE,
                                  pretty    = FALSE,
                                  deff      = TRUE,
                                  wide      = FALSE,
                                  keep      = "Yes")

bin_inv <- tabulate_binary_survey(s,
                                  awards,
                                  yr.rnd,
                                  sch.wide,
                                  proptotal = TRUE,
                                  pretty    = FALSE,
                                  deff      = TRUE,
                                  wide      = FALSE,
                                  invert    = TRUE,
                                  keep      = "Yes")

test_that("tabulate_binary_survey returns complementary proportions", {

  expect_equal(bin_tot$proportion + bin_inv$proportion, c(1,    1,    1))
  expect_equal(bin_tot$n          + bin_inv$n,          c(6194, 6194, 6194))
  expect_equal(bin_tot$deff,                            bin_inv$deff)

})


test_that("transposition doesn't happen without strata", {

  bin_trn <- tabulate_binary_survey(s,
                                    awards,
                                    yr.rnd,
                                    sch.wide,
                                    proptotal = TRUE,
                                    pretty    = FALSE,
                                    deff      = TRUE,
                                    wide      = TRUE,
                                    transpose = "variable",
                                    keep      = "Yes")
  
  expect_identical(bin_trn, bin_tot)

})


test_that("values are sensible in a transposition", {

  
  bin_trn <- tabulate_binary_survey(s,
                                    awards,
                                    yr.rnd,
                                    sch.wide,
                                    strata    = stype,
                                    proptotal = TRUE,
                                    pretty    = FALSE,
                                    deff      = TRUE,
                                    wide      = TRUE,
                                    transpose = "variable",
                                    keep      = "Yes")
  bin_str <- tabulate_binary_survey(s,
                                    awards,
                                    yr.rnd,
                                    sch.wide,
                                    strata    = stype,
                                    proptotal = TRUE,
                                    pretty    = FALSE,
                                    deff      = TRUE,
                                    wide      = TRUE,
                                    transpose = NULL,
                                    keep      = "Yes")

  trn_props <- bin_trn[grepl("proportion", names(bin_trn))]
  str_props <- bin_str[grepl("proportion", names(bin_str))]

  # The tables are not equal
  expect_failure(expect_equal(trn_props, str_props))
  # Their names don't match
  expect_failure(expect_named(trn_props, str_props))
  # but they do sum to the same value
  expect_equal(sum(trn_props), sum(str_props))

})
