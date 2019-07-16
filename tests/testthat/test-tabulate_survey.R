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

test_that("a warning is thrown for missing data", {
                             
  expect_warning(miss <- tabulate_survey(sm, yr.rnd, stype, wide = FALSE, pretty = FALSE),
                 "removing 1 missing value(s)", fixed = TRUE)                             
  # when comparing to known data, it's identical
  expect_identical(miss$n, yr_rnd$n)
  expect_identical(miss$proportion, yr_rnd$proportion)

})

test_that("tabulate_survey will throw an error if the stratification is not correct", {
  
  msg <- paste("The stratification present in the survey object \\(%s\\) does",
               "not match the user-specified stratification \\(%s\\). If you",
               "want to assess the survey tabulation stratifying by '%s',",
               "re-specify the survey object with this",
               "strata and the appropriate weights.")
  expected <- sprintf(msg, "stype", "awards", "awards")
  expect_error(tabulate_survey(s, stype, awards), expected)
  
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
