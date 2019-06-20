data('api', package = 'survey')

s <- srvyr::as_survey_design(apistrat, strata = stype, weights = pw)


# with out proptotal
sa_crd_p  <- tabulate_survey(s,
                             stype, 
                             awards,
                             coltotals = TRUE,
                             rowtotals = TRUE,
                             deff      = TRUE)

# with proptotal
sa_pcrd_p <- tabulate_survey(s,
                             stype, 
                             awards,
                             proptotal = TRUE,
                             coltotals = TRUE,
                             rowtotals = TRUE,
                             deff      = TRUE)

test_that("tabulations return pretty results by default", {
  expect_is(sa_crd_p, "tbl_df")
  expect_is(sa_pcrd_p, "tbl_df")

  expect_named(sa_crd_p, c('stype', 'No n', 'No ci', 'No deff', 
                           'Yes n', 'Yes ci', 'Yes deff', 'Total n'))
  
  expect_named(sa_pcrd_p, c('stype', 'No n', 'No ci', 'No deff', 
                            'Yes n', 'Yes ci', 'Yes deff', 'Total n'))

  expect_is(sa_crd_p$'No ci' , "character")
  expect_is(sa_crd_p$'No n'  , "character")

  expect_is(sa_pcrd_p$'No ci', "character")
  expect_is(sa_pcrd_p$'No n' , "character")
})

no_proptot <- tabulate_survey(s,
                              stype,
                              awards,
                              proptotal = FALSE,
                              coltotals = FALSE,
                              rowtotals = FALSE,
                              pretty    = FALSE,
                              wide      = FALSE,
                              deff      = TRUE)

proptot <- tabulate_survey(s,
                           stype,
                           awards,
                           proptotal = TRUE,
                           coltotals = FALSE,
                           rowtotals = FALSE,
                           pretty    = FALSE,
                           wide      = FALSE,
                           deff      = TRUE)

test_that("Proportions are correct", {

  expect_named(proptot, c('awards', 'stype', 'n', 'deff', 'proportion', 'proportion_lower', 'proportion_upper'))
  expect_named(no_proptot, c('awards', 'stype', 'n', 'deff', 'proportion', 'proportion_lower', 'proportion_upper'))
  expect_equal(sum(proptot$proportion)   , 1)
  expect_equal(sum(no_proptot$proportion), 2)

})
