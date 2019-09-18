test_that("msf_dict_survey works", {

  surveys <- c("Vaccination", "Nutrition", "Mortality")

  for (survey in surveys) {

    nested  <- msf_dict_survey(survey, compact = TRUE)
    long    <- msf_dict_survey(survey, compact = FALSE)

    expect_is(nested, "tbl_df", label = survey)
    expect_is(long, "tbl_df", label = survey)
    expect_equal(ncol(nested) + 2, ncol(long), label = survey)
  
  }

})

