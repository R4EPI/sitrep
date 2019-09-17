
outbreaks <- c("Measles", "Cholera", "AJS", "Meningitis")

test_that("msf_dict works", {

  for (disease in c(outbreaks)) {
    nested  <- msf_dict(disease)
    long    <- msf_dict(disease, compact = FALSE)
    neither <- msf_dict(disease, compact = FALSE, long = FALSE)

    expect_is(nested, "tbl_df", label = disease)
    expect_is(long, "tbl_df", label = disease)
    expect_is(neither, "list", label = disease)
    expect_named(neither, c("dictionary", "options"), label = disease)
    expect_equal(nrow(neither$dictionary), nrow(nested), label = disease)
    expect_equal(ncol(long), sum(vapply(neither, ncol, integer(1))) - 1L, label = disease)
    expect_equal(ncol(neither$dictionary) + 1L, ncol(nested), label = disease, info = disease)
  
  }


})

test_that("data can be generated", {

  for (disease in c(outbreaks)) {
    dictionary <- msf_dict(disease)
    data       <- gen_data(disease, numcases = 30)

    expect_is(data, "tbl_df", label = disease)
    expect_equal(nrow(dictionary), ncol(data), label = disease)
  }
})
