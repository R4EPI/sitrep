context("template construction tests")

test_that("templates can be built", {

  skip_if_offline() 
  skip_on_cran()
  avail <- available_sitrep_templates()
  p     <- file.path(tempdir(), "template_tests", as.character(Sys.time()))
  dir.create(p)

  for (i in avail) {
  
    expect_identical(check_sitrep_templates(i, path = p, quiet = TRUE, mustwork = FALSE),
                     expected = p, 
                     label = i)

  }
})
