context("template construction tests")

test_that("templates can be built", {

  skip_if_offline() 
  skip_on_cran()
  avail <- available_sitrep_templates(full.names = TRUE)
  p     <- file.path(tempdir(), "template_tests", format(Sys.time(), "%F%H%M%S"))
  dir.create(p, recursive = TRUE)

  for (i in avail) {

    tmp <- check_sitrep_templates(i, path = p, quiet = TRUE, mustwork = TRUE)
    expect_is(tmp, "character", label = sprintf("f: %s, msg: %s", i, tmp[[1]]))

  }
})
