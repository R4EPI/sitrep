context("age pyramid tests")

set.seed(2018-01-15)
ages <- cut(sample(80, 150, replace = TRUE),
           breaks = c(0, 5, 10, 30, 50, 80, 100), right = FALSE)
sex  <- sample(c("Female", "Male"), 150, replace = TRUE)
gender <- sex
gender[sample(5)] <- "NB"
ill  <- sample(0:1, 150, replace = TRUE)
dat  <- data.frame(AGE             = ages,
                  sex              = factor(sex, c("Male", "Female")),
                  gender           = factor(gender, c("Male", "NB", "Female")),
                  ill              = ill,
                  stringsAsFactors = FALSE)
ap1  <- plot_age_pyramid(dat, age_group = "AGE")
ap2  <- plot_age_pyramid(dat, age_group = "AGE", split_by = "ill")
apg  <- plot_age_pyramid(dat, age_group = "AGE", split_by = gender)
apnp <- plot_age_pyramid(dat, age_group = AGE, pyramid = FALSE)
# missing data
datd <- dat[dat$AGE != levels(dat$AGE)[2], , drop = FALSE]
ap3  <- plot_age_pyramid(datd, age_group = "AGE")

test_that("plots appear the same", {
           
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("default age pyramid", ap1)
  vdiffr::expect_doppelganger("ill age pyramid"    , ap2)
  vdiffr::expect_doppelganger("missing age pyramid", ap3)
  vdiffr::expect_doppelganger("gender age pyramid" , apg)
  vdiffr::expect_doppelganger("default no pyramid" , apnp)

})


test_that("age pyramid returns a ggplot2 object", {
  expect_is(ap1, "ggplot")
  expect_is(ap2, "ggplot")
})

test_that("toggling pyramid will turn off the pyramid", {

  expect_identical(names(ap1$data), names(apnp$data))
  # the absolute values of pyramid data are equal to the values of non-pyramid data
  expect_identical(abs(ap1$data$n), apnp$data$n)
  expect_failure(expect_equal(ap1$data$n, apnp$data$n))

})

test_that("choosing a column that doesn't exist results in an error", {
  expect_error(plot_age_pyramid(dat, age_group = "pourg_ega"))
})

test_that("plot by gender works", {
  
  expect_true("gender" %in% colnames(apg$data))
  expect_equal(levels(apg$data$gender), c("Male", "NB", "Female"))

})

test_that("plot by sex default works", {
  expect_true("sex" %in% colnames(ap1$data))
  expect_equal(levels(ap1$data$sex), c("Male", "Female"))
  expect_true("AGE" %in% colnames(ap1$data))
  expect_false("ill" %in% colnames(ap1$data))
})

test_that("plot by ill works", {
  expect_true("ill" %in% colnames(ap2$data))
  expect_true("AGE" %in% colnames(ap2$data))
  expect_false("sex" %in% colnames(ap2$data))
  expect_equal(unique(ap2$data$ill), as.character(0:1))
})

test_that("missing levels are still plotted", {
  # Complete data has both groups
  expect_equal(sum(ap1$data$AGE == levels(dat$AGE)[2]), 2)
  # Incomplete data has groups replaced with NA
  expect_equal(sum(ap3$data$AGE == levels(dat$AGE)[2]), 0)
})

test_that("missing split data are removed before plotting", {
  dat$sex[69] <- NA
  expect_warning(plot_age_pyramid(dat, age_group = "AGE"),
                 "removing 1 observations with missing values between the sex and sex column")
})
