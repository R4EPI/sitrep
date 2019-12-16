context("age pyramid tests")

# set.seed(2018-01-15)
# ages <- cut(sample(80, 150, replace = TRUE),
#            breaks = c(0, 5, 10, 30, 50, 80, 100), right = FALSE)
# sex  <- sample(c("Female", "Male"), 150, replace = TRUE)
# gender <- sex
# gender[sample(5)] <- "NB"
# ill  <- sample(0:1, 150, replace = TRUE)
ages <-  structure(c(4L, 4L, 5L, 3L, 3L, 1L, 5L, 5L, 4L, 3L, 5L, 2L, 5L, 5L,
                     2L, 3L, 5L, 4L, 3L, 5L, 1L, 3L, 5L, 3L, 1L, 5L, 2L, 5L,
                     4L, 3L, 4L, 2L, 5L, 1L, 4L, 3L, 4L, 3L, 3L, 4L, 4L, 2L,
                     4L, 3L, 3L, 4L, 3L, 4L, 2L, 5L, 5L, 4L, 4L, 2L, 5L, 3L,
                     5L, 4L, 4L, 5L, 3L, 5L, 2L, 4L, 4L, 5L, 5L, 5L, 5L, 5L,
                     5L, 3L, 1L, 4L, 3L, 3L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 3L,
                     5L, 5L, 5L, 3L, 4L, 1L, 3L, 5L, 5L, 3L, 5L, 5L, 4L, 5L,
                     5L, 2L, 4L, 3L, 2L, 3L, 5L, 4L, 3L, 4L, 6L, 3L, 4L, 3L,
                     5L, 3L, 5L, 5L, 4L, 5L, 4L, 4L, 4L, 5L, 5L, 5L, 3L, 2L,
                     5L, 1L, 3L, 5L, 5L, 4L, 2L, 2L, 5L, 5L, 1L, 1L, 2L, 4L,
                     5L, 4L, 3L, 5L, 3L, 5L, 3L, 4L, 4L, 4L), 
                   .Label = c("[0,5)", "[5,10)", "[10,30)", "[30,50)", "[50,80)", "[80,100)"), 
                   class = "factor")
sex <- structure(c(1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 1L,
                   1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L,
                   2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L,
                   2L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L,
                   2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
                   1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L,
                   1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 2L,
                   2L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L,
                   1L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L,
                   2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L),
                 .Label = c("Male", "Female"), 
                 class = "factor")
gender <- structure(c(2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 1L, 3L,
                      1L, 1L, 3L, 3L, 1L, 3L, 3L, 3L, 1L, 3L, 1L, 3L, 3L, 3L,
                      1L, 1L, 3L, 1L, 3L, 3L, 1L, 1L, 1L, 3L, 1L, 1L, 1L, 1L,
                      3L, 1L, 1L, 3L, 1L, 1L, 1L, 3L, 3L, 1L, 1L, 1L, 3L, 1L,
                      3L, 1L, 1L, 3L, 3L, 1L, 3L, 3L, 3L, 1L, 3L, 3L, 1L, 1L,
                      1L, 3L, 3L, 3L, 3L, 1L, 3L, 1L, 1L, 1L, 1L, 3L, 1L, 3L,
                      3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 1L, 3L,
                      3L, 3L, 3L, 3L, 1L, 1L, 3L, 3L, 3L, 1L, 3L, 3L, 1L, 1L,
                      3L, 1L, 1L, 3L, 3L, 1L, 1L, 3L, 1L, 3L, 3L, 1L, 3L, 1L,
                      3L, 1L, 1L, 1L, 3L, 1L, 3L, 1L, 3L, 3L, 1L, 3L, 1L, 3L,
                      1L, 3L, 3L, 1L, 3L, 1L, 1L, 1L, 3L, 1L), 
                    .Label = c("Male", "NB", "Female"), 
                    class = "factor")
ill <- c(0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 0L,
         0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L,
         0L, 1L, 1L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L,
         1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L,
         1L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 1L,
         0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L,
         1L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 1L,
         0L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L,
         0L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 1L)

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
           
  old <- ggplot2::theme_set(ggplot2::theme_classic(base_size = 18))
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("default age pyramid", ap1)
  vdiffr::expect_doppelganger("ill age pyramid"    , ap2)
  vdiffr::expect_doppelganger("missing age pyramid", ap3)
  vdiffr::expect_doppelganger("gender age pyramid" , apg)
  vdiffr::expect_doppelganger("default no pyramid" , apnp)
  ggplot2::theme_set(old)

})


test_that("age pyramid returns a ggplot2 object", {
  expect_is(ap1, "ggplot")
  expect_is(ap2, "ggplot")
})

