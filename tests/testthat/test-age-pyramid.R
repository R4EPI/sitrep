context("age pyramid tests")

set.seed(2018-01-15)
ages <- cut(sample(80, 150, replace = TRUE),
           breaks = c(0, 5, 10, 30, 50, 80, 100), right = FALSE)
sex  <- sample(c("Female", "Male"), 150, replace = TRUE)
ill  <- sample(0:1, 150, replace = TRUE)
dat  <- data.frame(AGE = ages, sex = sex, ill = ill, stringsAsFactors = FALSE)
print(ap1  <- plot_age_pyramid(dat, age_group = "AGE"))
print(ap2  <- plot_age_pyramid(dat, age_group = "AGE", split_by = "ill"))

test_that("age pyramid returns a ggplot2 object", {
  expect_is(ap1, "ggplot")
  expect_is(ap2, "ggplot")
})
