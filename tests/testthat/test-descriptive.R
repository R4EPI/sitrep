context('descriptive table tests')



humans     <- dplyr::filter(dplyr::starwars, species == "Human")
# table of hair color
hair_table <- dplyr::select(tab_linelist(humans, hair_color), -variable)
hair_table <- dplyr::rename(hair_table, hair_color = value)
# table of homeworld X eye color (homeworld is missing for 5 characters)
home_eye   <- tab_linelist(humans, homeworld, strata = eye_color, na.rm = FALSE)
home_eye   <- dplyr::select(home_eye, -variable)
home_eye   <- dplyr::rename(home_eye, homeworld = value)


# as above, but percentages are proportional to whole data set
phair_table <- tab_linelist(humans, hair_color, prop_total = TRUE, na.rm = FALSE)
phome_eye   <- tab_linelist(humans, homeworld, strata = eye_color, prop_total = TRUE, na.rm = FALSE)

test_that("descriptive doesn't force zero counts to missing for binary", {

  onezero <- data.frame(x = c(1, 0, 0, 1, NA))
  expect_warning(res <- descriptive(onezero, x),
                 "converting `x` to a factor", fixed = TRUE)
  expect_equal(nrow(res), 3)
  expect_equal(res$n, c(2, 2, 1))
  expect_equivalent(res$x, forcats::fct_inorder(c("0", "1", "Missing")))

})


test_that("descriptive doesn't use cut for five categories", { 

  onezero <- data.frame(x = c(1, 0, 0, 1, NA, 42, 37, 341))
  expect_warning(res <- descriptive(onezero, x),
                 "converting `x` to a factor", fixed = TRUE)
  expect_equal(nrow(res), 6)
  expect_equal(res$n, c(2, 2, 1, 1, 1, 1))
  expect_equivalent(res$x, forcats::fct_inorder(c("0", "1", "37", "42", "341", "Missing")))

})


test_that("descriptive returns a table of counts and proportions that sum to 100", {

  skip_on_cran()

  expect_is(hair_table, "tbl_df")
  expect_named(hair_table, c('hair_color', 'n', 'proportion'))
  expect_equal(sum(hair_table$proportion), 100)
  expect_equal(sum(phair_table$proportion), 100)
  expect_equal(sum(hair_table$n), nrow(humans))
  expect_equal(sum(phair_table$n), nrow(humans))

  # can do NSE and quoted
  hc <- "hair_color"
  ht <- dplyr::select(tab_linelist(humans, hc), -variable)
  ht <- dplyr::rename(ht, hair_color = value)

  expect_identical(hair_table, ht) 

})

test_that("descriptive will convert numbers to factors with cut", {

  massive <- cut(humans$mass, 
                 breaks = pretty(range(humans$mass, na.rm = TRUE)), 
                 include.lowest = TRUE)

  expect_warning(humass <- descriptive(humans, mass, gender), 
                 "converting `mass` to a factor", fixed = TRUE)
  expect_identical(levels(humass$mass), c(levels(massive), "Missing"))

})


test_that("descriptive will convert logicals to factors", {

  humans$luke <- humans$name == "Luke Skywalker"
  huluke <- descriptive(humans, luke, gender)
  expect_identical(huluke$luke, factor(c(TRUE, FALSE), as.character(c(TRUE, FALSE))))

})

test_that("descriptive will add rows and columns for totals", {

  skip_on_cran()
  hair_table_rc <- descriptive(humans, hair_color, coltotals = TRUE, rowtotals = TRUE)

  # There will be an extra column
  expect_named(hair_table_rc, c('hair_color', 'n', 'proportion', 'Total'))

  # The last row will be total
  expect_identical(hair_table_rc$hair_color[nrow(hair_table_rc)], "Total")

  # The sum of the tabulations plus total will be double of the tabulations
  expect_identical(sum(hair_table_rc$n), sum(hair_table$n) * 2)

  # The final row should be 100
  expect_identical(hair_table_rc$proportion[nrow(hair_table_rc)], 100)
  
  # The total will be the same as n for no grouping
  expect_identical(hair_table_rc$n, hair_table_rc$Total)

})




test_that("descriptive can take a grouping factor", {

  skip_on_cran()
  expect_is(home_eye, "tbl_df")
  expect_warning({
    phome_eye_m <- descriptive(humans, homeworld, eye_color, proptotal = TRUE, explicit_missing = FALSE)
  }, "Removing 5 missing values")

  expect_identical(as.character(home_eye$homeworld[nrow(home_eye)]), "Missing")

  # The columns will be alphabetical if it's not a factor
  the_columns <- sprintf(c("%s n", "%s proportion"), rep(sort(unique(humans$eye_color)), each = 2))

  expect_named(home_eye, c("homeworld", the_columns))

  # all "n" should add up to the number of rows in the data set
  expect_equal(sum(dplyr::select(home_eye, dplyr::ends_with(" n"))), nrow(humans)) 
  expect_equal(sum(dplyr::select(phome_eye, dplyr::ends_with(" n"))), nrow(humans)) 
  expect_equal(sum(dplyr::select(phome_eye_m, dplyr::ends_with(" n"))), nrow(humans) - 5) 

  # all proportions should add up to 100 per column by default
  expect_equivalent(colSums(dplyr::select(home_eye, dplyr::ends_with(" proportion"))), rep(100, 6))
  # overall if proptotal = TRUE
  expect_equal(sum(colSums(dplyr::select(phome_eye, dplyr::ends_with(" proportion")))), 100)
  # missing data excluded does not affect the total
  expect_equal(sum(colSums(dplyr::select(phome_eye_m, dplyr::ends_with(" proportion")))), 100)

})


test_that("descriptive will respect ordering of the grouping factor", {

  skip_on_cran()
 
  hf <- humans %>% 
    mutate(eye_color = forcats::fct_inorder(eye_color)) %>%
    descriptive(homeworld, eye_color)

  the_columns <- sprintf(c("%s n", "%s proportion"), rep(unique(humans$eye_color), each = 2))

  expect_named(hf, c("homeworld", the_columns))

})


