
test_that("polygons can be generated", {

  poly <- suppressWarnings(gen_polygon(LETTERS[1:4]))
  expect_equal(dim(poly), c(4, 2))

})
