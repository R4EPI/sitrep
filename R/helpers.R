# Helper function to splice a data.frame with multiple columns
# into a dplyr function
# example
# dplyr::summarise(mtcars,
# n = n(),
# x = sum(hp > 100),
# !!!splice_df(binom::binom.wilson(x, n), mean, lower, upper)
# )
splice_df <- function(x, ...) {
  expr <- rlang::enquo(x)
  cols <- lapply(rlang::ensyms(..., .named = TRUE), as.character)
  lapply(cols, function(col_name) {
    rlang::quo(`[[`(!!expr, !!col_name))
  })
}
