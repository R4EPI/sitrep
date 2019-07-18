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


# This will convert numbers to factors.
#
# If the number of unique numbers is five or fewer, then they will simply
# be converted to factors in order, otherwise, they will be passed to cut and
# pretty, preserving the lowest value. 
#
#' create factors from numbers
#'
#' @param x a vector of integers or numerics
#'
#' @noRd
#' @return a factor
fac_from_num <- function(x) {
  # count the number of unique numbers
  udc <- sort(unique(x))
  udc <- as.character(udc)

  if (length(udc) < 6) {
    x <- factor(as.character(x), levels = udc)
  } else {
    x <- cut(x, 
             breaks = pretty(range(x, na.rm = TRUE)),
             include.lowest = TRUE
             )
  }
  x
}
