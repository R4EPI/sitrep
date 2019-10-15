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
