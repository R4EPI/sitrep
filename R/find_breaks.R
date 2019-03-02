#' Automatically calculate breaks for a number
#'
#' @param n a number to calcluate breaks for
#' @param breaks the maximum number of segements you want to have
#' @param snap the number defining where to snap to the nearest factor
#' @param ceiling if `TRUE`, n is included in the breaks
#'
#' @return a vector of integers
#' @export
#'
#' @examples
#'
#' # find four breaks from 1 to 100
#' find_breaks(100)
#'
#' # find four breaks from 1 to 123, rounding to the nearest 20
#' find_breaks(123, snap = 20)
#'
#' # note that there are only three breaks here because of the rounding
#' find_breaks(123, snap = 25)
#'
#' # Include the value itself
#' find_breaks(123, snap = 25, ceiling = TRUE)
find_breaks <- function(n, breaks = 4, snap = 1, ceiling = FALSE) {
  if (snap >= n) stop(sprintf("snap (%d) must be smaller than n (%d)", snap, n))
  seq_by <- ceiling((n/breaks)/snap) * snap
  res <- seq(1, n, by = seq_by)
  if (ceiling) unique(c(res, n)) else res
}
