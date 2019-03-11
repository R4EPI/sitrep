#' Unite estimates and confidence intervals
#'
#' @param x a data frame with at least three columns defining an estimate, lower
#' bounds, and upper bounds.
#' @param col the quoted name of the replacement column to create
#' @param ... three columns to bind together in the order of Estimate, Lower, and
#' Upper.
#' @param remove if `TRUE` (default), the three columns in `...` will be replaced by `col`
#' @param digits the number of digits to retain for the confidence interval.
#' @param percent if the result should be multiplied by 100 and have a "%" symbol added.
#' @param rate if `percent = FALSE`, `rate = TRUE` will add a percent symbol to the result.
#'
#' @export
#' @examples
#'
#' print(cfr <- case_fatality_rate((1:4)*10, 50))
#' unite_ci(cfr, "CFR (CI)", cfr, lower, upper, rate = TRUE)
#'
unite_ci <- function(x, col = NULL, ..., remove = TRUE, digits = 2, percent = FALSE, rate = FALSE) {

  from_vars <- tidyselect::vars_select(colnames(x), ...)
  if (length(from_vars) != 3) {
    stop("This function requires three columns: an estimate, a lower value, and an upper value")
  }
  if (is.null(col)) {
    col <- from_vars[1]
    col <- if (remove) col else sprintf("%s_ci", col)
  }
  col <- rlang::ensym(col)
  out <- x
  if (remove) {
    out <- out[setdiff(names(out), from_vars)]
  }
  first_pos <- which(names(x) %in% from_vars)[1]
  last_pos  <- which(names(x) %in% from_vars)[3]

  

  if (percent) {
    new_col <- fmt_pci_df(x, e = from_vars[1], l = from_vars[2], u = from_vars[3], digits = digits)
  } else {
    new_col <- fmt_ci_df(x, e = from_vars[1], l = from_vars[2], u = from_vars[3], digits = digits, percent = rate)
  }
  after <- if (remove) first_pos - 1L else last_pos
  out <- tibble::add_column(out, !! col := new_col, .after = after)

  out


}
