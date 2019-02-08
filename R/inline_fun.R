#' Helper to format confidence interval for text
#' 
#' This function is mainly used for placing in the text fields of Rmarkdown
#' reports. You can use it by writing it in something like this:
#' \preformatted{The CFR for Bamako is `r fmt_pci(case_fatality_rate(10, 50))`}
#' which will render like this: "The CFR for Bamako is 20.00\% (CI 11.24--33.04)"
#'
#' @param x a data frame
#' @param e the column of the estimate (defaults to the third column). Otherwise, a number
#' @param l the column of the lower bound (defaults to the fourth column). Otherwise, a number
#' @param u the column of the upper bound (defaults to the fifth column), otherwise, a number
#' @param digits the number of digits to show
#' @return a text string in the format of "e\% (CI l--u)"
#' @rdname fmt_ci
#' @export
#' @examples
#'
#' cfr <- case_fatality_rate(10, 50)
#' fmt_ci_df(cfr)
#' fmt_ci_df(cfr, d = 1)
#' # If the data starts at a different column, specify a different number
#' fmt_ci_df(cfr[-1], 2, d = 1)
#'
#' # It's also possible to provide numbers directly
#' fmt_ci(pi, pi - runif(1), pi + runif(1))
fmt_ci <- function(e = numeric(), l = numeric(), u = numeric(), digits = 2) {
  stopifnot(is.numeric(e), is.numeric(l), is.numeric(u), is.numeric(digits))
  msg <- "%.2f%% (CI %.2f--%.2f)"
  msg <- gsub("2", digits, msg)
  sprintf(msg, e, l, u)
} 

#' @export
#' @rdname fmt_ci
fmt_pci <- function(e = numeric(), l = numeric(), u = numeric(), digits = 2) {
  fmt_ci(e = e * 100, l = l * 100, u = u * 100, digits = digits)
}

#' @export
#' @rdname fmt_ci
fmt_pci_df <- function(x, e = 3, l = e + 1, u = e + 2, digits = 2) {
  fmt_pci(x[[e]], x[[l]], x[[u]], digits = digits)
}

#' @export
#' @rdname fmt_ci
fmt_ci_df <- function(x, e = 3, l = e + 1, u = e + 2, digits = 2) {
  fmt_ci(x[[e]], x[[l]], x[[u]], digits = digits)
}

#' @export
#' @rdname fmt_ci
merge_ci_df <- function(x, e = 3, l = e + 1, u = e + 2, digits = 2) {
  cis <- fmt_ci_df(x, e, l, u, digits)
  x[c(l, u)] <- NULL
  x$ci <- gsub("^.+?\\(CI ", "(", cis)
  x
}
