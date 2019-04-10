#' Find the first date beyond a cutoff in several columns
#'
#' This function will find the first date in an orderd series of columns that 
#' is either before or after a cutoff date, inclusive. 
#' 
#' @param x a data frame
#' @param ... an ordered series of date columns.
#' @param datecol the name of the new column to contain the dates
#' @param datereason the name of the column to contain the name of the column
#'   from which the date came.
#' @param cutoff the cutoff date
#' @param period one of either "before" or "after" indicating that the new 
#'   column should only contain dates before or after the cutoff date. 
#' @export
find_date_cause <- function(x, 
                            ...,
                            datecol = "start_date", 
                            datereason = "start_date_reason", 
                            cutoff = NA, 
                            period = "after") {

  stopifnot(inherits(cutoff, "Date"))
  period <- match.arg(tolower(period), c("before", "after"))
  .dots  <- tidyselect::vars_select(colnames(x), ...)

  are_dates <- vapply(x[.dots], inherits, logical(1), "Date")

  if (!all(are_dates)) {
    stop("All columns in ... must be dates")
  }
  y <- x[.dots]
  
  if (period == "after") {
    the_judge <- function(i, cutoff) dplyr::if_else(i >= cutoff, i, as.Date(NA))
  } else {
    the_judge <- function(i, cutoff) dplyr::if_else(i <= cutoff, i, as.Date(NA))
  }

  # removing dates that don't conform
  y <- dplyr::mutate_at(.tbl = x, .vars = .dots, .funs = ~the_judge(., cutoff))
  y <- choose_first_good_date(y[.dots])
  tibble::add_column(!! rlang::sym(datecol) := y[[1]],
                     !! rlang::sym(datereason) := y[[2]],
                     .data = x, 
                     .before = .dots[[1]])
}

#' @rdname find_date_cause
#' @export
find_start_date <- function(x, ..., 
                            datecol = "start_date", 
                            datereason = "start_date_reason",
                            cutoff = NA) {

  find_date_cause(x, ..., datecol = datecol, datereason = datereason, cutoff = cutoff, period = "after")

}

#' @rdname find_date_cause
#' @export
find_end_date <- function(x, ..., 
                          datecol = "end_date", 
                          datereason = "end_date_reason",
                          cutoff = NA) {

  find_date_cause(x, ..., datecol = datecol, datereason = datereason, cutoff = cutoff, period = "before")

}

#' Choose the first non-missing date from a data frame of dates
#'
#' @param date_a_frame a data frame where each column contains a different
#'   parsing of the same date vector
#' @keywords internal
#' @noRd
#' @note: This function was written and modified by Zhian N. Kamvar and comes 
#'   from the linelist package, 
choose_first_good_date <- function(date_a_frame) {
  n   <- nrow(date_a_frame)
  res <- data.frame(the_date = rep(as.Date(NA), length = n),
                    the_col  = character(n),
                    stringsAsFactors = FALSE
                   )
  for (i in seq_len(n)) {
    tmp <- date_a_frame[i, ]
    suppressWarnings(nona <- min(which(!is.na(tmp))))
    if (!is.finite(nona)) next
    res$the_date[i] <- tmp[[nona]]
    res$the_col[i]  <- names(date_a_frame)[nona]
  }
  res
}
