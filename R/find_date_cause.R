#' Find the first date beyond a cutoff in several columns
#'
#' This function will find the first date in an orderd series of columns that
#' is either before or after a cutoff date, inclusive.
#'
#' @param x a data frame
#' @param ... an ordered series of date columns (i.e. the most important date
#'   to be considered first).
#' @param datecol the name of the new column to contain the dates
#' @param datereason the name of the column to contain the name of the column
#'   from which the date came.
#' @param period_start,period_end for the find_ functions, this should be the
#'   name of a column in `x` that contains the start/end of the recall period.
#'   For `constrain_dates`, this should be a vector of dates.
#' @param na_fill one of either "before" or "after" indicating that the new
#'   column should only contain dates before or after the cutoff date.
#' @export
#' 
#' @examples
#' d <- data.frame(
#'   s1 = c(as.Date("2013-01-01") + 0:10, as.Date(c("2012-01-01", "2014-01-01"))),
#'   s2 = c(as.Date("2013-02-01") + 0:10, as.Date(c("2012-01-01", "2014-01-01"))),
#'   s3 = c(as.Date("2013-01-10") - 0:10, as.Date(c("2012-01-01", "2014-01-01"))),
#'   ps = as.Date("2012-12-31"),
#'   pe = as.Date("2013-01-09")
#' )
#' print(dd <- find_date_cause(d, s1, s2, s3, period_start = ps, period_end = pe))
#' print(bb <- find_date_cause(d, s1, s2, s3, period_start = ps, period_end = pe,
#'                             na_fill = "end", 
#'                             datecol = "enddate",
#'                             datereason = "endcause"))
#' find_date_cause(d, s3, s2, s1, period_start = ps, period_end = pe)
#'
#' # works
#' assert_positive_timespan(dd, start_date, pe)
#'
#' # returns a warning because the last date isn't later than the start_date
#' assert_positive_timespan(dd, start_date, s2)
#' 
#'
#' with(d, constrain_dates(s1, ps, pe))
#' with(d, constrain_dates(s2, ps, pe))
#' with(d, constrain_dates(s3, ps, pe))
#'
find_date_cause <- function(x,
                            ...,
                            period_start = NULL,
                            period_end = NULL,
                            datecol = "start_date",
                            datereason = "start_date_reason",
                            na_fill = "start") {

  na_fill      <- match.arg(tolower(na_fill), c("start", "end"))
  .dots        <- tidyselect::vars_select(colnames(x), ...)
  period_start <- rlang::enquo(period_start)
  period_end   <- rlang::enquo(period_end)
  period_start <- tidyselect::vars_select(colnames(x), !! period_start)
  period_end   <- tidyselect::vars_select(colnames(x), !! period_end)


  are_dates <- vapply(x[c(.dots, period_start, period_end)], inherits, logical(1), "Date")

  if (!all(are_dates)) {
    stop("All columns in ..., period_start, and period_end must be dates")
  }

  y <- x[c(.dots, period_start, period_end)]

  # removing dates that don't conform
  y <- dplyr::mutate_at(.tbl         = y, 
                        .vars        = .dots, 
                        .funs        = constrain_dates, 
                        period_start = y[[period_start]],
                        period_end   = y[[period_end]],
                        boundary     = na_fill)

  y <- choose_first_good_date(y[.dots])

  tibble::add_column(!! rlang::sym(datecol)    := y[[1]],
                     !! rlang::sym(datereason) := y[[2]],
                     .data   = x,
                     .before = .dots[[1]])
}


#' @rdname find_date_cause
#' @export
find_start_date <- function(x, ...,
                            period_start = NULL,
                            period_end = NULL,
                            datecol = "start_date",
                            datereason = "start_date_reason"
                            ) {

  find_date_cause(x, ..., 
                  period_start = !! rlang::enquo(period_start),
                  period_end   = !! rlang::enquo(period_end),
                  datecol      = datecol, 
                  datereason   = datereason, 
                  na_fill      = "start")

}

#' @rdname find_date_cause
#' @export
find_end_date <- function(x, ...,
                          period_start = NULL,
                          period_end = NULL,
                          datecol = "end_date",
                          datereason = "end_date_reason"
                          ) {

  find_date_cause(x, ..., 
                  period_start = !! rlang::enquo(period_start),
                  period_end   = !! rlang::enquo(period_end),
                  datecol      = datecol, 
                  datereason   = datereason, 
                  na_fill      = "end")

}

#' @rdname find_date_cause
#' @param i a vector of dates
#' @param boundary one of "both", "start", or "end". Dates outside of the 
#'   boundary will be set to NA. 
#' @export
constrain_dates <- function(i, period_start, period_end, boundary = "both") {

  boundary  <- match.arg(boundary, c("both", "start", "end"))
  nna       <- !is.na(i)
  too_early <- nna & i < period_start
  too_late  <- nna & i > period_end

  if (boundary != "both") {
    at_the_beginning <- boundary == "start"
    trim    <- if (at_the_beginning) too_early    else too_late
    repl    <- if (at_the_beginning) period_start else period_end
    i[trim] <- repl[trim]
  }

  i[nna & (i < period_start | i > period_end)] <- NA

  i

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
    if (!is.finite(nona)) {
      next
    }
    res$the_date[i] <- tmp[[nona]]
    res$the_col[i]  <- names(date_a_frame)[nona]
  }
  res
}

#' @param date_start,date_end column name of a date vector
#' @rdname find_date_cause
#' @export
assert_positive_timespan <- function(x, date_start, date_end) {

  ds <- tidyselect::vars_select(colnames(x), !! rlang::enquo(date_start))
  de <- tidyselect::vars_select(colnames(x), !! rlang::enquo(date_end  ))
  res <- x[[de]] - x[[ds]]
  all_right <- all(res >= 0, na.rm = TRUE)  
  if (!all_right) {
    y <- x[res < 0, , drop = FALSE]    
    warning(sprintf("%d rows had negative timespans", nrow(y)), immediate. = TRUE)
    return(y)
  }
  return(invisible(NULL))
}
