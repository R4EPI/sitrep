#' Produces counts with respective proportions from specified variables in a dataframe.
#' Option to add row and column totals
#' @param df A dataframe (e.g. your linelist)
#' @param counter A name of the variable (in quotation marks) that you would like to have as rows.
#' @param grouper A name of the variable (in quotation marks) that you would like to have as columns.
#' @param multiplier What you would like to have your proportions as (default is per 100).
#' @param digits The number of decimal places you would like in your proportions (default is 1).
#' @param coltotals Add column totals on the end
#' @param rowtotals Add row totals (only sums counts)
#' @importFrom dplyr group_by ungroup bind_rows summarise_all funs count mutate mutate_at
#' @importFrom tidyr gather unite spread
#' @importFrom rlang sym "!!" ".data" ":="
#' @export

descriptive <- function(df, counter, grouper = NA, multiplier = 100, digits = 1,
                        coltotals = FALSE, rowtotals = FALSE) {

  # Using rlang::sym() allows us to use quoted arguments
  # If we wanted to go full NSE, we would use rlang::enquo() instead
  sym_count <- rlang::sym(counter)
  # if given two variables then group by the "grouper" var
  if (!is.na(grouper)) {
    sym_group <- rlang::sym(grouper)
    # produce count table with props column-wise (seperate for each "grouper" level)
    count_data <- dplyr::group_by(df, !!sym_group)
    count_data <- dplyr::count(count_data, !!sym_count)
    count_data <- dplyr::mutate(count_data,
                                prop = round(.data$n / sum(.data$n) * multiplier,
                                             digits = digits))

    # change to wide format, to have "grouper" var levels as columns
    count_data <- tidyr::gather(count_data, key = "variable", value = "value", c(.data$n, .data$prop))
    count_data <- tidyr::unite(count_data, "temp", !!sym_group, .data$variable, sep = "_")
    count_data <- tidyr::spread(count_data, .data$temp, .data$value)
  } else {
    # get counts and props for just a single variable
    count_data <- dplyr::count(df, !!sym_count)
    count_data <- dplyr::mutate(count_data,
                                prop = round(.data$n / sum(.data$n) * multiplier,
                                             digits = digits))
  }

  # if there are NA counts, then change these to zero (except) in first col (which contains )
  count_data <- mutate_at(count_data, 2:ncol(count_data), funs(replace(., is.na(.), 0)))


  if (coltotals == TRUE) {
    count_data <- dplyr::ungroup(count_data)
    # change first column (with var levels) in to a character (for rbinding)
    count_data <- dplyr::mutate(count_data, !!sym_count := as.character(!!sym_count))
    # summarise all columns that are numeric, make first col "Total", bind as a row
    csummaries <- summarise_all(count_data, funs(if (is.numeric(.)) sum(.) else "Total"))
    count_data <- dplyr::bind_rows(count_data, csummaries)
  }

  if (rowtotals == TRUE) {
    count_data <-
      # add columns which have "_n" in the name
      mutate(count_data,
             Total = rowSums(count_data[, grep("(_n$|^n$)", colnames(count_data))], na.rm = TRUE))
  }
  count_data
}
