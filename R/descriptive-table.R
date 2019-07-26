#' Produces counts with respective proportions from specified variables in a dataframe.
#'
#' Option to add row and column totals
#' 
#' @param df A dataframe (e.g. your linelist)
#'
#' @param counter A name of the variable (in quotation marks) that you would
#'   like to have as rows.
#'
#' @param grouper A name of the variable (in quotation marks) that you would
#'   like to have as columns.
#'
#' @param multiplier What you would like to have your proportions as (default
#'   is per 100).
#'
#' @param digits The number of decimal places you would like in your
#'   proportions (default is 1).
#'
#' @param proptotal A TRUE/FALSE variable specifying whether you would
#'   proportions to be of total cases.The default is FALSE and returns
#'   proportions for each column.
#'
#' @param coltotals Add column totals on the end
#'
#' @param rowtotals Add row totals (only sums counts)
#'
#' @param single_row if `TRUE` and `grouper = NA`, then the output is flattened
#'   to a single row so that variables can be concatenated into a data frame.
#'   Defaults to `FALSE`.
#'
#' @param explicit_missing if `TRUE`, missing values will be marked as
#' `Missing` and tabulated. Defaults to `FALSE`, where missing values are
#' excluded from the computation
#'
#' @details The `descriptive()` function returns a single table with counts and
#'   proportions of a categorical variable (`counter`). Adding a grouper adds
#'   more columns, stratifying "n" and "prop", the option `coltotals = TRUE`
#'   adds one row and `rowtotals = TRUE` (useful if a grouper is present) adds
#'   one column. 
#'
#'   The `multi_descriptive()` function allows you to combine several counter
#'   variables into a single table where each row represents a variable and the
#'   columns represent counts and proportions of the values within those
#'   variables. This function assumes that all of the variables have the same
#'   values (e.g. Yes/No values) and atttempts no correction.
#'
#' @importFrom dplyr group_by ungroup bind_rows summarise_all funs count mutate mutate_at
#' @importFrom tidyr complete gather unite spread
#' @importFrom rlang sym "!!" ".data" ":="
#' @importFrom stats setNames
#' @export
#' @examples 
#' have_packages <- require("dplyr") && require("linelist")
#' if (have_packages) { withAutoprint({
#' 
#' # Simulating linelist data
#'
#' linelist     <- gen_data("Measles")
#' measles_dict <- msf_dict("Measles", compact = FALSE) %>%
#'   select(option_code, option_name, everything())
#'
#' # Cleaning linelist data
#' linelist_clean <- clean_variable_spelling(
#'   x             = linelist,
#'   wordlists     = filter(measles_dict, !is.na(option_code)),
#'   spelling_vars = "data_element_shortname",
#'   sort_by       = "option_order_in_set"
#' )
#' 
#' # get a descriptive table by sex
#' descriptive(linelist_clean, "sex")
#' 
#' # describe prenancy statistics, but remove missing data from the tally
#' descriptive(linelist_clean, "trimester", explicit_missing = FALSE)
#' 
#' # describe prenancy statistics, stratifying by vitamin A perscription
#' descriptive(linelist_clean, "trimester", "prescribed_vitamin_a", explicit_missing = FALSE)
#' 
#'
#' }) }
#' 
descriptive <- function(df, counter, grouper = NULL, multiplier = 100, digits = 1,
                        proptotal = FALSE, coltotals = FALSE, rowtotals = FALSE,
                        single_row = FALSE, explicit_missing = TRUE) {


  # translate the variable names to character
  counter   <- tidyselect::vars_select(colnames(df), !!enquo(counter))
  grouper   <- tidyselect::vars_select(colnames(df), !!enquo(grouper))
  sym_count <- rlang::sym(counter)

  # Check if counter is an integer and force factor ----------------------------

  if (is.numeric(df[[counter]])) {
    warning(glue::glue("converting `{counter}` to a factor"))
    df[[counter]] <- fac_from_num(df[[counter]])
  }
  
  if (is.logical(df[[counter]])) {
    df[[counter]] <- factor(df[[counter]], levels = c("TRUE", "FALSE"))
  }
  # Filter missing data --------------------------------------------------------

  if (explicit_missing) {
    df[[counter]] <- forcats::fct_explicit_na(df[[counter]], "Missing")
  } else {
    nas <- is.na(df[[counter]])
    warning(sprintf("Removing %d missing values", sum(nas)))
    df  <- df[!nas, , drop = FALSE]
  }

  # Apply grouping -------------------------------------------------------------

  if (length(grouper) == 1) {
    # This grouper var will always have explicit missing.
    sym_group     <- rlang::sym(grouper)
    df[[grouper]] <- forcats::fct_explicit_na(df[[grouper]], "Missing")
    count_data    <- dplyr::group_by(df, !!sym_group, .drop = FALSE)
  } else {
    count_data <- df
  }

  # Get counts and proportions -------------------------------------------------

  count_data <- dplyr::count(count_data, !! sym_count, .drop = FALSE)

  if (proptotal) {
    count_data <- dplyr::mutate(count_data,
                                prop = .data$n / nrow(df) * multiplier,
                                )
  } else {
    count_data <- dplyr::mutate(count_data,
                                prop = .data$n / sum(.data$n) * multiplier,
                                )
  }

  # Widen grouping data --------------------------------------------------------

  if (length(grouper) == 1) {
    # change to wide format, to have "grouper" var levels as columns
    count_data <- tidyr::gather(count_data, 
                                key = "variable", 
                                value = "value", 
                                c(.data$n, .data$prop))
    count_data <- tidyr::unite(count_data, 
                               col = "temp", 
                               !! sym_group, .data$variable, 
                               sep = "_")

    if (is.factor(df[[grouper]])) {
      lvls            <- rep(levels(df[[grouper]]), each = 2)
      lvls            <- paste0(lvls, c("_n", "_prop"))
      count_data$temp <- factor(count_data$temp, levels = lvls)
    }
    count_data <- tidyr::spread(count_data, .data$temp, .data$value)
  }

  # fill in the counting data that didn't make it
  count_data <- dplyr::mutate_if(count_data, is.numeric, tidyr::replace_na, 0)

  # Calculate totals for each column -------------------------------------------

  if (coltotals == TRUE) {
    count_data <- dplyr::ungroup(count_data)
    # change first column (with var levels) in to a character (for rbinding)
    count_data <- dplyr::mutate(count_data, !!sym_count := as.character(!!sym_count))
    # summarise all columns that are numeric, make first col "Total", bind as a row
    csummaries <- dplyr::summarise_if(count_data, is.numeric, sum, na.rm = TRUE)
    count_data <- dplyr::bind_rows(count_data, csummaries)
    count_data[nrow(count_data), 1] <- "Total"
  }

  # Calculate totals for all rows ----------------------------------------------

  if (rowtotals == TRUE) {
    # add columns which have "_n" in the name
    count_data <- mutate(count_data,
             Total = rowSums(count_data[, grep("(_n$|^n$)", colnames(count_data))], na.rm = TRUE))
  }

  if (single_row){
    count_data <- tidyr::gather(count_data, "variable", "value", -1)
    count_data <- tidyr::unite(count_data, "tempvar", 1, .data$variable)
    count_data <- tidyr::spread(count_data, "tempvar", "value")
  }
  count_data

}


#' @rdname descriptive
#' @param ... columns to pass to descriptive
#' @param .id the name of the column identifying the aggregates
#' @export
multi_descriptive <- function(df, ..., multiplier = 100, digits = 1, proptotal = FALSE, coltotals = TRUE, .id = "symptom", explicit_missing = TRUE) { 
  
  the_vars <- tidyselect::vars_select(colnames(df), ...)
  res <- lapply(the_vars, function(i) {
    suppressWarnings({
    descriptive(df, i, multiplier = multiplier, digits = digits, 
                proptotal = proptotal, coltotals = coltotals, single_row = TRUE,
                explicit_missing = explicit_missing)
    })
  })
  bind_rows(res, .id = .id)
}
