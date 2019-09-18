#' Tabulate counts and proportions
#'
#' @param x a [data.frame()] or [tbl_svy][srvyr::as_survey_design] object
#' 
#' @param ... categorical variables to tabulate
#'
#' @param strata a stratifier to split the data
#'
#' @param keep a character vector specifying which values to retain in the
#'   tabulation. Defaults to `TRUE`, which keeps all the values.
#'
#' @param drop a character vector specifying which values to drop in the
#'   tabulation. Defaults to `NULL`, which keeps all values. 
#'
#' @param na.rm When `TRUE` (default), missing (NA) values present in `var`
#'   will be removed from the data set with a warning, causing a change in
#'   denominator for the tabulations. Setting this to `FALSE` creates an
#'   explicit missing value called "(Missing)".
#'
#' @param prop_total if `TRUE` and `strata` is not `NULL`, then the totals of the
#'   rows will be reported as proportions of the total data set, otherwise, they
#'   will be proportions within the stratum (default).
#'
#' @param row_total create a new column with the total counts for each row of 
#'   stratified data.
#'
#' @param col_total create a new row with the total counts for each column of
#'   stratified data.
#'
#' @param wide if `TRUE` (default) and strata is defined, then the results are
#'   presented in a wide table with each stratification counts and estimates in
#'   separate columns. If `FALSE`, then the data will be presented in a long
#'   format where the counts and estimates are presented in single columns. This
#'   has no effect if strata is not defined.
#'
#' @param transpose if `wide = TRUE`, then this will transpose the columns to 
#'   the rows, which is useful when you stratify by age group. Default is
#'   `NULL`, which will not transpose anything. You have three options for
#'   transpose:
#'    - `transpose = "variable"`: uses the variable column, (dropping values if strata exists).
#'       Use this if you know that your values are all identical or at least
#'       identifiable by the variable name.
#'    - `transpose = "value"`   : uses the value column, (dropping variables if strata exists).
#'       Use this if your values are important and the variable names are
#'       generic placeholders.
#'    - `transpose = "both"`    : combines the variable and value columns.
#'       Use this if both the variables and values are important.
#'
#' @param pretty (survey only) if `TRUE`, default, the proportion and CI are merged
#'
#' @param digits (survey only) if `pretty = FALSE`, this indicates the number
#'   of digits used for proportion and CI
#'
#' @param method (survey only) a method from [survey::svyciprop()] to calculate
#'   the confidence interval. Defaults to "logit".
#'
#' @param deff a logical indicating if the design effect should be reported.
#'   Defaults to `TRUE`.
#'
#' @return a [tibble::tibble()] with a column for variables, a column for values,
#'   and counts and proportions. If `strata` is not `NULL` and `wide = TRUE`, 
#'   then there will be separate columns for each strata for the counts and 
#'   proportions. Survey data will report confidence intervals.
#'
#' @rdname tab_functions
#' @export
#'
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
#' tab_linelist(linelist_clean, sex)
#' 
#' # describe prenancy statistics, but remove missing data from the tally
#' tab_linelist(linelist_clean, trimester, na.rm = TRUE)
#' 
#' # describe by symptom
#'
#' tab_linelist(linelist_clean, 
#'              cough, nasal_discharge, severe_oral_lesions,
#'              transpose = "value")
#' # describe prenancy statistics, stratifying by vitamin A perscription
#' tab_linelist(linelist_clean, trimester, sex, strata = prescribed_vitamin_a, 
#'              na.rm = TRUE, row_total = TRUE)
#'
#' }) }
#' 
#' library(srvyr)
#' library(survey)
#' have_survey_packages <- require("survey") && require("srvyr")
#' if (have_survey_packages) { withAutoprint({
#' data(api)
#'
#' # stratified sample
#' surv <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' s <- surv %>%
#'   tab_survey(awards, strata = stype, col_total = TRUE, row_total = TRUE, deff = TRUE)
#' s
#'
#' # making things pretty
#' s %>%
#'   # wrap all "n" variables in braces (note space before n).
#'   augment_redundant(" (n)" = " n") %>%
#'   # relabel all columns containing "prop" to "% (95% CI)"
#'   rename_redundant("% (95% CI)"    = ci,
#'                    "Design Effect" = deff)
#'
#' # long data
#' surv %>%
#'   tab_survey(awards, strata = stype, wide = FALSE)
#'
#' # tabulate binary variables
#' surv %>%
#'   tab_survey(yr.rnd, sch.wide, awards, keep = "Yes")
#'
#' # stratify the binary variables
#' surv %>%
#'   tab_survey(yr.rnd, sch.wide, awards, 
#'              strata    = stype,
#'              keep      = "Yes")
#'
#' # invert the tabulation
#' surv %>%
#'   tab_survey(yr.rnd, sch.wide, awards, 
#'              strata    = stype,
#'              drop      = "Yes",
#'              deff      = TRUE,
#'              row_total = TRUE)
#' }) }
tab_linelist <- function(x,
                         ...,
                         strata     = NULL,
                         keep       = TRUE,
                         drop       = NULL,
                         na.rm      = TRUE,
                         prop_total = FALSE,
                         row_total  = FALSE,
                         col_total  = FALSE,
                         wide       = TRUE,
                         transpose  = NULL,
                         digits     = 1,
                         pretty     = TRUE) {

  tab_general(x,
              ...,
              strata     = !!rlang::enquo(strata),
              keep       = keep,
              drop       = drop,
              na.rm      = na.rm,
              prop_total = prop_total,
              row_total  = row_total,
              col_total  = col_total,
              wide       = wide,
              transpose  = transpose,
              digits     = digits,
              pretty     = pretty
             )

}

#' @rdname tab_functions
#' @export
tab_survey <- function(x,
                       ...,
                       strata     = NULL,
                       keep       = TRUE,
                       drop       = NULL,
                       na.rm      = TRUE,
                       prop_total = FALSE,
                       row_total  = FALSE,
                       col_total  = FALSE,
                       wide       = TRUE,
                       transpose  = NULL,
                       digits     = 1,
                       method     = "logit",
                       deff       = FALSE,
                       pretty     = TRUE) {


  tab_general(x,
              ...,
              strata     = !!rlang::enquo(strata),
              keep       = keep,
              drop       = drop,
              na.rm      = na.rm,
              prop_total = prop_total,
              row_total  = row_total,
              col_total  = col_total,
              wide       = wide,
              transpose  = transpose,
              digits     = digits,
              method     = method,
              deff       = deff,
              pretty     = pretty
             )
}

tab_general <-  function(x,
                         ...,
                         strata     = NULL,
                         keep       = TRUE,
                         drop       = NULL,
                         na.rm      = TRUE,
                         prop_total = FALSE,
                         row_total  = FALSE,
                         col_total  = FALSE,
                         wide       = TRUE,
                         transpose  = NULL,
                         digits     = 1,
                         method     = "logit",
                         deff       = FALSE,
                         pretty     = TRUE){

  is_survey <- inherits(x, "tbl_svy")
  stopifnot(is_survey || is.data.frame(x))

  # We try to match the user-supplied variables to the colnames. If the user
  # supplied a tidyselect verb (e.g. `starts_with("CHOICE")`, then it should
  # filter properly.
  vars <- tidyselect::vars_select(colnames(x), ..., .strict = FALSE)

  # However, if the user supplies a vector of column names and some do not exist,
  # tidyselect unhelpfully returns nothing 
  #
  # ಠ_ಠ
  #
  # so, to give our users something, we wrap this character vector in one_of(),
  # which will warn about which columns were not recognised.
  if (length(vars) == 0) {
    vars <- tidyselect::vars_select(colnames(x), tidyselect::one_of(...), .strict = FALSE)
  }

  stra    <- rlang::enquo(strata)
  flip_it <- wide && !is.null(transpose)

  if (flip_it) {
    transpose <- match.arg(tolower(transpose), c("variable", "value", "both"))
  }

  # Create list for results to go into that will eventually be bound together
  res        <- vector(mode = "list", length = length(vars))
  names(res) <- vars


  # loop over each name in the list and tabulate the survey for that variable
  for (i in names(res)) {
    i <- rlang::ensym(i)
    if (is_survey) {
      res[[i]] <- tabulate_survey(x,
                                  var       = !!i,
                                  strata    = !!stra,
                                  proptotal = prop_total,
                                  coltotals = col_total,
                                  rowtotals = row_total,
                                  pretty    = pretty,
                                  digits    = digits,
                                  method    = method,
                                  wide      = wide,
                                  na.rm     = na.rm,
                                  deff      = deff)
    } else {
      res[[i]] <- descriptive(x,
                              counter          = !!i,
                              grouper          = !!stra,
                              proptotal        = prop_total,
                              coltotals        = col_total,
                              rowtotals        = row_total,
                              digits           = digits,
                              explicit_missing = !na.rm
                             )

    }

    # The ouptut columns will have the value as whatever i was, so we should
    # rename this to "value" to make it consistent
    names(res[[i]])[names(res[[i]]) == i] <- "value"
    res[[i]][["value"]]                   <- as.character(res[[i]][["value"]])
  }
  # Combine the results into one table
  suppressWarnings(res <- dplyr::bind_rows(res, .id = "variable"))

  # return the results with only the selected values
  if (!isTRUE(keep) && !is.null(drop)) {
    stop('you can only choose to keep values or drop values. Specifying both is not allowed')
  }

  strata_exists <- tidyselect::vars_select(colnames(x), !! stra)
  strata_exists <- length(strata_exists) > 0

  if (!isTRUE(keep)) {
    
    res <- res[res$value %in% keep, , drop = FALSE]
    
  } else if (!is.null(drop)) {
    
    res <- res[!res$value %in% drop, , drop = FALSE]
    
  } else if (flip_it && !strata_exists && transpose != "both") {

    flip_it <- FALSE
    # This is the situation where the user doesn't have a stratafying variable,
    # but they want to transpose either the variable or value.
    the_column <- if (transpose == "variable") "value" else "variable"
    res[[the_column]] <- forcats::fct_inorder(res[[the_column]])
    res[[transpose]]  <- forcats::fct_inorder(res[[transpose]])
    res <- widen_tabulation(res, 
                            !!rlang::sym(the_column), 
                            !!rlang::sym(transpose),
                            pretty = if (is_survey) pretty else FALSE,
                            digits = digits)

    if (col_total && the_column == "value") {
      # prevent Total from appearing as one of the middle rows
      res[["value"]] <- forcats::fct_relevel(res[["value"]], "Total", after = Inf)
      res <- res[order(res[["value"]]), ] 
    }
    if (col_total && the_column == "variable") {
      # prevent Total from appearing as one of the middle columns
      good_order <- c(grep("Total", names(res), invert = TRUE), 
                      grep("Total", names(res)))
      res <- res[good_order]
    }

  } else {

    if (flip_it) { 
      warning("Cannot transpose data that hasn't been filtered with keep or drop", call. = FALSE)
    }
    flip_it <- FALSE
    
  }
  # If the user wants to transpose the data, then we need to do this for each
  # level of data available into separate tables, combine the columns, and then
  # rearrange them so that they are grouped by variable/value
  if (flip_it) {
    res <- flipper(if (is_survey) x$variables else x, 
                   res, transpose, pretty = pretty, stra = stra)
  }
  res

}
