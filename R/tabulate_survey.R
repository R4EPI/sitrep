#' Tabulate survey design objects by a categorical and another stratifying variable
#'
#' @param x a survey design object
#' @param var the bare name of a categorical variable
#' @param strata a variable to stratify the results by
#' @param pretty if `TRUE`, default, the proportion and CI are merged
#' @param wide if `TRUE` (default) and strata is defined, then the results are
#'   presented in a wide table with each stratification counts and estimates in
#'   separate columns. If `FALSE`, then the data will be presented in a long
#'   format where the counts and estimates are presented in single columns. This
#'   has no effect if strata is not defined.
#' @param digits if `pretty = FALSE`, this indicates the number of digits used
#'   for proportion and CI
#' @param method a method from [survey::svyciprop()] to calculate the confidence
#'   interval. Defaults to "logit"
#' @param deff a logical indicating if the design effect should be reported.
#'   Defaults to "TRUE"
#' @param coltotals if `TRUE` a new row with totals for each "n" column is
#'   created.
#' @param rowtotals if `TRUE` and `strata` is defined, then an extra "Total"
#'   column will be added tabulating all of the rows across strata.
#' @return a long or wide tibble with tabulations n, ci, and deff
#' @export
#' @seealso [rename_redundant()], [augment_redundant()]
#' @importFrom srvyr survey_total survey_mean
#' @note The proportions presented here represent the proportions of the total
#'   population, not for the stratified samples
#' @examples
#' library(srvyr)
#' library(survey)
#' data(api)
#'
#' # stratified sample
#' s <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw) %>%
#'   tabulate_survey(stype, awards, coltotals = TRUE, rowtotals = TRUE, deff = TRUE)
#' s
#'
#' # making things pretty
#' s %>%
#'   # wrap all "n" variables in braces (note space before n).
#'   augment_redundant(" n" = " (n)") %>%
#'   # relabel all columns containing "prop" to "% (95% CI)"
#'   rename_redundant("ci"   = "% (95% CI)",
#'                    "deff" = "Design Effect")
#'
#' # long data
#' apistrat %>%
#'   as_survey_design(strata = stype, weights = pw) %>%
#'   tabulate_survey(stype, awards, wide = FALSE)
#'
#' apistrat %>%
#'   as_survey_design(strata = stype, weights = pw) %>%
#'   tabulate_binary_survey(stype, awards, keep = c("Yes", "E"))
#'
#' apistrat %>%
#'   as_survey_design(strata = stype, weights = pw) %>%
#'   tabulate_binary_survey(stype, awards, keep = c("Yes", "E"), invert = TRUE)
tabulate_survey <- function(x, var, strata = NULL, pretty = TRUE, wide = TRUE, digits = 1, method = "logit", deff = FALSE, rowtotals = FALSE, coltotals = FALSE) {
  stopifnot(inherits(x, "tbl_svy"))

  cod <- rlang::enquo(var)
  st  <- rlang::enquo(strata)
  null_strata <- is.null(rlang::get_expr(st))

  x <- srvyr::select(x, !! cod, !!st)

  # here we are creating a dummy variable that is either the var or the
  # combination of var and strata so that we can get the right proportions from
  # the survey package in a loop below
  if (null_strata) {
    x <- srvyr::group_by(x, !! cod)
    x <- srvyr::mutate(x, dummy = !! cod)
  } else {
    # if there is a strata, create a unique, parseable dummy var by inserting
    # the timestamp in between the vars
    tim <- as.character(Sys.time())
    # This is to handle situations where you have a binary variable that is a
    # character vector and only has one of the responses recorded (factors 
    # should be fine here)
    x <- srvyr::mutate(x, !! cod := if (is.factor(!! cod)) !! cod else factor(!! cod, unique(!! cod)))
    x <- srvyr::group_by(x, !!st, !! cod)
    x <- srvyr::mutate(x, dummy = sprintf("%s %s %s", !! st, tim, !! cod))
  }

  # Calculating the survey total will also give us zero counts
  y <- srvyr::summarise(x,
                        n = survey_total(vartype = "se", na.rm = TRUE),
                        mean = survey_mean(na.rm = TRUE, deff = deff))

  # We can then set up the proportion calculations. Because of issues with using
  # srvyr::survey_mean() on several variables, we have to roll our own.
  y$proportion       <- NA_real_
  y$proportion_lower <- NA_real_
  y$proportion_upper <- NA_real_
  y$mean <- NULL
  y$mean_se <- NULL
  if (deff) {
    names(y)[names(y) == "mean_deff"] <- "deff"
    y$deff <- round(y$deff, digits)
    y$deff[!is.finite(y$deff)] <- NA
  }


  # Here we pull out the relevant values for the proportions
  # first, filter out any rows with missing values
  tx <- dplyr::filter(srvyr::as_tibble(x), !is.na(!! cod))
  # Then pull out the unique dummy variable names
  p <- as.character(unique(dplyr::pull(tx, !!quote(dummy))))
  p <- p[!is.na(p)]

  # loop over the values and caclucate CI proportion as needed. If they aren't
  # present, then the proportion remains NA.
  for (i in p) {
    # Find the row that i corresponds to. It differs if there are strata or not
    if (null_strata) {
      val <- y[[1]] == i
    } else {
      val <- sprintf("%s %s %s", y[[1]], tim, y[[2]]) == i
    }
    if (y$n[val] > 0) {
      # The peanutbutter and paperclips way of getting a proportion:
      # set a column to contain only the value you desire
      x   <- srvyr::mutate(x, this = i)
      # get the proportion of your target variable that matches the new column
      # et voila!
      tmp <- survey::svyciprop(~I(dummy == this), x, method = method)
      ci  <- attr(tmp, "ci")
      y$proportion[val]       <- tmp[[1]]
      y$proportion_lower[val] <- ci[[1]]
      y$proportion_upper[val] <- ci[[2]]
    }
  }


  y$n <- round(y$n)
  y   <- y[!colnames(y) %in% "n_se"]

  if (coltotals) {
    if (null_strata) {
      tot <- data.frame(n = sum(y$n, na.rm = TRUE))
    } else {
      # group by stratifier
      y <- dplyr::group_by(y, !! st)
      # tally up the Ns
      tot <- dplyr::tally(y, !! rlang::sym("n"))
      # bind to the long data frame
      y <- dplyr::ungroup(y)
    }
    suppressMessages(y <- dplyr::bind_rows(y, tot))
    # replace any NAs in the cause of death with "Total"
    y <- dplyr::mutate(y, !! cod := forcats::fct_explicit_na(!! cod, "Total"))  
    
  }

  if (rowtotals && !null_strata) {
    # group by cause of death
    y <- dplyr::group_by(y, !! cod)
    # tally up the Ns
    tot <- dplyr::tally(y, !! rlang::sym("n"))
    # bind to the long data frame
    y <- dplyr::ungroup(y)
    suppressMessages(y <- dplyr::bind_rows(y, tot))
    # replace any NAs in the stratifier with "Total"
    y <- dplyr::mutate(y, !! st := forcats::fct_explicit_na(!! st, "Total"))
  }

  if (pretty) {
    y <- prettify_tabulation(y, digits, null_strata, !! cod, !! st)
  }

  if (wide && !null_strata) {
    y <- widen_tabulation(y, !! cod, !! st)
  }
  y$"Total deff" <- NULL
  y$"Total ci"   <- NULL

  return(y)
}


#' Make the tabulation pretty by uniting the confindence intervals
#'
#' @param y a data frame
#' @param digits number of digits to round to
#' @param null_strata a logical value specifyingif there is a null strata variable
#' @param cod variable of interest
#' @param st stratifying variable
#' @noRd
prettify_tabulation <- function(y, digits = 1, null_strata, cod, st) {

  y <- unite_ci(y, "ci", dplyr::starts_with("proportion"), percent = TRUE, digits = digits)

  # convert any NA% proportions to just NA
  y$ci <- dplyr::if_else(grepl("NA%", y$ci), NA_character_, y$ci)

  return(y)

}

widen_tabulation <- function(y, cod, st) {

  cod <- rlang::enquo(cod)
  st  <- rlang::enquo(st)

  # Only select the necessary columns. n, deff, and prop are all numeric columns
  # that need to be gathered
  y <- dplyr::select(y, !! cod, !! st, "n",
                     # deff, ci, and prop are all columns that _might_ exist
                     dplyr::matches("prop"), dplyr::starts_with("ci"), dplyr::starts_with("deff"))
  # gather "n", "deff", and "prop" into a single column
  y <- tidyr::gather(y, key = "variable", value = "value", -(1:2))
  # make sure that everything is arranged in the correct order
  y <- dplyr::arrange(y, !! cod, !! st)
  # combine the stratifier and the n/prop signifier
  y <- tidyr::unite(y, "tmp", !! st, "variable", sep = " ")
  # Make sure the factors are in the correct order
  y$tmp <- forcats::fct_inorder(y$tmp)
  # Spread out the combined stratifier and signifier to columns
  y <- tidyr::spread(y, "tmp", "value")

}

#' @export
#' @rdname tabulate_survey
#' @param ... binary variables for tabulation
#' @param keep a vector of binary values to keep
#' @param invert if `TRUE`, the kept values are rejected. Defaults to `FALSE`
#'
tabulate_binary_survey <- function(x, ..., strata = NULL, keep = NULL, invert = FALSE, pretty = TRUE, wide = TRUE, digits = 1, method = "logit", deff = FALSE) {

  stopifnot(inherits(x, "tbl_svy"))
  if (is.null(keep)) {
    stop("Please provide a list of values to keep in the output.")
  }

  vars <- tidyselect::vars_select(colnames(x), ...)
  strt <- rlang::enquo(strata)
  null_strata <- is.null(rlang::get_expr(strt))

  # Create list for results to go into that will eventually be bound together
  res <- vector(mode = "list", length = length(vars))
  names(res) <- vars

  # loop over each name in the list and tabulate the survey for that variable
  for (i in names(res)) {
    i        <- rlang::ensym(i)
    res[[i]] <- tabulate_survey(x,
                                var    = !! i,
                                strata = !! strt,
                                pretty = pretty,
                                digits = digits,
                                method = method,
                                wide   = wide,
                                deff   = deff)

    # The ouptut columns will have the value as whatever i was, so we should
    # rename this to "value" to make it consistent
    names(res[[i]])[if (wide) 1 else 2] <- "value"

  }
  # Combine the results into one table
  suppressWarnings(res <- dplyr::bind_rows(res, .id = "variable"))

  # return the results with only the selected values
  res[if (invert) !res$value %in% keep else res$value %in% keep, ]
}
