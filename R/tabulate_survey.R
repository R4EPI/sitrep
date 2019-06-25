#' Tabulate survey design objects by a categorical and another stratifying variable
#'
#' @param x a survey design object
#'
#' @param var the bare name of a categorical variable
#'
#' @param strata a variable to stratify the results by
#'
#' @param pretty if `TRUE`, default, the proportion and CI are merged
#'
#' @param wide if `TRUE` (default) and strata is defined, then the results are
#'   presented in a wide table with each stratification counts and estimates in
#'   separate columns. If `FALSE`, then the data will be presented in a long
#'   format where the counts and estimates are presented in single columns. This
#'   has no effect if strata is not defined.
#'
#' @param digits if `pretty = FALSE`, this indicates the number of digits used
#'   for proportion and CI
#'
#' @param method a method from [survey::svyciprop()] to calculate the confidence
#'   interval. Defaults to "logit"
#'
#' @param deff a logical indicating if the design effect should be reported.
#'   Defaults to "TRUE"
#'
#' @param proptotal if `TRUE` and `strata` is not `NULL`, then the totals of the
#'   rows will be reported as proportions of the total data set, otherwise, they
#'   will be proportions within the stratum (default).
#'
#' @param coltotals if `TRUE` a new row with totals for each "n" column is
#'   created.
#'
#' @param rowtotals if `TRUE` and `strata` is defined, then an extra "Total"
#'   column will be added tabulating all of the rows across strata.
#'
#' @return a long or wide tibble with tabulations n, ci, and deff
#'
#' @export
#'
#' @seealso [rename_redundant()], [augment_redundant()]
#'
#' @importFrom srvyr survey_total survey_mean
#'
#' @examples
#' library(srvyr)
#' library(survey)
#' data(api)
#'
#' # stratified sample
#' surv <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' s <- surv %>%
#'   tabulate_survey(awards, stype, coltotals = TRUE, rowtotals = TRUE, deff = TRUE)
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
#' surv %>%
#'   tabulate_survey(awards, strata = stype, wide = FALSE)
#'
#' # tabulate binary variables
#' surv %>%
#'   tabulate_binary_survey(yr.rnd, sch.wide, awards, keep = c("Yes"))
#'
#' # stratify the binary variables
#' surv %>%
#'   tabulate_binary_survey(yr.rnd, sch.wide, awards, strata = stype, keep = c("Yes"))
#'
#' # invert the tabulation
#' surv %>%
#'   tabulate_binary_survey(yr.rnd, sch.wide, awards, keep = c("Yes"), deff = TRUE, invert = TRUE)
tabulate_survey <- function(x, var, strata = NULL, pretty = TRUE, wide = TRUE,
                            digits = 1, method = "logit", deff = FALSE,
                            proptotal = FALSE, rowtotals = FALSE, 
                            coltotals = FALSE) {
  stopifnot(inherits(x, "tbl_svy"))

  # The idea behind this function is the fact that it can get complicated
  # to tabulate survey data with groupings. We originally wanted to lean
  # heavily on the srvyr package for this but ran into problems: 
  # https://github.com/gergness/srvyr/issues/49
  #
  # This takes in either character or bare variable names and will return a
  # table similar to that of `descriptive()` with the exception that it also
  # will have confidence intervals and design effects. 
  #
  # This will first tabulate the survey total using `srvyr::survey_total()` and
  # grab the design effect with `srvyr::survey_mean()`. 
  #
  # Unfortunately, because of the issue above and various other things with
  # srvyr::survey_mean(), it wasn't possible to get proportions for each strata,
  # so we had to roll our own. (See below for more details).
  #
  # After the tabulations are done, the counts are rounded, and the SE columns
  # are removed, the confidence intervals and mean estimates are collapsed into
  # a single column using `prettify_tabulation()`
  #
  # The results are in long data naturally, but if the user requests wide data
  # (which is the default), then the strata are spread out into columns using
  # the widen_tabulation function 

  cod  <- rlang::enquo(var)
  st   <- rlang::enquo(strata)
  vars <- tidyselect::vars_select(colnames(x), !! cod, !! st)
  cod  <- rlang::sym(vars[1])

  null_strata <- is.na(vars[2])
  if (null_strata) {
    st <- st 
  } else {
    if (x$has.strata && vars[2] != names(x$strata)[1]) {
      msg <- paste("The stratification present in the survey object (%s) does",
                   "not match the user-specified stratification (%s). If you",
                   "want to assess the survey tabulation stratifying by '%s',",
                   "re-specify the survey object with this",
                   "strata and the appropriate weights.")
      stop(sprintf(msg, names(x$strata)[1], vars[2], vars[2]))
    }
    st <- rlang::sym(vars[2])
  }

  x <- srvyr::select(x, !! cod, !!st)

  # here we are creating a dummy variable that is either the var or the
  # combination of var and strata so that we can get the right proportions from
  # the survey package in a loop below
  if (null_strata) {
    x <- srvyr::group_by(x, !! cod, .drop = FALSE)
  } else {
    # if there is a strata, create a unique, parseable dummy var by inserting
    # the timestamp in between the vars
    tim <- as.character(Sys.time())
    x <- srvyr::group_by(x, !! cod, !!st, .drop = FALSE)
  }

  # Calculating the survey total will also give us zero counts
  y <- srvyr::summarise(x,
                        n    = survey_total(vartype = "se", na.rm = TRUE),
                        mean = survey_mean(na.rm    = TRUE, deff  = deff))

  # Removing the mean values here because we are going to calculate them later
  y$mean             <- NULL
  y$mean_se          <- NULL
  if (deff) {
    names(y)[names(y) == "mean_deff"] <- "deff"
    y$deff[!is.finite(y$deff)] <- NA
  }



  # By this time, we already have a summary table with counts and deff. 
  # This will contain one or two columns in the front either being the counting
  # variable (cod) and *maybe* the stratifying variable (st) if it exists. 
  #
  # Because survey_mean does not calculate CI for factors using the svypropci, 
  # this gives negative confidence intervals for low values. The way we solve
  # it is to loop through all the values of the counter and use a logical test
  # for each one and then bind all the rows together. 
  #
  # If the user wants proportions relative to the total population (as opposed
  # to proportions relative to the strata, then we will take survey mean of
  # both of the stratifier and the counter variable, otherwise, we group by the
  # stratifier (if the user specified) and then count by the counter.
  #
  # Once we have this data frame, we will join it with the original result and
  # then make it pretty and/or wide.
  # make sure the survey is ungrouped
  xx      <- srvyr::ungroup(x)
  # get the column with all the values of the counter
  ycod    <- dplyr::pull(y, !! cod)
  if (!null_strata && proptotal) {
    # Calculate the survey proportion for both the stratifier and counter
    # @param xx a tbl_svy object
    # @param .x a single character value matching those found in the cod column
    # @param .y a single character value matching those found in the st column
    # @param cod a symbol specifying the column for the counter
    # @param st a symbol specifying the column for the stratifier
    # @return a data frame with five columns, the stratifier, the counter, 
    # proportion, lower, and upper.
    s_prop_strat <- function(xx, .x, .y, cod, st) {
      st  <- rlang::enquo(st)
      cod <- rlang::enquo(cod)
      res <- srvyr::summarise(xx, 
                              proportion = srvyr::survey_mean(!! cod == .x & !! st == .y,
                                                              proportion = TRUE,
                                                              vartype = "ci"))
      res <- dplyr::bind_cols(!! cod := .x, res)
      dplyr::bind_cols(!! st := .y, res)
    }
  } else {
    s_prop <- function(xx, .x, cod) {
      cod <- rlang::enquo(cod)
      res <- srvyr::summarise(xx, 
                              proportion = srvyr::survey_mean(!! cod == .x,
                                                              proportion = TRUE,
                                                              vartype = "ci"))
      dplyr::bind_cols(!! cod := rep(.x, nrow(res)), res)
    }
  }

  if (!null_strata) {
    # get the column with all the unique values of the stratifier
    yst <- dplyr::pull(y, !!  st)
    if (proptotal) {
      # map both the counter and stratifier to sprop
      props <- purrr::map2_dfr(ycod, yst, ~s_prop_strat(xx, .x, .y, !! cod, !! st))
    } else {
      # group by the stratifier and then map the counter
      xx    <- srvyr::group_by(xx, !! st, .drop = FALSE)
      g     <- unique(ycod)
      props <- purrr::map_dfr(g, ~s_prop(xx, .x, !! cod))
    }
    # Make sure that the resulting columns are factors
    codl  <- levels(ycod)
     stl  <- levels(yst)
    props <- dplyr::mutate(props, !! cod := factor(!! cod, levels = codl))
    props <- dplyr::mutate(props, !!  st := factor(!!  st, levels =  stl))

  } else {
    # no stratifier, just map the counter to sprop and make sure it's a factor
    xx    <- srvyr::ungroup(x)
    v     <- as.character(y[[1]])
    props <- purrr::map_dfr(ycod, ~s_prop(xx, .x, !! cod))
    codl  <- levels(dplyr::pull(y, !! cod))
    props <- dplyr::mutate(props, !! cod := factor(!! cod, levels = codl))
  }

  # Join the data together
  y       <- y[!colnames(y) %in% "n_se"]
  join_by <- if (null_strata) names(y)[[1]] else names(y)[1:2]
  y       <- dplyr::left_join(y, props, by = join_by)

  if (coltotals) {
    if (null_strata) {
      tot <- data.frame(n = sum(y$n, na.rm = TRUE))
    } else {
      # group by stratifier
      y   <- dplyr::group_by(y, !! st, .drop = FALSE)
      # tally up the Ns
      tot <- dplyr::tally(y, !! rlang::sym("n"))
      # bind to the long data frame
      y   <- dplyr::ungroup(y)
    }
    suppressMessages(y <- dplyr::bind_rows(y, tot))

    # replace any NAs in the cause of death with "Total"
    y <- dplyr::mutate(y, !! cod := forcats::fct_explicit_na(!! cod, "Total"))  
  }

  if (rowtotals && !null_strata) {
    # group by cause of death
    y   <- dplyr::group_by(y, !! cod, .drop = FALSE)
    # tally up the Ns
    tot <- dplyr::tally(y, !! rlang::sym("n"))
    # bind to the long data frame
    y   <- dplyr::ungroup(y)
    suppressMessages(y <- dplyr::bind_rows(y, tot))
    # replace any NAs in the stratifier with "Total"
    y <- dplyr::mutate(y, !! st := forcats::fct_explicit_na(!! st, "Total"))
  }


  if (wide && !null_strata) {
    y <- widen_tabulation(y, !! cod, !! st, pretty = pretty, digits = digits)
  } else if (pretty) {
    y <- prettify_tabulation(y, digits = digits)
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
prettify_tabulation <- function(y, digits = 1, ci_prefix = "") {


  ci <- trimws(sprintf("%s ci", ci_prefix))
  y <- unite_ci(y, ci, dplyr::contains("proportion"), percent = TRUE, digits = digits)

  # convert any NA% proportions to just NA
  y[[ci]] <- dplyr::if_else(grepl("NA%", y[[ci]]), NA_character_, y[[ci]])

  return(y)

}


#' Convert the table to wide format consistently
#'
#' @param y a data frame
#' @param cod variable of interest
#' @param st stratifying variable
#' @noRd
widen_tabulation <- function(y, cod, st, pretty = TRUE, digits = 1) {

  cod <- rlang::enquo(cod)
  st  <- rlang::enquo(st)

  # getting all the labels for the stratifier
  l   <- levels(dplyr::pull(y, !! st))

  #  1 Only select the necessary columns. n, deff, and prop are all numeric
  #    columns that need to be gathered
  #
  #  2 Gather "n", "deff", and "prop" into a single column
  #
  #  3 Make sure that everything is arranged in the correct order
  #
  #  4 Combine the stratifier and the n/prop signifier
  #
  #  5 Make sure the factors are in the correct order
  #
  #  6 Spread out the combined stratifier and signifier to columns
  #  
  #  7 Run through the stratified columns with map, and make them pretty
  y <- dplyr::select(y, !! cod, !! st, "n",
                     # deff, ci, and prop are all columns that _might_ exist
                     dplyr::matches("prop"), 
                     dplyr::starts_with("ci"), 
                     dplyr::starts_with("deff"))

  y     <- tidyr::gather(y, key = "variable", value = "value", -(1:2))
  
  y     <- dplyr::arrange(y, !! cod, !! st)

  y     <- tidyr::unite(y, "tmp", !! st, "variable", sep = " ")

  y$tmp <- forcats::fct_inorder(y$tmp)

  y     <- tidyr::spread(y, "tmp", "value")

  if (pretty) {
    # map through all the levels of l and pull out the matching columns
    tmp <- purrr::map(l, ~dplyr::select(y, dplyr::starts_with(paste0(., " "))))
    # pretty up those columns and bind them all together
    tmp <- purrr::map2_dfc(tmp, l, ~prettify_tabulation(.x, digits = digits, ci_prefix = .y))

    # names(tmp)[grepl("ci\\d*", names(tmp))] <- sprintf("%s ci", l)
    # glue the result to the first column of the data
    y   <- dplyr::bind_cols(y[1], tmp)
  }
  
  return(y)

}

#' @export
#' @rdname tabulate_survey
#' @param ... binary variables for tabulation
#' @param keep a vector of binary values to keep
#' @param invert if `TRUE`, the kept values are rejected. Defaults to `FALSE`
#'
tabulate_binary_survey <- function(x, ..., strata = NULL, proptotal = FALSE,
                                   keep = NULL, invert = FALSE, pretty = TRUE,
                                   wide = TRUE, digits = 1, method = "logit",
                                   deff = FALSE) {

  stopifnot(inherits(x, "tbl_svy"))
  if (is.null(keep)) {
    stop("Please provide a list of values to keep in the output.")
  }

  vars <- tidyselect::vars_select(colnames(x), ...)

  # Create list for results to go into that will eventually be bound together
  res <- vector(mode = "list", length = length(vars))
  names(res) <- vars

  # loop over each name in the list and tabulate the survey for that variable
  for (i in names(res)) {
    i        <- rlang::ensym(i)
    res[[i]] <- tabulate_survey(x,
                                var       = !! i,
                                strata    = !! enquo(strata),
                                proptotal = proptotal,
                                pretty    = pretty,
                                digits    = digits,
                                method    = method,
                                wide      = wide,
                                deff      = deff)

    # The ouptut columns will have the value as whatever i was, so we should
    # rename this to "value" to make it consistent
    names(res[[i]])[names(res[[i]]) == i] <- "value"
  }
  # Combine the results into one table
  suppressWarnings(res <- dplyr::bind_rows(res, .id = "variable"))

  # return the results with only the selected values
  res[if (invert) !res$value %in% keep else res$value %in% keep, ]
}
