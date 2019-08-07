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
#' @param na.rm When `TRUE`, missing (NA) values present in `var` will be removed
#'   from the data set with a warning, causing a change in denominator for the
#'   tabulations.  The default is set to `FALSE`, which creates an explicit
#'   missing value called "(Missing)".
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
                            digits = 1, method = "logit", na.rm = FALSE, deff = FALSE,
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
      msg <- glue::glue(
        "The stratification present in the survey object ({names(x$strata[1])})",
        "does not match the user-specified stratification ({vars[2]}). If you",
        "want to assess the survey tabulation stratifying by '{vars[2]}',",
        "re-specify the survey object with this",
        "strata and the appropriate weights.",
        .sep = " "
        )
      stop(msg)
    }
    st <- rlang::sym(vars[2])
  }

  x <- srvyr::select(x, !! cod, !!st)

  # If the counter variable is numeric or logical, we need to convert it to a
  # factor. For logical variables, this is trivial, so we just do it.
  if (is.logical(x$variables[[vars[1]]])) {
    x <- srvyr::mutate(x, !! cod := factor(!! cod, levels = c("TRUE", "FALSE")))
  }
  # For numeric data, however, we need to warn the user
  if (is.numeric(x$variables[[vars[1]]])) {
    warning(glue::glue("converting `{vars[1]}` to a factor"))
    x <- srvyr::mutate(x, !! cod := fac_from_num(!! cod))
  }

  # if there is missing data, we should treat it by either removing the rows
  # with the missing values or making the missing explicit.
  if (na.rm) {
    nas <- sum(is.na(x$variables[[vars[1]]]))
    if (nas > 0) {
      warning(glue::glue("removing {nas} missing value(s) from `{vars[1]}`"))
      x <- srvyr::filter(x, !is.na(!! cod))
    }
  } else {
    x <- srvyr::mutate(x, !! cod := forcats::fct_explicit_na(!! cod, na_level = "(Missing)"))
  }

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
                        n    = srvyr::survey_total(vartype = "se", na.rm = TRUE),
                        mean = srvyr::survey_mean(na.rm    = TRUE, deff  = deff))

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
#' @param transpose if `wide = TRUE`, then this will transpose the columns to 
#'   the rows, which is useful when you stratify by age group. Default is
#'   `NULL`, which will not transpose anything. You have three options for
#'   transpose:
#'    - `transpose = "variable"`: uses the variable column, dropping values.
#'       Use this if you know that your values are all identical or at least
#'       identifiable by the variable name.
#'    - `transpose = "value"`   : uses the value column, dropping variables.
#'       Use this if your values are important and the variable names are
#'       generic placeholders.
#'    - `transpose = "both"`    : combines the variable and value columns.
#'       Use this if both the variables and values are important.
#'    If there is no stratification, the results will produce a single-row table
tabulate_binary_survey <- function(x, ..., strata = NULL, proptotal = FALSE,
                                   keep = NULL, invert = FALSE, pretty = TRUE,
                                   wide = TRUE, digits = 1, method = "logit",
                                   na.rm = FALSE, deff = FALSE, transpose = NULL) {

  stopifnot(inherits(x, "tbl_svy"))
  if (is.null(keep)) {
    stop("Please provide a list of values to keep in the output.")
  }

  vars <- tidyselect::vars_select(colnames(x), ...)
  stra <- rlang::enquo(strata)


  flip_it <- wide && !is.null(transpose)

  # Create list for results to go into that will eventually be bound together
  res <- vector(mode = "list", length = length(vars))
  names(res) <- vars


  # loop over each name in the list and tabulate the survey for that variable
  for (i in names(res)) {
    i        <- rlang::ensym(i)
    res[[i]] <- tabulate_survey(x,
                                var       = !! i,
                                strata    = !! stra,
                                proptotal = proptotal,
                                pretty    = pretty,
                                digits    = digits,
                                method    = method,
                                wide      = wide,
                                na.rm     = na.rm,
                                deff      = deff)

    # The ouptut columns will have the value as whatever i was, so we should
    # rename this to "value" to make it consistent
    names(res[[i]])[names(res[[i]]) == i] <- "value"
  }
  # Combine the results into one table
  suppressWarnings(res <- dplyr::bind_rows(res, .id = "variable"))

  # return the results with only the selected values
  res <- res[if (invert) !res$value %in% keep else res$value %in% keep, ]
  
  # If the user wants to transpose the data, then we need to do this for each
  # level of data available into separate tables, combine the columns, and then
  # rearrange them so that they are grouped by variable/value

  if (flip_it) {
    res <- flipper(x, res, transpose, pretty = pretty, stra = stra, var = var)    
  }
  res
}

flipper <- function(x, res, transpose = c("variable", "value", "both"), pretty = TRUE, stra, var) {

  transpose <- match.arg(tolower(transpose), c("variable", "value", "both"))
  if (transpose == "both") {
    # if the user wants to keep both columns, then we unite them and then
    res <- tidyr::unite(res, col = "both", "variable", "value", remove = TRUE)
  }

  # number of rows in the original table
  nr   <- seq_len(nrow(res))
  # if deff exists in the table
  deff <- any(grepl("deff$", names(res)))
  # number of new columns based on the number of rows
  nc   <- 1L + deff + if (pretty) 1L else 3L
  # the variable column to be transposed
  var  <- rlang::ensym(transpose)

  # when there is no strata, we have to come up with a dummy variable
  strata_exists <- tidyselect::vars_select(colnames(x), !! stra)
  strata_exists <- length(strata_exists) > 0

  if (strata_exists) {
    slevels <- dplyr::pull(x$variables, !! stra)
    slevels <- if (is.factor(slevels)) levels(slevels) else sort(slevels)
  } else {
    stra      <- paste0("__", as.integer(Sys.time()))
    stra      <- rlang::ensym(stra)
    old_names <- names(res)[names(res) != transpose]
    names(res)[names(res) != transpose] <- paste0(" ", old_names)
    slevels   <- NULL
  }

  # transposing the count variable, which is always there.
  tres <- transpose_pretty(res, !! stra, !! var, !! rlang::sym("n"), slevels)

  # determining the list of suffixes to run through when appending the columns
  suffix <- c(
              if (deff) "deff" else NULL,
              if (pretty) "ci" else c("proportion", "proportion_low", "proportion_upp")
  )

  # transposing and appending the columns
  for (i in suffix) {
    suff <- rlang::ensym(i)
    tmp  <- transpose_pretty(res, !! stra, !! var, !! suff, slevels)
    tres <- dplyr::bind_cols(tres, tmp[-1])
  }

  # re-ordering the columns so that they are grouped by the original row order
  res <- tres[c(1, order(rep(nr, nc)) + 1)]
  if (!strata_exists) {
    res <- dplyr::select(res, - !! stra)
  }
  res
}

tab_survey <- function(dt,
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
tabulate_binary_survey(x = dt, ..., 
                       strata = {{ strata }}, 
                       proptotal = prop_total, 
                       keep = keep, 
                       invert = TRUE, 
                       pretty = pretty, 
                       wide = wide, 
                       digits = digits, 
                       method = method,
                       na.rm = na.rm, 
                       deff = deff,
                       transpose = transpose) 

}
