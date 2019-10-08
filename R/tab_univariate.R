#' Produce odds ratios, risk ratios or incidence rate ratios
#'
#' @param x A data frame
#'
#' @param outcome Name of A TRUE/FALSE variable as your outcome of interest
#'   (e.g. illness)
#'
#' @param ... Names of TRUE/FALSE variables as exposures of interest (e.g. risk
#'   factors)
#'
#' @param perstime A numeric variable containing the observation time for each
#'   individual
#'
#' @param strata Name of a TRUE/FALSE variable to be used for stratifying
#'   results. Note that this results in a different output table - giving you a
#'   table of crude measure, measures for each strata and the mantel-haeszel
#'   adjusted measure for each exposure variable listed in `...`
#'
#' @param measure Specify what you would like to calculated, options are "OR",
#'   "RR" or "IRR" default is "OR"
#'
#' @param extend_output TRUE/FALSE to specify whether would like all columns in
#'   the outputs (default is TRUE) Non-extended output drops group odds or risk
#'   calculations as well as p-values
#'
#' @param digits Specify number of decimal places (default is 3)
#'
#' @param mergeCI Whether or not to put the confidence intervals in one column
#'   (default is FALSE)
#'
#' @param woolf_test Only if strata specified and measure is "RR" or "OR".
#'   TRUE/FALSE to specify whether to include woolf test for homogeneity
#'   p-value. Tests whether there is a significant difference in the estimates
#'   between strata.
#'
#' @importFrom dplyr select mutate_at group_by summarise
#'
#' @references Inspired by Daniel Gardiner,
#' see [github repo](https://github.com/DanielGardiner/UsefulFunctions/tree/master/single.variable.analysis.v0.3.R)
#' @export
#' @examples
#'
#' # generate a fake dataset
#' a <- data.frame(case_def = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            riskA = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            riskB = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            stratifier = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            perstime = sample(150:250, 2000, replace = TRUE)
#'            )
#'
#' # get the results from tab_univariate function
#' func_res <- tab_univariate(a, case_def, riskA,
#'                            strata = stratifier, digits = 6, measure = "OR")
#'
#' # get risk ratios
#' func_res <- tab_univariate(a, case_def, riskA,
#'                            strata = stratifier, digits = 6, measure = "RR")
#'
#'
tab_univariate <- function(x, outcome, ... , perstime = NULL, strata = NULL,
                           measure = "OR", extend_output = TRUE,
                           digits = 3, mergeCI = FALSE, woolf_test = FALSE) {

  # check that x is a data frame
  if (!is.data.frame(x)) {
    stop("x must be a data frame")
  }

  ### Selecting variables

  # pull multiple variables from ...
  the_vars <- tidyselect::vars_select(colnames(x), ...)

  # select the var in the outcome column
  outcome_var <- tidyselect::vars_select(colnames(x), {{outcome}})

  # select the var in the perstime column
  perstime_var <- tidyselect::vars_select(colnames(x), {{perstime}})


  # select the var in the strata column
  strata_var <- tidyselect::vars_select(colnames(x), {{strata}})


  ### checks and messasges


  # check that outcome variable is logical
  if (!is.logical(x[[outcome_var]])) {
    stop("outcome must be a TRUE/FALSE variable")
  }

  # check that strata variable is logical
  if (length(strata_var) > 0 && !is.logical(x[[strata_var]])) {
    stop("strata variable must be a TRUE/FALSE variable")
  }

  # check person time is not missing for incidence rate ratio
  if (length(perstime_var) == 0 && measure == "IRR") {
    stop(glue::glue("You have selected IRR as a measure but not specified a perstime variable.",
         "To calculate an incidence rate ratio please specify a variable which indicates",
         "observation time for each individual"))
  }

  # lapply to each of the vars
  purrr::map_dfr(the_vars,
                 backend_tab_univariate,
                 # Exposure in here
                 outcome       = outcome_var,
                 x             = x,
                 perstime      = perstime_var,
                 strata        = strata_var,
                 measure       = measure,
                 extend_output = extend_output,
                 digits        = digits,
                 mergeCI       = mergeCI,
                 woolf_test    = woolf_test
  )


}



# the single exposure variable version of the above function
#' Backend for tab_univariate
#'
#' This is an internal function that does the work of tab_univariate over
#' several exposure variables
#'
#' @param exposure a character
#' @param outcome a character`
#' @param x a data frame
#' @param perstime a character
#' @param strata a character
#' @param measure either "OR" or "RR"
#' @param extend_output logical
#' @param digits an integer
#' @param mergeCI logical
#' @param woolf_test logical
#' @noRd
#'
#' @return a data frame
backend_tab_univariate <- function(exposure, outcome, x, perstime = NULL, strata = NULL,
                           measure = "OR", extend_output = TRUE,
                           digits = 3, mergeCI = FALSE, woolf_test = FALSE) {


  ### Selecting variables
  # select the vars in the dots
  exposure_var <- exposure
  exposure     <- if (length(exposure_var) > 0) rlang::sym(exposure_var) else NULL
  # check if exposure variable is logical
  if (!is.logical(x[[exposure_var]])) {
    stop("exposure variable must be a TRUE/FALSE variable")
  }

  # select the var in the outcome column
  outcome_var <- outcome
  outcome     <- if (length(outcome_var) > 0) rlang::sym(outcome_var) else NULL

  # select the var in the perstime column
  perstime_var <- perstime
  perstime     <- if (length(perstime_var) > 0) rlang::sym(perstime_var) else NULL


  # select the var in the strata column
  strata_var <- strata
  has_strata <- length(strata_var) > 0
  strata     <- if (has_strata) rlang::sym(strata_var) else NULL



  # swap the factor levels so TRUE comes first (required by epiR::epi2by2 function)
  x[[outcome_var]]  <- factor(x[[outcome_var]], levels = c("TRUE", "FALSE"))
  x[[exposure_var]] <- factor(x[[exposure_var]], levels = c("TRUE", "FALSE"))

  # swap factor levels for strata if not null
  if (has_strata) {
    x[[strata_var]] <- factor(x[[strata_var]], levels = c("TRUE", "FALSE"))
  }


  ### pulling together counts for to feed epi function

  # for "IRR" return counts and person time by exposure
  if (measure == "IRR") {

    # if stratifier specified then do by each group
    if (has_strata) {
      # sum outcome and obstime by exposure and strata
      x_table <- group_by(x, {{exposure}}, {{strata}})
      x_table <- summarise(x_table,
                           otcm = sum({{outcome}} == TRUE, na.rm = TRUE),
                           tme  = sum({{perstime}}, na.rm = TRUE))

      arr <- tidyr::gather(x_table, variable, value, -{{exposure}}, -{{strata}})
      arr <- dplyr::arrange(arr, {{strata}}, variable, {{exposure}})

      the_table <- array(arr$value,
                         dim = c(2, 2, 2),
                         dimnames = list(
                                         unique(arr[[exposure_var]]),
                                         unique(arr$variable),
                                         unique(arr[[strata_var]])
                         )
      )
      names(dimnames(the_table)) <- c(exposure_var, outcome_var, strata_var)

    } else { # if no stratifier then simple table
      # sum outcome and obstime by exposure
      the_table <- group_by(x, {{exposure}})
      the_table <- summarise(the_table,
                             otcm = sum({{outcome}} == TRUE, na.rm = TRUE),
                             tme  = sum({{perstime}}, na.rm = TRUE))

      # drop the first column and change to a table (for use in epi.2by2)
      the_table <- as.table(data.matrix(the_table[,2:3]))
    }
  } else {
  
    the_table <- table(x[c(exposure_var, outcome_var, strata_var)])

  }

  
  if (has_strata) {

    vals <- rbind(strata_ratio_table(the_table, measure), NA)
    if (measure != "IRR") {
      vals <- rbind(vals, NA)
    }
    est  <- get_ratio_est(the_table, measure, conf = 0.95, strata_name = strata_var)
    nums <- dplyr::bind_cols(variable = rep(exposure_var, nrow(est)), 
                             est_type = rownames(est), 
                             vals, 
                             est)
  } else {
    nums <- dplyr::bind_cols(variable = exposure_var,
                             est_type = "crude",
                             strata_ratio_table(the_table, measure),
                             get_ratio_est(the_table, measure)
    )
  }

  # drop columns if specified
  # use numbers because names will be different according to measure, but place is always same
  if (!extend_output) {
    to_keep <- !grepl("(odds|risk|incidence)$", names(nums))
    nums    <- nums[to_keep]
  }

  # drop woolf-test pvalue
  if (!woolf_test) {
    nums <- nums[!nums$est_type == "woolf", ]
  }

  # merge upper and lower CIs
  if (mergeCI) {
    nums <- unite_ci(nums, col = "est_ci", "ratio", "lower", "upper", m100 = FALSE, digits = digits)
  }

  # spit out the out table
  return(nums)
}

