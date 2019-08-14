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
#' @importFrom epiR epi.2by2
#' @importFrom dplyr select mutate_at group_by summarise
#'
#' @references Inspired by Daniel Gardiner,
#' see [github repo](https://github.com/DanielGardiner/UsefulFunctions/single.variable.analysis.v0.3.R)
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
    stop("You have selected IRR as a measure but not specified a perstime variable.
         To calculate an incidence rate ratio please specify a variable which indicates
         observation time for each individual")
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
#' @noRd
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



  ### backing data and fixing variable levels
  # create a temp dataset so we dont overwrite the users dataset with factors
  temp <- x


  # swap the factor levels so TRUE comes first (required by epiR::epi2by2 function)
  temp <- mutate_at(temp, vars(outcome_var, exposure_var),
                 ~factor(., levels = c(TRUE, FALSE)))

  # swap factor levels for strata if not null
  if (!is.null(strata_var)) {
    temp <- mutate_at(temp, vars(strata_var),
                      ~factor(., levels = c(TRUE, FALSE)))
  }


  ### pulling together counts for to feed epi function

  # for "OR" get basic counts table to pass to the epiR::epi2by2 function
  # NOTE that this is flipped the wrong way (cases/controls as rows) - because epi2by2 calculates ORs WRONG!!
  if (measure == "OR") {

    the_table <- table(temp[c(outcome_var, exposure_var, strata_var)])

  }

  # for "RR" get basic counts table to pass to the epiR::epi2by2 function
  # NOTE for RRs - having cases as columns for input is fine for using epi2by2
  if (measure == "RR") {

    the_table <- table(temp[c(exposure_var, outcome_var, strata_var)])

  }

  # for "IRR" return counts and person time by exposure
  if (measure == "IRR") {

    # if stratifier specified then do by each group
    if (has_strata) {
      # sum outcome and obstime by exposure and strata
      temp_table <- group_by(temp, {{exposure}}, {{strata}})
      temp_table <- summarise(temp_table,
                              otcm = sum({{outcome}} == TRUE, na.rm = TRUE),
                              tme  = sum({{perstime}}, na.rm = TRUE))

      arr <- tidyr::gather(temp_table, variable, value, -{{exposure}}, -{{strata}})
      arr <- dplyr::arrange(arr, {{strata}}, variable, {{exposure}})

      the_table <- array(arr$value,
                         dim = c(2, 2, 2),
                         dimnames = list(
                                         unique(arr[[exposure_var]]),
                                         unique(arr$variable),
                                         unique(arr[[strata_var]])
                         )
      )
    } else { # if no stratifier then simple table
      # sum outcome and obstime by exposure
      the_table <- group_by(temp, {{exposure}})
      the_table <- summarise(the_table,
                             otcm = sum({{outcome}} == TRUE, na.rm = TRUE),
                             tme  = sum({{perstime}}, na.rm = TRUE))

      # drop the first column and change to a table (for use in epi.2by2)
      the_table <- as.table(data.matrix(the_table[,2:3]))
    }
  } 




  ### Run the epiR::epi2by2 function on the counts data to get appropriate outputs

  ## pull together an Odds ratio table
  # note that for this pulls the counts of cases and controls (not the totals by exposure) - for calculating ODDS
  if (measure == "OR") {

    # get a contingency table with lots of stats in it
    epitable <- suppressWarnings(epiR::epi.2by2(the_table, method = "case.control"))

    # for non stratified results, simply pull together one liners
    if (!has_strata) {
      # pull outputs together
      nums <- cbind(exposure_var,                           # name of the exposure variable
                    get_epitable_values(epitable, measure), # extract the values from the table
                    get_epitable_ci(epitable, measure)      # pull the estimate, CIs, and p-value
      )
      # set correct column names
      colnames(nums) <- c("variable",
                          "exp_cases", "unexp_cases", "cases_odds",
                          "exp_controls", "unexp_controls", "controls_odds",
                          "est", "lower", "upper", "pval"
      )
      # remove row names
      rownames(nums) <- NULL

    }

    # if strata specified then pull together four rows (crude, strataTRUE, strataFALSE and MH estimates)
    if (has_strata) {

        # crude counts and estimates
        crude <- cbind(exposure_var,                  # name of the exposure variable
                       "crude",                       # type of estimate
                    get_epitable_values(epitable, measure), # extract the values from the table
                    # get_epitable_ci(epitable, measure, "crude"),
                       epitable$massoc$OR.crude.wald, # pull the the OR and CIs
                       epitable$massoc$chisq.crude[3], # pull the p-value
                       NA                              # Leave space for wolf-test of homogeneity in strata rows
                       )

        # stratified counts and estimates
        stratified <- cbind(rep(exposure_var, 2),    # name of the exposure variable repeated for each strata
                c("strata_TRUE", "strata_FALSE"),    # type of estimate
                strata_ratio_table(the_table, measure),
                epitable$massoc$OR.strata.wald,       # pull the OR and CIs for each strata
                data.frame(                           # pull the p-values for each strata as a dataframe
                  epitable$massoc$chisq.strata[,3]
                  ),
                rbind(epitable$massoc$OR.homog.woolf[,3], NA)    # pull the woolf test of homogeneity p-value
                )


          # mantel-haenszel counts (NAs) and estimates
          mh <- cbind(exposure_var,                    # name of exposure variable
                "mh",                                  # type of estimate
                t(rep(NA, 6)),                         # make all the counts and odds NAs
                epitable$massoc$OR.mh.wald,            # pull the mh OR and CIS
                epitable$massoc$chisq.mh$p.value,      # pull the mh pvalue
                NA                                     # Leave space for wolf-test of homogeneity in strata rows
                )

          # remove all the colnames and column names
          colnames(crude) <- NA
          colnames(stratified) <- NA
          colnames(mh) <- NA
          rownames(crude) <- NULL
          rownames(stratified) <- NULL
          rownames(mh) <- NULL


        # bind the four rows together (each cbinded together seperately below)
        nums <- rbind(crude, stratified, mh)

        # set correct column names
        colnames(nums) <- c("variable",
                            "est_type",
                            "exp_cases", "unexp_cases", "cases_odds",
                            "exp_controls", "unexp_controls", "controls_odds",
                            "est", "lower", "upper", "pval", "woolf_pval"
                            )
        # remove row names
        rownames(nums) <- NULL

    }
  }

  ## pull together a risk ratio table
  # note that this pulls the counts of cases and the total observations - for calculating RISKS

  if (measure == "RR") {

    # get a contingency table with lots of stats in it
    epitable <- suppressWarnings(epiR::epi.2by2(the_table, method = "cohort.count"))

    # for non stratified results, simply pull together one liners
    if (!has_strata) {
      # pull outputs together
      nums <- cbind(exposure_var,                           # name of the exposure variable
                    get_epitable_values(epitable, measure), # extract the values from the table
                    get_epitable_ci(epitable, measure)      # pull the estimate, CIs, and p-value
      )
      # set correct column names
      colnames(nums) <- c("variable",
                          "exp_cases", "exp_total", "exp_risk",
                          "unexp_cases", "unexp_total", "unexp_risk",
                          "est", "lower", "upper", "pval"
      )
      # remove row names
      rownames(nums) <- NULL
    }

    # if strata specified then pull together four rows (crude, strataTRUE, strataFALSE and MH estimates)
    if (has_strata) {

      # crude counts and estimates
      crude <- cbind(exposure_var,                   # name of the exposure variable
                     "crude",                        # type of estimate
                    get_epitable_values(epitable, measure), # extract the values from the table
                     epitable$massoc$RR.crude.wald,  # pull the the RR and CIs
                     epitable$massoc$chisq.crude[3], # pull the p-value
                     NA                              # Leave space for wolf-test of homogeneity in strata rows
      )

      # stratified counts and estimates
      stratified <- cbind(rep(exposure_var, 2),                # name of the exposure variable repeated for each strata
                          c("strata_TRUE", "strata_FALSE"),    # type of estimate
                          strata_ratio_table(the_table, measure),
                          epitable$massoc$RR.strata.wald,       # pull the RR and CIs for each strata
                          data.frame(                           # pull the p-values for each strata as a dataframe
                            epitable$massoc$chisq.strata[,3]
                          ),
                          rbind(epitable$massoc$RR.homog.woolf[,3], NA)    # pull the woolf test of homogeneity p-value
                          )

      # mantel-haenszel counts (NAs) and estimates
      mh <- cbind(exposure_var,                          # name of exposure variable
                  "mh",                                  # type of estimate
                  t(rep(NA, 6)),                         # make all the counts and odds NAs
                  epitable$massoc$RR.mh.wald,            # pull the mh RR and CIS
                  epitable$massoc$chisq.mh$p.value,      # pull the mh pvalue
                  NA                                     # Leave space for wolf-test of homogeneity in strata rows
      )

      # remove all the colnames and column names
      colnames(crude) <- NA
      colnames(stratified) <- NA
      colnames(mh) <- NA
      rownames(crude) <- NULL
      rownames(stratified) <- NULL
      rownames(mh) <- NULL


      # bind the four rows together (each cbinded together seperately below)
      nums <- rbind(crude, stratified, mh)

      # set correct column names
      colnames(nums) <- c("variable",
                          "est_type",
                          "exp_cases", "exp_total", "exp_risk",
                          "unexp_cases", "unexp_total", "unexp_risk",
                          "est", "lower", "upper", "pval", "woolf_pval"
      )
      # remove row names
      rownames(nums) <- NULL


    }

  }

  ## pull together a incidence rate ratio table
  # note that this pulls the counts of cases and the total observation time - for calculating INCIDENCE RATES

  if (measure == "IRR") {
    # get a contingency table with lots of stats in it
    epitable <- suppressWarnings(epiR::epi.2by2(the_table, method = "cohort.time"))

    # for non stratified results, simply pull together one liners
    if (!has_strata) {

      # pull outputs together
      nums <- cbind(exposure_var,                           # name of the exposure variable
                    get_epitable_values(epitable, measure), # extract the values from the table
                    get_epitable_ci(epitable, measure)      # pull the estimate, CIs, and p-value
      )


      # set correct column names
      colnames(nums) <- c("variable",
                          "exp_cases", "exp_perstime", "exp_incidence",
                          "unexp_cases", "unexp_perstime", "unexp_incidence",
                          "est", "lower", "upper", "pval"
      )
      # remove row names
      rownames(nums) <- NULL

    }

    # if strata specified then pull together four rows (crude, strataTRUE, strataFALSE and MH estimates)
    if (has_strata) {

      # crude counts and estimates
      crude <- cbind(exposure_var,               # name of the exposure variable
                     "crude",                    # type of estimate
                    get_epitable_values(epitable, measure), # extract the values from the table
                     epitable$massoc$IRR.crude.wald, # pull the the IRR and CIs
                     epitable$massoc$chisq.crude[3] # pull the p-value
      )

      # stratified counts and estimates
      stratified <- cbind(rep(exposure_var, 2),                # name of the exposure variable repeated for each strata
                          c("strata_TRUE", "strata_FALSE"),    # type of estimate
                          strata_ratio_table(the_table, measure = measure),
                          epitable$massoc$IRR.strata.wald,       # pull the RR and CIs for each strata
                          data.frame(                            # pull the p-values for each strata as a dataframe
                            epitable$massoc$chisq.strata[,3]
                          )
      )

      # mantel-haenszel counts (NAs) and estimates
      mh <- cbind(exposure_var,                          # name of exposure variable
                  "mh",                                  # type of estimate
                  t(rep(NA, 6)),                         # make all the counts and odds NAs
                  epitable$massoc$IRR.mh.wald,           # pull the mh IRR and CIS
                  epitable$massoc$chisq.mh$p.value       # pull the mh pvalue
      )

      # remove all the colnames and column names
      colnames(crude) <- NA
      colnames(stratified) <- NA
      colnames(mh) <- NA
      rownames(crude) <- NULL
      rownames(stratified) <- NULL
      rownames(mh) <- NULL


      # bind the four rows together (each cbinded together seperately below)
      nums <- rbind(crude, stratified, mh)

      # set correct column names
      colnames(nums) <- c("variable",
                          "est_type",
                          "exp_cases", "exp_perstime", "exp_incidence",
                          "unexp_cases", "unexp_perstime", "unexp_incidence",
                          "est", "lower", "upper", "pval"
                          )
      # remove row names
      rownames(nums) <- NULL


    }

  }


  # turn things to numeric
  nums <- mutate_at(nums, vars(-tidyselect::one_of("variable", "est_type")), as.numeric)

  # drop columns if specified
  # use numbers because names will be different according to measure, but place is always same
  if (!extend_output & !has_strata) {
    nums <- select(nums, -4, -7, -11)
  }
  if (!extend_output & has_strata) {
    nums <- select(nums, -5, -8, -12)
  }

  # drop woolf-test pvalue
  if (length(strata_var != 0) & measure != "IRR" & woolf_test == FALSE) {
    nums <- select(nums, -tidyselect::one_of("woolf_pval"))
  }

  # merge upper and lower CIs
  if (mergeCI) {
    nums <- unite_ci(nums, col = "est_ci", "est", "lower", "upper", m100 = FALSE, digits = digits)
  }

  # change output table to a tibble
  # nums <- tibble(nums)

  # spit out the out table
  return(nums)
}

