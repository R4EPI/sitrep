#' Produce odds ratios, risk ratios or incidence rate ratios with accompanying confidence intervals
#' @param x A data frame
#' @param outcome Name of A TRUE/FALSE variable as your outcome of interest (e.g. illness)
#' @param ... Names of TRUE/FALSE variables as exposures of interest (e.g. risk factors)
#' @param perstime A numeric variable containing the observation time for each individual
#' @param strata Name of a TRUE/FALSE variable to be used for stratifying results. Note that this results
#' in a different output table - giving you a table of crude measure, measures for each strata and
#' the mantel-haeszel adjusted measure for each exposure variable listed in `...`
#' @param measure Specify what you would like to calculated, options are "OR", "RR" or "IRR"
#' default is "OR"
#' @param extend_output TRUE/FALSE to specify whether would like all columns in the outputs (default is TRUE)
#' Non-extended output drops group odds or risk calculations as well as p-values
#' @param digits Specify number of decimal places (default is 3)
#' @param mergeCI Whether or not to put the confidence intervals in one column (default is FALSE)
#' @importFrom epiR epi.2by2
#' @importFrom dplyr select
#' @references Inspired by Daniel Gardiner,
#' see [github repo](https://github.com/DanielGardiner/UsefulFunctions/blob/efffde624d424d977651ed1a9ee4430cbf2b0d6f/single.variable.analysis.v0.3.R#L12)
#' @export
#' @examples
#' # generate a fake dataset
#' a <- tibble(potato = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            apple = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            orange = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            stratifier = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            perstime = sample(150:250, 2000, replace = TRUE)
#'            )
#'
#' # get counts table
#' counts <- table(a[c("apple", "potato")])
#'
#' ## For odds ratios
#'
#' # get odds of exposure among cases
#' expo_cases_odds <- counts[4]/counts[3]
#'
#' # get odds of exposure among controls
#' expo_controls_odds <- counts[2]/counts[1]
#'
#' # calculate odds ratio by using odds above
#' or_from_odds <- expo_cases_odds / expo_controls_odds
#'
#' # calculate odds ratio by cross multiplying counts
#' or_from_cross <- (counts[4] * counts[1]) / (counts[2] * counts[3])
#'
#' # get the results from tab_univariate function
#' func_res <- tab_univariate(a, potato, apple)
#'
#' # tests
#' stopifnot(
#' # check that function OR is equal to OR from odds
#' func_res$exp == or_from_odds,
#'
#' # check that function OR is equal to OR from cross multiplying
#' func_res$exp == or_from_cross,
#'
#' # check that function case_odds is equal to count odds
#' func_res$cases_odds == expo_cases_odds,
#' func_res$controls_odds == expo_controls_odds
#' )

tab_univariate <- function(x, outcome, exposure, perstime = NULL, strata = NULL, measure = "OR", extend_output = TRUE,
                           digits = 3, mergeCI = FALSE) {


  ### Selecting variables
  # select the vars in the dots
  exposure_var <- tidyselect::vars_select(colnames(x), {{exposure}})

  # select the var in the outcome column
  outcome_var <- tidyselect::vars_select(colnames(x), {{outcome}})

  # select the var in the perstime column
  perstime_var <- tidyselect::vars_select(colnames(x), {{perstime}})


  # select the var in the strata column
  strata_var <- tidyselect::vars_select(colnames(x), {{strata}})


  ### checks and messasges

  # # check that x is a data frame
  # if (!is.data.frame(x)) {
  #   stop("x must be a data frame")
  # }
  #
  # # check that outcome variable is logical
  # if (!is.logical(outcome_var)) {
  #   stop("outcome must be a TRUE/FALSE variable")
  # }
  #
  # # check if exposure variable is logical
  # if (!is.logical(exposure_var)) {
  #   stop("exposure variable must be a TRUE/FALSE variable")
  # }
  #
  # # check that strata variable is logical
  # if (!is.logical(strata_var)) {
  #   stop("strata variable must be a TRUE/FALSE variable")
  # }
  #
  # # check person time is not missing for incidence rate ratio
  # if (is.null(perstime_var) & measure == "IRR") {
  #   stop("You have selected IRR as a measure but not specified a perstime variable.
  #        To calculate an incidence rate ratio please specify a variable which indicates
  #        observation time for each individual")
  # }


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

      # sum outcome and obstime by exposure
      the_table <- group_by(temp, {{exposure}})
      the_table <- summarise(the_table,
                          otcm = sum({{outcome}} == TRUE),
                          tme = sum({{perstime}}))

      the_table <- as.table(data.matrix(the_table[,2:3]))

    }





  ### Run the epiR::epi2by2 function on the counts data to get appropriate outputs

  # pull together an Odds ratio table
  # note that for this pulls the counts of cases and controls (not the totals by exposure) - for calculating ODDS
  if (measure == "OR") {

    # get a contingency table with lots of stats in it
    epitable <- epiR::epi.2by2(the_table, method = "case.control")

    # for non stratified results, simply pull together one liners
    if (length(strata_var) == 0) {
      # pull outputs together
      nums <- cbind(exposure_var,                   # name of the exposure variable
                    epitable$tab[1L, c(1L, 2L)],    # pull counts of exposed among cases (REMEMBER IS FLIPPED!)
                    epitable$tab[1L, 5L],           # pull odds of exposure among cases
                    epitable$tab[2L, c(1L, 2L)],    # pull counts of exposed among among controls
                    epitable$tab[2L, 5L],           # pull odds of exposure among controls
                    epitable$massoc$OR.strata.wald, # pull the the OR and CIs
                    epitable$massoc$chisq.strata[3] # pull the p-value
      )
      colnames(nums) <- c("variable",
                          "exp_cases", "unexp_cases", "cases_odds",
                          "exp_controls", "unexp_controls", "controls_odds",
                          "est", "lower", "upper", "pval"
      )

    }

    # if strata specified then pull together four rows (crude, strataTRUE, strataFALSE and MH estimates)
    if (length(strata_var) != 0) {

        # crude counts and estimates
        crude <- cbind(exposure_var,                  # name of the exposure variable
                       "crude",                       # type of estimate
                       epitable$tab[1L, c(1L, 2L)],   # pull counts of exposed among cases (REMEMBER IS FLIPPED!)
                       epitable$tab[1L, 5L],          # pull odds of exposure among cases
                       epitable$tab[2L, c(1L, 2L)],   # pull counts of exposed among controls
                       epitable$tab[2L, 5L],          # pull odds of exposure among controls
                       epitable$massoc$OR.crude.wald, # pull the the OR and CIs
                       epitable$massoc$chisq.crude[3] # pull the p-value
                       )

        # stratified counts and estimates
        stratified <- cbind(rep(exposure_var, 2),    # name of the exposure variable repeated for each strata
                c("strata_TRUE", "strata_FALSE"),    # type of estimate
                rbind(                               # row bind strata counts and odds together
                  cbind(                                  # cbind strata_true counts together seperately
                    t(the_table[TRUE][c(1,3)]),             # pull counts of exposed among cases
                    the_table[TRUE][1] /
                      the_table[TRUE][3],                   # calculate odds of exposure among cases
                    t(the_table[TRUE][c(2,4)]),             # pull counts of exposed among controls
                    the_table[TRUE][2] /
                      the_table[TRUE][4]                    # calculate odds of exposure among cases
                    ),
                  cbind(                                  # cbind strata_false outcomes counts together seperately
                    t(the_table[TRUE][c(5,7)]),             # pull counts of exposure among cases
                    the_table[TRUE][5] /
                      the_table[TRUE][7],                   # calculate odds of exposure among cases
                    t(the_table[TRUE][c(6,8)]),             # pull counts of exposure among controls
                    the_table[TRUE][6] /
                      the_table[TRUE][8]                    # calculate odds of exposure among controls
                  )
                  ),
                epitable$massoc$OR.strata.wald,       # pull the OR and CIs for each strata
                data.frame(                           # pull the p-values for each strata as a dataframe
                  epitable$massoc$chisq.strata[,3]
                  )
                )


          # mantel-haenszel counts (NAs) and estimates
          mh <- cbind(exposure_var,                    # name of exposure variable
                "mh",                                  # type of estimate
                t(rep(NA, 6)),                         # make all the counts and odds NAs
                epitable$massoc$OR.mh.wald,            # pull the mh OR and CIS
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
                            "exp_cases", "unexp_cases", "cases_odds",
                            "exp_controls", "unexp_controls", "controls_odds",
                            "est", "lower", "upper", "pval"
                            )

    }
  }

  # pull together a risk ratio table
  # note that this pulls the counts of cases and the total observations - for calculating RISKS

  if (measure == "RR") {

    # get a contingency table with lots of stats in it
    epitable <- epiR::epi.2by2(the_table, method = "cohort.count")

    # for non stratified results, simply pull together one liners
    if (length(strata_var) == 0) {
      # pull outputs together
      nums <- cbind(exposure_var,                   # name of the exposure variable
                    epitable$tab[1L, c(1L, 3L)],    # pull counts of cases on among exposed and total exposed
                    epitable$tab[1L, 4L],           # pull risk of being case among exposed (as proportion)
                    epitable$tab[2L, c(1L, 3L)],    # pull counts of cases on among exposed and total exposed
                    epitable$tab[2L, 4L],           # pull risk of being case among exposed (as proportion)
                    epitable$massoc$RR.strata.wald, # pull the the RR and CIs
                    epitable$massoc$chisq.strata[3] # pull the p-value
                    )

      colnames(nums) <- c("variable",
                          "exp_cases", "exp_total", "exp_risk",
                          "unexp_cases", "unexp_total", "unexp_risk",
                          "est", "lower", "upper", "pval"
      )
    }

    # if strata specified then pull together four rows (crude, strataTRUE, strataFALSE and MH estimates)
    if (length(strata_var) != 0) {

      # crude counts and estimates
      crude <- cbind(exposure_var,                   # name of the exposure variable
                     "crude",                        # type of estimate
                     epitable$tab[1L, c(1L, 3L)],    # pull counts of cases on among exposed and total exposed
                     epitable$tab[1L, 4L],           # pull risk of being case among exposed (as proportion)
                     epitable$tab[2L, c(1L, 3L)],    # pull counts of cases on among exposed and total exposed
                     epitable$tab[2L, 4L],           # pull risk of being case among exposed (as proportion)
                     epitable$massoc$RR.crude.wald, # pull the the RR and CIs
                     epitable$massoc$chisq.crude[3] # pull the p-value
      )

      # stratified counts and estimates
      stratified <- cbind(rep(exposure_var, 2),                # name of the exposure variable repeated for each strata
                          c("strata_TRUE", "strata_FALSE"),    # type of estimate
                          rbind(                               # row bind strata counts and risks together
                            cbind(                                  # cbind strata_true counts together seperately
                              the_table[TRUE][1L],                     # counts of cases among exposed
                              sum(the_table[TRUE][c(1L,3L)]),          # total exposed
                              the_table[TRUE][1L] /
                                sum(the_table[TRUE][c(1L,3L)]) * 100,  # risk among the exposed
                              the_table[TRUE][2L],                     # counts of cases among unexposed
                              sum(the_table[TRUE][c(2L,4L)]),          # total unexposed
                              the_table[TRUE][2L] /
                                sum(the_table[TRUE][c(2L,4L)]) * 100  # risk among the unexposed
                            ),
                            cbind(
                              the_table[TRUE][5L],                     # counts of cases among exposed
                              sum(the_table[TRUE][c(5L,7L)]),          # total exposed
                              the_table[TRUE][5L] /
                                sum(the_table[TRUE][c(5L,7L)]) * 100,  # risk among the exposed
                              the_table[TRUE][6L],                     # counts of cases among unexposed
                              sum(the_table[TRUE][c(6L,8L)]),          # total unexposed
                              the_table[TRUE][6L] /
                                sum(the_table[TRUE][c(6L,8L)]) * 100  # risk among the unexposed

                            )
                          ),
                          epitable$massoc$RR.strata.wald,       # pull the RR and CIs for each strata
                          data.frame(                           # pull the p-values for each strata as a dataframe
                            epitable$massoc$chisq.strata[,3]
                          )
                          )

      # mantel-haenszel counts (NAs) and estimates
      mh <- cbind(exposure_var,                          # name of exposure variable
                  "mh",                                  # type of estimate
                  t(rep(NA, 6)),                         # make all the counts and odds NAs
                  epitable$massoc$RR.mh.wald,            # pull the mh RR and CIS
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
                          "exp_cases", "exp_total", "exp_risk",
                          "unexp_cases", "unexp_total", "unexp_risk",
                          "est", "lower", "upper", "pval"
      )


    }

  }
  return(nums)
}

