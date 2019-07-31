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
  if (length(strata_var) != 0 && !is.logical(x[[strata_var]])) {
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
  strata     <- if (length(strata_var) > 0) rlang::sym(strata_var) else NULL



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

    # if no stratifier then simple table
      if ((length(strata_var) == 0)) {
        # sum outcome and obstime by exposure
        the_table <- group_by(temp, {{exposure}})
        the_table <- summarise(the_table,
                            otcm = sum({{outcome}} == TRUE),
                            tme = sum({{perstime}}))

        # drop the first column and change to a table (for use in epi.2by2)
        the_table <- as.table(data.matrix(the_table[,2:3]))
      }

    # if stratifier specified then do by each group
      if ((length(strata_var) != 0)) {
        # sum outcome and obstime by exposure and strata
        temp_table <- group_by(temp, {{exposure}}, {{strata}})
        temp_table <- summarise(temp_table,
                               otcm = sum({{outcome}} == TRUE, na.rm = TRUE),
                               tme = sum({{perstime}}, na.rm = TRUE))

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
      }



    }





  ### Run the epiR::epi2by2 function on the counts data to get appropriate outputs

  ## pull together an Odds ratio table
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
    if (length(strata_var) != 0) {

        # crude counts and estimates
        crude <- cbind(exposure_var,                  # name of the exposure variable
                       "crude",                       # type of estimate
                       epitable$tab[1L, c(1L, 2L)],   # pull counts of exposed among cases (REMEMBER IS FLIPPED!)
                       epitable$tab[1L, 5L],          # pull odds of exposure among cases
                       epitable$tab[2L, c(1L, 2L)],   # pull counts of exposed among controls
                       epitable$tab[2L, 5L],          # pull odds of exposure among controls
                       epitable$massoc$OR.crude.wald, # pull the the OR and CIs
                       epitable$massoc$chisq.crude[3], # pull the p-value
                       NA                              # Leave space for wolf-test of homogeneity in strata rows
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
    if (length(strata_var) != 0) {

      # crude counts and estimates
      crude <- cbind(exposure_var,                   # name of the exposure variable
                     "crude",                        # type of estimate
                     epitable$tab[1L, c(1L, 3L)],    # pull counts of cases on among exposed and total exposed
                     epitable$tab[1L, 4L],           # pull risk of being case among exposed (as proportion)
                     epitable$tab[2L, c(1L, 3L)],    # pull counts of cases on among unexposed and total unexposed
                     epitable$tab[2L, 4L],           # pull risk of being case among unexposed (as proportion)
                     epitable$massoc$RR.crude.wald,  # pull the the RR and CIs
                     epitable$massoc$chisq.crude[3], # pull the p-value
                     NA                              # Leave space for wolf-test of homogeneity in strata rows
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
                            cbind(                                   # cbind strata_false counts together seperately
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
    epitable <- epiR::epi.2by2(the_table, method = "cohort.time")

    # for non stratified results, simply pull together one liners
    if (length(strata_var) == 0) {

      # pull outputs together
      nums <- cbind(exposure_var,                    # name of the exposure variable
                    epitable$tab[1L, 1L:3L],         # pull counts of cases on among exposed, total person-time among exp and incidence per 100 pers-time
                    epitable$tab[2L, 1L:3L],         # pull counts of cases on among unexposed, total person-time among unexp and incidence per 100 pers-time
                    epitable$massoc$IRR.strata.wald, # pull the the IRR and CIs
                    epitable$massoc$chisq.strata[3]  # pull the p-value
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
    if (length(strata_var) != 0) {

      # crude counts and estimates
      crude <- cbind(exposure_var,               # name of the exposure variable
                     "crude",                    # type of estimate
                     epitable$tab[1L, 1L:3L],    # pull counts of cases on among exposed, total person-time among exp and incidence per 100 pers-time
                     epitable$tab[2L, 1L:3L],    # pull counts of cases on among unexposed, total person-time among unexp and incidence per 100 pers-time
                     epitable$massoc$IRR.crude.wald, # pull the the IRR and CIs
                     epitable$massoc$chisq.crude[3] # pull the p-value
      )

      # stratified counts and estimates
      stratified <- cbind(rep(exposure_var, 2),                # name of the exposure variable repeated for each strata
                          c("strata_TRUE", "strata_FALSE"),    # type of estimate
                          rbind(                               # row bind strata counts and risks together
                            cbind(                                  # cbind strata_true counts together seperately
                              the_table[TRUE][1L],                     # counts of cases among exposed
                              the_table[TRUE][3L],                     # person time among exposed
                              the_table[TRUE][1L] /
                                the_table[TRUE][3L] * 100,             # incidence among the exposed
                              the_table[TRUE][2L],                     # counts of cases among unexposed
                              the_table[TRUE][4L],                     # person time among unexposed
                              the_table[TRUE][2L] /
                                the_table[TRUE][4L] * 100              # incidence among the unexposed
                            ),
                            cbind(                                  # cbind strata_false counts together seperately
                              the_table[TRUE][5L],                     # counts of cases among exposed
                              the_table[TRUE][7L],                     # person time among exposed
                              the_table[TRUE][5L] /
                                the_table[TRUE][7L] * 100,             # incidence among the exposed
                              the_table[TRUE][6L],                     # counts of cases among unexposed
                              the_table[TRUE][8L],                     # person time among unexposed
                              the_table[TRUE][6L] /
                                the_table[TRUE][8L] * 100              # incidence among the unexposed

                            )
                          ),
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


  # fix digits and turn things to numeric
  if (length(strata_var) == 0) {
    nums <- mutate_at(nums, vars(-variable), ~round(as.numeric(.), digits = digits))
  }
  if (length(strata_var) != 0) {
    nums <- mutate_at(nums, vars(-variable, -est_type), ~round(as.numeric(.), digits = digits))
  }

  # drop columns if specified
  # use numbers because names will be different according to measure, but place is always same
  if (!extend_output & length(strata_var) == 0) {
    nums <- select(nums, -4, -7, -11)
  }
  if (!extend_output & length(strata_var) != 0) {
    nums <- select(nums, -5, -8, -12)
  }

  # drop woolf-test pvalue
  if (length(strata_var != 0) & measure != "IRR" & woolf_test == FALSE) {
    nums <- select(nums, -woolf_pval)
  }

  # merge upper and lower CIs
  if (mergeCI) {
    nums <- unite_ci(nums, col = "est_ci", est, lower, upper, m100 = FALSE, digits = digits)
  }

  # change output table to a tibble
  # nums <- tibble(nums)

  # spit out the out table
  return(nums)
}

