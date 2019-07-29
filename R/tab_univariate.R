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
#' a <- tibble(potato = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            apple = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            orange = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            stratifier = sample(c(TRUE, FALSE), 2000, replace = TRUE),
#'            perstime = sample(150:250, 2000, replace = TRUE)
#'            )

tab_univariate <- function(x, outcome, var, perstime = NULL, strata = NULL, measure = "OR", extend_output = TRUE,
                           digits = 3, mergeCI = FALSE) {


  ### Selecting variables
  # select the vars in the dots
  the_var <- tidyselect::vars_select(colnames(x), {{var}})

  # select the var in the outcome column
  outcome_var <- tidyselect::vars_select(colnames(x), {{outcome}})

  # select the var in the perstime column
  perstime_var <- tidyselect::vars_select(colnames(x), {{perstime}})


  # select the var in the strata column
  strata_var <- tidyselect::vars_select(colnames(x), {{strata}})


  ### checks and messasges

  if (!is.data.frame(x)) {
    stop("x must be a data frame")
  }

  if (!is.logical(pull(x, {{outcome}}))) {
    stop("outcome must be a TRUE/FALSE variable")
  }

  # check for teach of the exposure variables that they are acceptable - NEEDS FIXING!!
  # if (!is.logical(the_var)) {
  #   stop("this variable must be a TRUE/FALSE variable")
  # }

  # if (is.null(perstime) & measure == "IRR") {
  #   stop("You have selected IRR as a measure but not specified a perstime variable.
  #        To calculate an incidence rate ratio please specify a variable which indicates
  #        observation time for each individual")
  # }


  ### backing data and fixing variable levels
  # create a temp dataset so we dont overwrite the users dataset with factors
  temp <- x


  # swap the factor levels so TRUE comes first (required by epiR::epi2by2 function)
  temp <- mutate_at(temp, vars({{outcome}}, the_var),
                 ~factor(., levels = c(TRUE, FALSE)))

  # swap factor levels for strata if not null
  if (!is.null(strata)) {
    temp <- mutate_at(temp, vars({{strata}}),
                      ~factor(., levels = c(TRUE, FALSE)))
  }


  ### pulling together counts for to feed epi function

  # for "OR" get basic counts table to pass to the epiR::epi2by2 function
  # NOTE that this is flipped the wrong way (cases/controls as rows) - because epi2by2 calculates ORs WRONG!!
  if (measure == "OR") {

    the_table <- table(temp[c(outcome_var, the_var, strata_var)])

  }

  # for "RR" get basic counts table to pass to the epiR::epi2by2 function
  # NOTE for RRs inputting with cases as columns is fine for using epi2by2
  if (measure == "RR") {

    the_table <- table(temp[c(the_var, outcome_var, strata_var)])

  }





  # for "IRR" return counts and person time by exposure - NEEDS FIXING!!
  # if (measure == "IRR") {
  #
  #   for (i in the_var) {
  #
  #     # sum outcome and obstime by exposure
  #     counts <- group_by(temp, .data[[i]])
  #     counts <- summarise(counts,
  #                         otcm = sum(.data[[i]]),
  #                         tme = sum(.data[[perstime_var]]))
  #
  #     count_list[[i]] <- as.table(data.matrix(counts[,2:3]))
  #
  #   }
  # }


  ### Run the epiR::epi2by2 function on the counts data to get appropriate outputs

  # pull together an Odds ratio table
  # note that for this it pulls the counts by outcome group (not the totals by group) - for calculating ODDS
  if (measure == "OR") {

    epitable <- epiR::epi.2by2(the_table, method = "case.control")

    # start an empty list to save the output tables in to


    # for non stratified results, simply pull together one liners
    if (is.null(strata)) {
      # pull outputs together
      nums <- cbind(the_var,                        # name of the exposure variable
                    epitable$tab[1L, c(1L, 2L)],    # pull counts of exposed among cases (REMEMBER IS FLIPPED!)
                    epitable$tab[1L, 5L],           # pull odds of exposure among cases
                    epitable$tab[2L, c(1L, 2L)],    # pull counts of exposed among among controls
                    epitable$tab[2L, 5L],           # pull odds of exposure among controls
                    epitable$massoc$OR.strata.wald, # pull the the OR and CIs
                    epitable$massoc$chisq.strata[3] # pull the p-value
      )
      colnames(nums) <- c("variable",
                          "exp_cases", "unexp_cases", "cases_odds",
                          "exp_noncases", "unexp_noncases", "noncases_odds",
                          "exp", "lower", "upper", "pval"
      )

      # store in out put list
      outies <- nums
    }

    # if strata specified then pull together four rows (crude, strataTRUE, strataFALSE and MH estimates)

    if (!is.null(strata)) {
      # pull outputs together
        # crude counts and estimates
        crude <- cbind(the_var,                            # name of the exposure variable
                       "crude",                            # type of estimate
                       epitable$tab[1L, c(1L, 2L)],   # pull counts of exposed among cases (REMEMBER IS FLIPPED!)
                       epitable$tab[1L, 5L],          # pull odds of exposure among cases
                       epitable$tab[2L, c(1L, 2L)],   # pull counts of exposed among among controls
                       epitable$tab[2L, 5L],          # pull odds of exposure among controls
                       epitable$massoc$OR.crude.wald, # pull the the OR and CIs
                       epitable$massoc$chisq.crude[3] # pull the p-value
                       )

        # stratified counts and estimates
        stratified <- cbind(rep(the_var, 2),                           # name of the exposure variable repeated for each strata
                c("strata_TRUE", "strata_FALSE"),    # type of estimate
                rbind(                               # row bind strata outcomes together
                  cbind(                                  # cbind strata_true outcomes counts together seperately
                    t(the_table[TRUE][c(1,3)]),        # pull outcomes among exposed
                    the_table[TRUE][1] /
                      the_table[TRUE][3],           # calculate odds among exposed
                    t(the_table[TRUE][c(2,4)]),        # pull outcomes among unexposed
                    the_table[TRUE][2] /
                      the_table[TRUE][4]          # calculate odds among unexposed
                    ),
                  cbind(                                  # cbind strata_false outcomes counts together seperately
                    t(the_table[TRUE][c(5,7)]),        # pull outcomes among exposed
                    the_table[TRUE][5] /
                      the_table[TRUE][7],           # calculate odds among exposed
                    t(the_table[TRUE][c(6,8)]),        # pull outcomes among unexposed
                    the_table[TRUE][6] /
                      the_table[TRUE][8]           # calculate odds among unexposed
                  )
                  ),
                epitable$massoc$OR.strata.wald,  # pull the OR and CIs for each strata
                data.frame(                           # pull the p-values for each strata as a dataframe
                  epitable$massoc$chisq.strata[,3]
                  )
                )


          # mantel-haenszel counts (NAs) and estimates
          mh <- cbind(the_var,                                     # name of exposure variable
                "mh",                                  # type of estimate
                t(rep(NA, 6)),                            # make all the counts and odds NAs
                epitable$massoc$OR.mh.wald,       # pull the mh OR and CIS
                epitable$massoc$chisq.mh$p.value # pull the mh pvalue
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
                            "exp_noncases", "unexp_noncases", "noncases_odds",
                            "exp", "lower", "upper", "pval"
                            )

        # store in out put list
        outies <- nums

    }


  }
  if (measure == "RR") {

  }

  return(outies)
}
