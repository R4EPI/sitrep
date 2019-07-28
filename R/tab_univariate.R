#' Produce odds ratios, risk ratios or incidence rate ratios with accompanying confidence intervals
#' @param x A data frame
#' @param outcome Name of A TRUE/FALSE variable as your outcome of interest (e.g. illness)
#' @param ... Names of TRUE/FALSE variables as exposures of interest (e.g. risk factors)
#' @param perstime A numeric variable containing the observation time for each individual
#' @param measure Specify what you would like to calculated, options are "OR", "RR" or "IRR"
#' default is "OR"
#' @param extend_output TRUE/FALSE to specify whether would like all outputs in table (default is TRUE)
#' Non-extended output drops group odds/risk calculations as well as p-value
#' @param digits Specify number of decimal places (default is 3)
#' @param mergeCI Whether or not to put the confidence intervals in one column (default is FALSE)
#' @importFrom epiR epi.2by2
#' @importFrom dplyr select
#' @references Inspired by Daniel Gardiner,
#' see [github repo](https://github.com/DanielGardiner/UsefulFunctions/blob/efffde624d424d977651ed1a9ee4430cbf2b0d6f/single.variable.analysis.v0.3.R#L12)
#' @export

tab_univariate <- function(x, outcome, ..., perstime = NULL, measure = "OR", extend_output = TRUE,
                           digits = 3, mergeCI = FALSE) {


  ### Selecting variables
  # select the vars in the dots
  the_vars <- tidyselect::vars_select(colnames(x), ...)

  # select the var in the outcome column
  outcome_var <- tidyselect::vars_select(colnames(x), {{outcome}})

  # select the var in the perstime column
  perstime_var <- tidyselect::vars_select(colnames(x), {{perstime}})



  ### checks and messasges

  if (!is.data.frame(x)) {
    stop("x must be a data frame")
  }

  if (!is.logical(pull(x, {{outcome}}))) {
    stop("outcome must be a TRUE/FALSE variable")
  }

  # check for teach of the exposure variables that they are acceptable - NEEDS FIXING!!
  # if (!is.logical(the_vars)) {
  #   stop("this variable must be a TRUE/FALSE variable")
  # }

  if (is.null(perstime) & measure == "IRR") {
    stop("You have selected IRR as a measure but not specified a perstime variable.
         To calculate an incidence rate ratio please specify a variable which indicates
         observation time for each individual")
  }


  ### backing data and fixing variable levels
  # create a temp dataset so we dont overwrite the users dataset with factors
  temp <- x


  # swap the factor levels so TRUE comes first (required by epiR::epi2by2 function)
  temp <- mutate_at(temp, vars({{outcome}}, the_vars),
                 ~factor(., levels = c(TRUE, FALSE)))


  ### pulling together counts for to feed epi function

  # start an empty list for the counts tables
  count_list <- list()

  # for "OR" & "RR" get basic counts table to pass to the epiR::epi2by2 function
  if (measure != "IRR") {

    for (i in the_vars) {
      count_list[[i]] <- table(temp[[outcome_var]], temp[[i]])
    }
  }

  # for "IRR" return counts and person time by exposure - NEEDS FIXING!!
  # if (measure == "IRR") {
  #
  #   for (i in the_vars) {
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
  # note that for this it pulls the counts (not the totals) - for calculating Odds
  if (measure == "OR") {

    # run all the tables at same time
    epitable <- lapply(count_list, FUN = function(z) {
      epitable <- epiR::epi.2by2(z, method = "case.control")
    }
    )

    outies <- list()
    # pull outputs together
    for (i in names(count_list)) {
      nums <- cbind(i,
                    epitable[[z]]$tab[1L, c(1L, 2L)],   # pull outcomes among exposed
                    epitable[[z]]$tab[1L, 5L],          # pull odds among exposed
                    epitable[[z]]$tab[2L, c(1L, 2L)],   # pull outcomes among unexposed
                    epitable[[z]]$tab[2L, 5L],          # pull odds among exposed
                    epitable[[z]]$massoc$OR.strata.wald, # pull the the OR and CIs
                    epitable[[z]]$massoc$chisq.strata[3] # pull the p-value
                    )
      colnames(nums) <- c("variable",
                          "exp_cases", "unexp_cases", "cases_odds",
                          "exp_noncases", "unexp_noncases", "noncases_odds",
                          "exp", "lower", "upper", "pval"
                          )

      rownames(nums) <- i
      outies[[i]] <- nums
    }
    }



  outies <- bind_rows(outies)

  return(outies)
}
