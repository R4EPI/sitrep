# This is supposed to
# Odds Ratios, no strata:
#
# nums <- cbind(exposure_var,                   # name of the exposure variable
#               epitable$tab[1L, c(1L, 2L)],    # pull counts of exposed among cases (REMEMBER IS FLIPPED!)
#               epitable$tab[1L, 5L],           # pull odds of exposure among cases
#               epitable$tab[2L, c(1L, 2L)],    # pull counts of exposed among among controls
#               epitable$tab[2L, 5L],           # pull odds of exposure among controls
#               epitable$massoc$OR.strata.wald, # pull the the OR and CIs
#               epitable$massoc$chisq.strata[3] # pull the p-value
# )
# # set correct column names
# colnames(nums) <- c("variable",
#                     "exp_cases", "unexp_cases", "cases_odds",
#                     "exp_controls", "unexp_controls", "controls_odds",
#                     "est", "lower", "upper", "pval"
# )
#
# Odds Ratios with strata:
#
#                epitable$tab[1L, c(1L, 2L)],   # pull counts of exposed among cases (REMEMBER IS FLIPPED!)
#                epitable$tab[1L, 5L],          # pull odds of exposure among cases
#                epitable$tab[2L, c(1L, 2L)],   # pull counts of exposed among controls
#                epitable$tab[2L, 5L],          # pull odds of exposure among controls
#                epitable$massoc$OR.crude.wald, # pull the the OR and CIs
#                epitable$massoc$chisq.crude[3], # pull the p-value
#                NA                              # Leave space for wolf-test of homogeneity in strata rows
#                )

# # stratified counts and estimates
# stratified <- cbind(rep(exposure_var, 2),    # name of the exposure variable repeated for each strata
#         c("strata_TRUE", "strata_FALSE"),    # type of estimate
#         rbind(                               # row bind strata counts and odds together
#           cbind(                                  # cbind strata_true counts together seperately
#             t(the_table[TRUE][c(1,3)]),             # pull counts of exposed among cases
#             the_table[TRUE][1] /
#               the_table[TRUE][3],                   # calculate odds of exposure among cases
#             t(the_table[TRUE][c(2,4)]),             # pull counts of exposed among controls
#             the_table[TRUE][2] /
#               the_table[TRUE][4]                    # calculate odds of exposure among cases
#             ),
#           cbind(                                  # cbind strata_false outcomes counts together seperately
#             t(the_table[TRUE][c(5,7)]),             # pull counts of exposure among cases
#             the_table[TRUE][5] /
#               the_table[TRUE][7],                   # calculate odds of exposure among cases
#             t(the_table[TRUE][c(6,8)]),             # pull counts of exposure among controls
#             the_table[TRUE][6] /
#               the_table[TRUE][8]                    # calculate odds of exposure among controls
#           )
#           ),
#         epitable$massoc$OR.strata.wald,       # pull the OR and CIs for each strata
#         data.frame(                           # pull the p-values for each strata as a dataframe
#           epitable$massoc$chisq.strata[,3]
#           ),
#         rbind(epitable$massoc$OR.homog.woolf[,3], NA)    # pull the woolf test of homogeneity p-value
#         )


#   # mantel-haenszel counts (NAs) and estimates
#   mh <- cbind(exposure_var,                    # name of exposure variable
#         "mh",                                  # type of estimate
#         t(rep(NA, 6)),                         # make all the counts and odds NAs
#         epitable$massoc$OR.mh.wald,            # pull the mh OR and CIS
#         epitable$massoc$chisq.mh$p.value,      # pull the mh pvalue
#         NA                                     # Leave space for wolf-test of homogeneity in strata rows
#         )


or_names <- function() {
  c("variable",

    "exp_cases",
    "unexp_cases",
    "cases_odds",

    "exp_controls",
    "unexp_controls",
    "controls_odds",

    "est",
    "lower",
    "upper",
    "pval")
}

rr_names <- function() {
  c("variable",

    "exp_cases",
    "exp_total",
    "exp_risk",

    "unexp_cases",
    "unexp_total",
    "unexp_risk",

    "est",
    "lower",
    "upper",
    "pval")
}

irr_names <- function() {
  c("variable",

    "exp_cases",
    "exp_perstime",
    "exp_incidence",

    "unexp_cases",
    "unexp_perstime",
    "unexp_incidence",

    "est",
    "lower",
    "upper",
    "pval")
}

get_epitable_ci <- function(epitable, measure = "OR") {

  res <- data.frame(
                    exp    = numeric(1),
                    lower  = numeric(1),
                    upper  = numeric(1),
                    pvalue = numeric(1),
                   )
  var              <- glue::glue("{measure}.strata.wald")
  res[["exp"]]     <- epitable$massoc[[measure]][["exp"]]
  res[["lower"]]   <- epitable$massoc[[measure]][["lower"]]
  res[["upper"]]   <- epitable$massoc[[measure]][["upper"]]
  res[["p.value"]] <- epitable$massoc$chisq.strata[["p.value"]]
  res
}

get_epitable_values <- function(epitable, measure = "OR") {

  res        <- vector(mode = "character", length = 6L)
  names(res) <- 1:6
  res        <- as.data.frame(as.list(res), stringsAsFactors = FALSE)

  # row indices for extraction. These will always be the same.
  exposed   <- 1L # exposed row (Exposed +)
  unexposed <- 2L # unexposed row (Exposed -)

  # Setting up the columns for extraction. This is weird because the positions
  # of the values we need jump around based on the statistic. I pieced this
  # together from Alex's original code. It appears that the columns for the
  # case/noncase and ratio measure differ

  case <- 1L # column for cases (Outcome +)

  if (measure == "OR") {
    noncase   <- 2L # column for non-cases (Outcome -)
    the_ratio <- 5L # column for the odds ratio (Odds)
  } else if (measure == "RR") {
    noncase   <- 3L # column for non-cases (Outcome -)
    the_ratio <- 4L # column for the risk ratio
  } else if (measure == "IRR") {
    noncase   <- 2L 
    the_ratio <- 3L
  } else {
    stop(glue::glue("the measure {measure} is not recognised"), call. = FALSE)
  }

  # cases
  res[[1L]] <- epitable$tab[[case]][[exposed]]
  res[[2L]] <- epitable$tab[[case]][[unexposed]]
  res[[3L]] <- epitable$tab[[the_ratio]][[exposed]]

  # noncases
  res[[4L]] <- epitable$tab[[noncase]][[exposed]]
  res[[5L]] <- epitable$tab[[noncase]][[unexposed]]
  res[[6L]] <- epitable$tab[[the_ratio]][[unexposed]]

  res
}
