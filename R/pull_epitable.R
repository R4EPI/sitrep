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
  res <- data.frame(
                    exp_cases      = character(1),
                    unexp_cases    = character(1),
                    cases_odds     = character(1),
                    exp_noncases   = character(1),
                    unexp_noncases = character(1),
                    noncases_odds  = character(1),
                    exp            = character(1),
                    lower          = character(1),
                    upper          = character(1),
                    pvalue         = character(1),
                    stringsAsFactors = FALSE
  )

  case    <- 1L # column for cases (Outcome +)
  noncase <- 2L # column for non-cases (Outcome -)
  odds    <- 5L # column for the odds ratio (Odds)

  exposed   <- 1L # exposed row (Exposed +)
  unexposed <- 2L # unexposed row (Exposed -)

  # cases
  res[["exp_cases"]]      <- epitable$tab[[case]][[exposed]]
  res[["unexp_cases"]]    <- epitable$tab[[case]][[unexposed]]
  res[["cases_odds"]]     <- epitable$tab[[odds]][[exposed]]

  # noncases
  res[["exp_noncases"]]   <- epitable$tab[[noncase]][[exposed]]
  res[["unexp_noncases"]] <- epitable$tab[[noncase]][[unexposed]]
  res[["noncases_odds"]]  <- epitable$tab[[odds]][[unexposed]]

  res[["exp"]]     <- epitable$massoc$OR.strata.wald[["exp"]]
  res[["lower"]]   <- epitable$massoc$OR.strata.wald[["lower"]]
  res[["upper"]]   <- epitable$massoc$OR.strata.wald[["upper"]]
  res[["p.value"]] <- epitable$massoc$chisq.strata[["p.value"]]

  res
}
