# This is supposed to 
widen_epitable <- function(epitable) {
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
