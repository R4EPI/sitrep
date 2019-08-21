summarize_epitable <- function(epitable, contingency_table, exposure_var = NULL, measure = "OR", has_strata = FALSE, strata_var = NULL) {

  
  # if strata specified then pull together four rows (crude, strataTRUE, strataFALSE and MH estimates)
  if (has_strata) {

    # crude counts and estimates
    crude <- cbind(exposure_var,                  # name of the exposure variable
                   "crude",                       # type of estimate
                   get_epitable_values(epitable, measure), # extract the values from the table
                   get_epitable_ci(epitable, measure, "crude"),
                   NA                              # Leave space for wolf-test of homogeneity in strata rows
    )

    # stratified counts and estimates
    stratified <- cbind(rep(exposure_var, 2),    # name of the exposure variable repeated for each strata
                        paste(strata_var, c("TRUE", "FALSE")),
                        strata_ratio_table(contingency_table, measure),
                        get_epitable_ci(epitable, measure, "strata"),
                        rbind(epitable$massoc$RR.homog.woolf[,3], NA)    # pull the woolf test of homogeneity p-value
    )


    # mantel-haenszel counts (NAs) and estimates
    mh <- cbind(exposure_var,                    # name of exposure variable
                "mh",                                  # type of estimate
                t(rep(NA, 6)),                         # make all the counts and odds NAs
                get_epitable_ci(epitable, measure, "mh"),
                NA                                     # Leave space for wolf-test of homogeneity in strata rows
    )

    # remove all the colnames and column names
    colnames(crude)      <- NA
    colnames(stratified) <- NA
    colnames(mh)         <- NA
    rownames(crude)      <- NULL
    rownames(stratified) <- NULL
    rownames(mh)         <- NULL


    # bind the four rows together (each cbinded together seperately below)
    nums <- rbind(crude, stratified, mh)


  } else {
    # for non stratified results, simply pull together one liners
    # pull outputs together
    nums <- cbind(exposure_var,                           # name of the exposure variable
                  get_epitable_values(epitable, measure), # extract the values from the table
                  get_epitable_ci(epitable, measure)      # pull the estimate, CIs, and p-value
    )
  }

                get_ratio_est(contingency_table, measure),
                get_chisq_pval(arr)[, 2, drop = FALSE]
                )

  vars <- list(
               OR  = c("exp_cases",    "unexp_cases",    "cases_odds",
                       "exp_controls", "unexp_controls", "controls_odds"),
               RR  = c("exp_cases",    "exp_total",      "exp_risk",
                       "unexp_cases",  "unexp_total",    "unexp_risk"),
               IRR = c("exp_cases",    "exp_perstime",   "exp_incidence",
                       "unexp_cases",  "unexp_perstime", "unexp_incidence"),
               NULL
  )
  colnames(nums) <- c("variable",
                      "est_type",
                      vars[[measure]], 
                      "est", "lower", "upper", "pval", 
                      if (has_strata) "woolf_pval" else NULL
  )
  rownames(nums) <- NULL
  nums
}

get_epitable_ci <- function(epitable, measure = "OR", type = "strata") {

  var <- glue::glue("{measure}.{type}.wald")
  chi <- glue::glue("chisq.{type}")
  tab <- summary(epitable)
  n   <- nrow(tab[[var]])
  res <- data.frame(
                    est     = numeric(n),
                    lower   = numeric(n),
                    upper   = numeric(n),
                    p.value = numeric(n)
                   )
  res[["est"]]     <- tab[[var]][["est"]]
  res[["lower"]]   <- tab[[var]][["lower"]]
  res[["upper"]]   <- tab[[var]][["upper"]]
  res[["p.value"]] <- tab[[chi]][["p.value"]]
  res
}


# get odds of exposure among cases
get_expo_cases_odds <- function(x, measure = "RR") { 

  if (measure == "OR") {
    A <- x[1, 1, ]
    B <- x[1, 2, ]
    C <- x[2, 1, ]
    D <- x[2, 2, ] 
  } else {
    A <- x[1, 1, ]
    B <- x[2, 1, ]
    C <- x[1, 2, ]
    D <- x[2, 2, ] 
  }
  N <- sum(x)

  exposed_cases_odds    <- c(sum(A)/sum(B), A / B)
  exposed_noncases_odds <- c(sum(C)/sum(D), C / D)


  # sigma(a_i * d_i / n_i) / sigma(b_i * c_i / n_i) where n_i is the sum of respective strata
  MH <- mantelhaen.test(x)

  list( 
       odds_ratios = data.frame(exposed_cases= exposed_cases_odds,
                           exposed_non_cases = exposed_noncases_odds,
                           odds_ratio        = exposed_cases_odds / exposed_noncases_odds,
                           stringsAsFactors  = FALSE
                           ),
       MH = data.frame(est     = MH$estimate,
                       lower   = MH$conf.int[1],
                       upper   = MH$conf.int[2],
                       p.value = MH$p.value
       )
  )
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

