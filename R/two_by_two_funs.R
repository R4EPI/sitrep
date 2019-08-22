#' calculate ratios (odds/risk) from a 2x2x2 table.
#'
#' @param x a 2x2x2 table with exposure in rows, outcome in columns, and strata in the third dimension
#' @param measure one of "OR", "RR", or "IRR"
#'
#' @return a data frame with crude and stratified ratios
#' @noRd
#'
#' @examples
#' 
#' arr <- c(10, 35, 90, 465, 36, 25, 164, 175)
#' arr <- array(arr, dim = c(2, 2, 2),
#'              dimnames = list(risk = c(TRUE, FALSE),
#'                              outcome = c(TRUE, FALSE),
#'                              old = c(FALSE, TRUE))
#'        )
#' strata_ratio_table(arr)
strata_ratio_table <- function(x, measure = "OR") {

  d <- data_frame_from_2x2(x)

  if (measure == "OR") {

    data.frame(
      exp_cases      = d$A_exp_cases,
      unexp_cases    = d$C_unexp_cases,
      cases_odds     = d$A_exp_cases / d$C_unexp_cases,

      exp_controls   = d$B_exp_controls,
      unexp_controls = d$D_unexp_controls,
      controls_odds  = d$B_exp_controls / d$D_unexp_controls
    )

  } else if (measure == "RR") {
    # DIM 1: exposure 
    # DIM 2: outcome (case, control)
    # DIM 3: strata 

    data.frame(
      exp_cases   = d$A_exp_cases,
      exp_total   = d$total_exposed,
      exp_risk    = (d$A_exp_cases / d$total_exposed) * 100,

      unexp_cases = d$C_unexp_cases,
      unexp_total = d$total_unexposed,
      unexp_risk  = (d$C_unexp_cases / d$total_unexposed) * 100
    )

  } else if (measure == "IRR") {
    # DIM 1: exposure 
    # DIM 2: outcome (outcome, person time)
    # DIM 3: strata 
    data.frame(
      exp_cases       = d$A_exp_cases,
      exp_perstime    = d$B_exp_controls,
      exp_incidence   = (d$A_exp_cases / d$B_exp_controls) * 100,

      unexp_cases     = d$C_unexp_cases,
      unexp_perstime  = d$D_unexp_controls,
      unexp_incidence = (d$C_unexp_cases / d$D_unexp_controls) * 100
    )

  } else {
    stop(glue::glue("the measure {measure} is not recognised"), call. = FALSE)
  }
}

#' get ratio estimates from a 2x2x2 array
#'
#' @param x a 2x2x2 array
#' @param measure one of OR, RR, or IRR
#' @param conf the confidence level
#'
#' @return a matrix with three columns, ratio, lower, and upper
#' @noRd
#'
#' @examples
#' arr <- c(10, 35, 90, 465, 36, 25, 164, 175)
#' arr <- array(arr, dim = c(2, 2, 2),
#'              dimnames = list(risk = c(TRUE, FALSE),
#'                              outcome = c(TRUE, FALSE),
#'                              old = c(FALSE, TRUE))
#'        )
#' get_ratio_est(arr, "OR") 
#' get_ratio_est(arr, "RR") 
#' get_ratio_est(arr, "IRR")
#'
get_ratio_est <- function(x, measure = "OR", conf = 0.95, strata_name = "strata") {


  d  <- data_frame_from_2x2(x)
  CS <- get_chisq_pval(x)
  has_strata <- nrow(d) > 1
  n  <- 1L
  
  rnames <- "crude"

  if (has_strata) {
    n <- n + dim(x)[[3]] + 1L 
    rnames <- c(rnames, strata = glue::glue("{strata_name}: {dimnames(x)[[3]]}"), "MH")
    if (measure != "IRR") {
      n     <- n + 1L
      woolf <- get_woolf_pval(x, measure)
      rnames <- c(rnames, "woolf")
    } else {
      woolf <- NULL
    }
    MH <- get_mh(x, measure, conf)
  } else {
    MH       <- NULL
    woolf    <- NULL
  }

  # Risk ratio:
  # A / (A + B)      exposed cases / all cases
  # ----------- = ------------------------------
  # C / (C + D)   exposed controls / all controls
  ratio   <- switch(measure, 
                    RR = ratio_est(d$A_exp_cases, 
                                   d$total_exposed,
                                   d$C_unexp_cases,
                                   d$total_unexposed,
                                   measure = "RR",
                                   conf = conf),
  # Odds ratio:
  # A / B      exposed cases / unexposed cases
  # ----- = -------------------------------------
  # C / D   exposed controls / unexposed controls

                    OR = ratio_est(d$A_exp_cases, 
                                   d$C_unexp_cases,
                                   d$B_exp_controls, 
                                   d$D_unexp_controls,
                                   measure = "OR",
                                   conf = conf),

  # Incidence Rate Ratio:
  # A / B      exposed cases / exposed person-time
  # ----- = ----------------------------------------
  # C / D   exposed controls / unexposed person-time
                    IRR = ratio_est(d$A_exp_cases, 
                                    d$B_exp_controls, 
                                    d$C_unexp_cases,
                                    d$D_unexp_controls,
                                    measure = "IRR",
                                    conf = conf),

  )


  if (has_strata) {
    
    res <- data.frame(
      ratio   = numeric(n),
      lower   = numeric(n),
      upper   = numeric(n),
      p.value = numeric(n)
    )

    na <- function(x, m = measure) if (is.null(x) && m != "IRR") NA else x

    res$ratio   <- c(ratio[["ratio"]], MH[["ratio"]], na(woolf[["ratio"]]))
    res$lower   <- c(ratio[["lower"]], MH[["lower"]], na(woolf[["lower"]]))
    res$upper   <- c(ratio[["upper"]], MH[["upper"]], na(woolf[["upper"]]))
    res$p.value <- c(CS[["p.value"]],  NA_real_     , na(woolf[["p.value"]]))
    
  } else {
    
    ratio$p.value <- CS$p.value
    res           <- ratio
    
  }
  rownames(res) <- rnames
  res
  
}

#' get chi square p-value from a 2D or 3D array
#'
#' @param x a 2x2 or 2x2x2 table
#'
#' @return a 1 or 3x2 matrix with chi square statistics and p-values
#' @noRd
#'
#' @examples
get_chisq_pval <- function(x) {
  ndim   <- length(dim(x))
  n      <- if (ndim == 3L) dim(x)[ndim] + 1L else 1L
  rnames <- if (n == 3L) dimnames(x)[[3]] else NULL

  # res <- matrix(ncol = 2, nrow = n,
  #               dimnames = list(c("crude", rnames),
  #                               c("statistic", "p.value")))
  res <- data.frame(statistic = numeric(n), p.value = numeric(n))

  # No matter the dimensions, we can always get the crude p-value 
  crude <- stats::chisq.test(apply(x, MARGIN = 1:2, FUN = sum), correct = FALSE)

  res$statistic[1] <- crude$statistic
  res$p.value[1]   <- crude$p.value

  # if there are strata, we can apply over that dimension and get the statistic
  # for each. 
  if (ndim == 3) {
    strat <- apply(x, MARGIN = 3, FUN = chisq.test, correct = FALSE)
    for (i in seq_along(strat)) {
      res$statistic[i + 1L] <- strat[[i]]$statistic
      res$p.value[i + 1L]   <- strat[[i]]$p.value
    }
  }
  res
}

#' calculate ratio estimates for odds, risk, and incidence rate ratios
#'
#' @param N1 first numerator
#' @param D1 first denominator
#' @param N2 second numerator
#' @param D2 second denominator
#' @param measure one of "OR", "RR", or "IRR"
#' @param conf confidence level
#'
#' @return a vector containing the ratio estimate, lower bound, and upper bound
#' 
#' @note This was adapted from the code in the epiR::epi.2by2 version 1.0-2. 
#'
#' OR is from .funORwald (294--308)
#' RR is from .funRRwald (156--171)
#' IRR is from lines 787--797 (no specific function)
#' 
#' @details This function produces odds ratio, risk ratio, and incidence rate
#' ratio estimates with confidence intervals. I have generalized the methods to 
#' use as little code duplication as possible. 
#'
#' @noRd
ratio_est <- function(N1, D1, N2, D2, measure = "OR", conf = 0.95) {

  z <- get_z(conf)

  # inverse numbers for the harmonic
  iN1 <- 1 / N1
  iD1 <- 1 / D1
  iN2 <- 1 / N2
  iD2 <- 1 / D2

  case      <-   N1 / D1
  control   <-   N2 / D2
  ratio     <- case / control
  log_ratio <- log(ratio)
  
  log_ratio_var <- switch(measure,
      RR = (iN1 - iD1) + (iN2 - iD2),
      OR = (iN1 + iD1) + (iN2 + iD2),
      IRR = iN1 + iN2
  )

  if (measure == "IRR") {

    alpha       <- 1 - ((1 - conf) / 2)

    N1_quantile <- 1 / qf(p = 1 - alpha,
                          df1 = 2 * N1,
                          df2 = 2 * N2 + 2)

    N2_quantile <- 1 / qf(p = 1 - alpha,
                          df1 = 2 * N2,
                          df2 = 2 * N1 + 2)

    pl <-       N1 / ( N1      + ((N2 + 1) * N1_quantile))
    ph <- (N1 + 1) / ((N1 + 1) + (N2 / N2_quantile))

    lower_limit <- pl * D2 / ((1 - pl) * D1)
    upper_limit <- ph * D2 / ((1 - ph) * D1)

  } else {

    se_limits   <- get_ci_from_var(log_ratio, log_ratio_var, z)
    lower_limit <- se_limits[["ll"]]
    upper_limit <- se_limits[["ul"]]

  }


  data.frame(ratio, lower = lower_limit, upper = upper_limit)

}

get_z <- function(conf) {
  alpha <- 1 - ((1 - conf) / 2)
  z     <- qnorm(alpha, mean = 0, sd = 1)
  z
}

get_ci_from_var <- function(log_ratio, log_ratio_var, z) {

  log_ratio_se  <- sqrt(log_ratio_var)
  lower_limit   <- exp(log_ratio - (z * log_ratio_se))
  upper_limit   <- exp(log_ratio + (z * log_ratio_se))

  data.frame(se = log_ratio_se, ll = lower_limit, ul = upper_limit)

}

# Mantel-Haenszel tests (no p-values)

get_mh <- function(arr, measure = "OR", conf = 0.95) {

  switch(measure,
         OR  = mh_or(arr,  conf),
         RR  = mh_rr(arr,  conf),
         IRR = mh_irr(arr, conf)
  )

}


# These functions are adapted from epiR::epi.2by2 lines 1185--1204
mh_rr <- function(arr, conf = 0.95) {

  d <- data_frame_from_2x2(arr)[-1, ] # remove crude estimates
  z <- get_z(conf)

  ## Summary incidence risk ratio (Rothman 2002 p 148 and 152, equation 8-2):
  A    <- d$A_exp_cases
  C    <- d$C_unexp_cases
  N    <- d$total
  A_TU <- A * d$total_unexposed
  C_TE <- C * d$total_exposed

  MH_risk_ratio <- sum(A_TU / N) / sum(C_TE / N)

  total_prod      <- d$total_cases * d$total_exposed * d$total_unexposed
  var_numerator   <- sum((total_prod / N^2) - ((A * C)/ N))
  var_denominator <- sum(A_TU / N) * sum(C_TE / N)

  MH_risk_ratio_var <- var_numerator / var_denominator
    
  se_limits <- get_ci_from_var(log(MH_risk_ratio), MH_risk_ratio_var, z)

  data.frame(ratio = MH_risk_ratio, lower = se_limits[["ll"]], upper = se_limits[["ul"]])
  
}

mh_irr <- function(arr, conf = 0.95) {
  
  d <- data_frame_from_2x2(arr)[-1, ]
  ## Summary incidence rate ratio (Rothman 2002 p 153, equation 8-5):
  A     <- d$A_exp_cases
  B     <- d$B_exp_controls
  C     <- d$C_unexp_cases
  D     <- d$D_unexp_controls
  cases <- d$total_cases # M1

  person_time <- d$total_controls #M0

  numerator   <- sum((A * D) / person_time)
  denominator <- sum((C * B) / person_time)
  MH_IRR      <- numerator / denominator

  MH_IRR_var <- sum((cases * B * D) / (person_time^2)) / (numerator * denominator)

  se_limits <- get_ci_from_var(log(MH_IRR), MH_IRR_var, get_z(conf))

  data.frame(ratio = MH_IRR, lower = se_limits[["ll"]], upper = se_limits[["ul"]])
}

# The M-H statistic for odds ratios already exist in R, so it's just a matter of
# pulling out the correct values
mh_or <- function(arr, conf = 0.95) {

  MH <- mantelhaen.test(arr, conf.level = conf)
  data.frame(ratio = MH$estimate, lower = MH$conf.int[1], upper = MH$conf.int[2])

}


get_woolf_pval <- function(x, measure = "OR") {

  d       <- data_frame_from_2x2(x)[-1, ]
  nstrata <- dim(x)[[3]]

  A         <- d$A_exp_cases
  B         <- d$B_exp_controls
  C         <- d$C_unexp_cases
  D         <- d$D_unexp_controls

  exposed   <- d$total_exposed
  unexposed <- d$total_unexposed

  # This is taken from lines 1320--1340 of epiR::epi.2by2
  # 
  # I have isolated the necessary elements and combined them into one function
  if (measure == "RR") {
    ## Test of homogeneity of risk ratios (Jewell 2004, page 154). First work
    ## out the Woolf estimate of the adjusted risk ratio (labelled adj_log_est
    ## here) based on Jewell (2004, page 134):

    log_est         <- log((A / exposed) / (C / unexposed))
    log_est_var     <- (B / (A * exposed)) + (D / (C * unexposed))

  } else {
    ## Test of homogeneity of odds ratios (Jewell 2004, page 154). First work
    ## out the Woolf estimate of the adjusted odds ratio (labelled adj_log_est
    ## here) based on Jewell (2004, page 129):
    log_est     <- log(((A + 0.5) * (D + 0.5)) / ((B + 0.5) * (C + 0.5)))
    log_est_var <- (1 / (A + 0.5)) + (1 / (B + 0.5)) + (1 / (C + 0.5)) + (1 / (D + 0.5))
  }

  inv_log_est_var <- 1 / log_est_var
  adj_log_est     <- sum(inv_log_est_var * log_est) / sum(inv_log_est_var)

  ## Equation 10.3 from Jewell (2004):
  res     <- sum(inv_log_est_var * (log_est - adj_log_est)^2)
  p_value <- 1 - stats::pchisq(res, df = nstrata - 1)

  data.frame(statistic = res, df = nstrata - 1, p.value = p_value)
}
