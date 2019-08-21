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
get_ratio_est <- function(x, measure = "OR", conf = 0.95) {


  d  <- data_frame_from_2x2(x)
  CS <- get_chisq_pval(x)[, 2, drop = FALSE]
  MH <- mantelhaen.test(x)
  MH <- data.frame(ratio   = MH$estimate,
                   lower   = MH$conf.int[1],
                   upper   = MH$conf.int[2],
                   p.value = MH$p.value
  )

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

  ratio           <- cbind(ratio, p.value = CS)
  ratio           <- rbind(ratio, MH)
  rownames(ratio) <- c(rownames(d), "MH")
  ratio
  
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

  res <- matrix(ncol = 2, nrow = n,
                dimnames = list(c("crude", rnames),
                                c("statistic", "p.value")))

  # No matter the dimensions, we can always get the crude p-value 
  crude          <- stats::chisq.test(apply(x, MARGIN = 1:2, FUN = sum), correct = FALSE)
  res["crude", ] <- c(crude$statistic, crude$p.value)

  # if there are strata, we can apply over that dimension and get the statistic
  # for each. 
  if (ndim == 3) {
    strat <- apply(x, MARGIN = 3, FUN = chisq.test, correct = FALSE)
    for (i in seq_along(strat)) {
      res[i + 1, ] <- c(strat[[i]]$statistic, strat[[i]]$p.value)
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

  alpha <- 1 - ((1 - conf) / 2)
  z     <- qnorm(alpha, mean = 0, sd = 1)

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

  log_ratio_se  <- sqrt(log_ratio_var)

  if (measure == "IRR") {
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
    lower_limit   <- exp(log_ratio - (z * log_ratio_se))
    upper_limit   <- exp(log_ratio + (z * log_ratio_se))
  }

  matrix(c(ratio, lower_limit, upper_limit), 
         ncol = 3, 
         dimnames = list(NULL, c("ratio", "lower", "upper")))

}


pwoolf <- function(x, measure = "OR") {
  # This is taken from lines 1320--1340 of epiR::epi.2by2
  #
  ## Test of homogeneity of risk ratios (Jewell 2004, page 154). First work out the Woolf estimate of the adjusted risk ratio (labelled lnRR.s. here) based on Jewell (2004, page 134):
  lnRR. <- log((a / (a + b)) / (c / (c + d)))
  lnRR.var. <- (b / (a * (a + b))) + (d / (c * (c + d)))
  wRR. <- 1 / lnRR.var.
  lnRR.s. <- sum(wRR. * lnRR.) / sum(wRR.)

  ## Equation 10.3 from Jewell (2004):
  RR.homogeneity <- sum(wRR. * (lnRR. - lnRR.s.)^2)
  RR.homogeneity.p <- 1 - pchisq(RR.homogeneity, df = n.strata - 1)
  RR.homog <- data.frame(test.statistic = RR.homogeneity, df = n.strata - 1, p.value = RR.homogeneity.p)

  ## Test of homogeneity of odds ratios (Jewell 2004, page 154). First work out the Woolf estimate of the adjusted odds ratio (labelled lnOR.s. here) based on Jewell (2004, page 129):
  lnOR. <- log(((a + 0.5) * (d + 0.5)) / ((b + 0.5) * (c + 0.5)))
  lnOR.var. <- (1 / (a + 0.5)) + (1 / (b + 0.5)) + (1 / (c + 0.5)) + (1 / (d + 0.5))
  wOR. <- 1 / lnOR.var.
  lnOR.s. <- sum((wOR. * lnOR.)) / sum(wOR.)

  ## Equation 10.3 from Jewell (2004):
  OR.homogeneity   <- sum(wOR. * (lnOR. - lnOR.s.)^2)
  OR.homogeneity.p <- 1 - pchisq(OR.homogeneity, df = n.strata - 1)
  OR.homog <- data.frame(test.statistic = OR.homogeneity, df = n.strata - 1, p.value = OR.homogeneity.p)
}
