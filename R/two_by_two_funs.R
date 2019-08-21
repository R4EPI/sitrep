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

  # The incoming table will be a 3D array that has this pattern:
  # x <- array(1:8, 
  #       dim = c(2, 2, 2), 
  #       dimnames = list(exposure = 1:2, outcome = 1:2, strata = 1:2)
  #      )
  # x
  # , , strata = 1
  # 
  #         outcome
  # exposure 1 2
  #        1 1 3
  #        2 2 4
  # 
  # , , strata = 2
  # 
  #         outcome
  # exposure 1 2
  #        1 5 7
  #        2 6 8
  #
  # ---------------------------------------------------------------------------
  # # One of the big gotchas about 3D arrays is the fact that you can subset
  # # them both as matrices and as vectors.  
  #
  # # EXPOSURE is the first dimension. If you drop that deminsion, the resulting
  # # matrix will have OUTCOME as the first dimension
  # x[1, , ]
  #        strata
  # outcome 1 2
  #       1 1 5
  #       2 3 7
  # 
  # ---------------------------------------------------------------------------
  cmat <- case_matrix(x)

  # Because we want to get both the crude and the stratified results, we place
  # the crude result at the top of the vector.
  index <- 2:3
  A_exp_cases      <- c(sum(cmat[, "exp_cases"]),      cmat[, "exp_cases"])[index]
  B_unexp_cases    <- c(sum(cmat[, "unexp_cases"]),    cmat[, "unexp_cases"])[index]
  C_exp_controls   <- c(sum(cmat[, "exp_controls"]),   cmat[, "exp_controls"])[index]
  D_unexp_controls <- c(sum(cmat[, "unexp_controls"]), cmat[, "unexp_controls"])[index]

  total_cases     <- A_exp_cases    + B_unexp_cases
  total_controls  <- C_exp_controls + D_unexp_controls

  total_exposed   <- A_exp_cases    + C_exp_controls
  total_unexposed <- B_unexp_cases  + D_unexp_controls

  if (measure == "OR") {
    # NOTE: The table input for epiR::epi.2by2 calculates ORs wrong, so we have
    # to assume that the table is flipped around
    # DIM 1: outcome (case, control)
    # DIM 2: exposure
    # DIM 3: strata 
    # cases_exp     <- x[1, 1, , drop = TRUE] # x[c(1, 5)]
    # cases_unexp   <- x[1, 2, , drop = TRUE] # x[c(3, 7)]

    # controls_exp   <- x[2, 1, , drop = TRUE] # x[c(2, 6)]
    # controls_unexp <- x[2, 2, , drop = TRUE] # x[c(4, 8)]

    # data.frame(
    #   exp_cases      = cases_exp, #x[1, , 1, drop = TRUE], # x[c(1, 3)]
    #   unexp_cases    = cases_unexp, #x[1, , 2, drop = TRUE], # x[c(5, 7)]
    #   cases_odds     = cases_exp / cases_unexp,

    #   exp_controls   = controls_exp, #x[2, , 1, drop = TRUE], # x[c(2, 4)]
    #   unexp_controls = controls_unexp, # x[2, , 2, drop = TRUE], # x[c(6, 8)]
    #   controls_odds  = controls_exp / controls_unexp
    # )
    data.frame(
      exp_cases      = A_exp_cases, #cases_exp, #x[1, , 1, drop = TRUE], # x[c(1, 3)]
      unexp_cases    = B_unexp_cases, #x[1, , 2, drop = TRUE], # x[c(5, 7)]
      cases_odds     = A_exp_cases / B_unexp_cases,

      exp_controls   = C_exp_controls, #x[2, , 1, drop = TRUE], # x[c(2, 4)]
      unexp_controls = D_unexp_controls, # x[2, , 2, drop = TRUE], # x[c(6, 8)]
      controls_odds  = C_exp_controls / D_unexp_controls
    )

  } else if (measure == "RR") {
    # DIM 1: exposure 
    # DIM 2: outcome (case, control)
    # DIM 3: strata 

    # NOTE: Here, I'm aligning the subsetting so that it's easier to see how I'm
    # obtaining each value. The exposed cases include both of the values in the
    # stratifier. The exposed total uses the colSums because the strata variable
    # is now in the columns as it moved down a dimension after dropping the 
    # exposure.
      # exp_cases <-         x[1, 1, , drop = TRUE]
      # exp_total <- colSums(x[1,  , , drop = TRUE])
    # unexp_cases <-         x[2, 1, , drop = TRUE]
    # unexp_total <- colSums(x[2,  , , drop = TRUE])

    data.frame(
      exp_cases   = A_exp_cases,
      exp_total   = total_exposed,
      exp_risk    = (A_exp_cases / total_exposed) * 100,

      unexp_cases = B_unexp_cases,
      unexp_total = total_unexposed,
      unexp_risk  = (B_unexp_cases / total_unexposed) * 100
    )

  } else if (measure == "IRR") {
    # sigma(a_i * PTo / PTt) / sigma(c_i * PTe / PTt)
    # where PTt is the sum of respective strata
    # and the table is set out as follows:
    #           outcome+   | person time
    # exposure+     a      |      PTe
    # exposure-     c      |      PTo
    # Total                |      PTt
    # DIM 1: exposure 
    # DIM 2: outcome (outcome, person time)
    # DIM 3: strata 
      # exp_cases    <- x[1, 1, , drop = TRUE]
      # exp_perstime <- x[1, 2, , drop = TRUE]

    # unexp_cases    <- x[2, 1, , drop = TRUE]
    # unexp_perstime <- x[2, 2, , drop = TRUE]

    data.frame(
      exp_cases       = A_exp_cases,
      exp_perstime    = C_exp_controls,
      exp_incidence   = (A_exp_cases / C_exp_controls) * 100,

      unexp_cases     = B_unexp_cases,
      unexp_perstime  = D_unexp_controls,
      unexp_incidence = (B_unexp_cases / D_unexp_controls) * 100
    )

  } else {
    stop(glue::glue("the measure {measure} is not recognised"), call. = FALSE)
  }
}


#' create a 2x4 matrix from a 2x2x2 array
#'
#' Here, we are assuming the following:
#'
#' 1. the array has three dimensions
#' 2. the first value for any dimension is TRUE and the second value is FALSE
#' 3. there are a total of 8 cells whose contents represent counts.
#' 4. the dimensions are orderd as exposure, outcome, and stratifier
#'
#' @param x a 2x2x2 array
#'
#' @return a 2x4 matrix with the third dimension of the array in rows.
#' @noRd
case_matrix <- function(x, total = FALSE) {

  # Dimension 1: exposure (exposed, unexposed)
  # Dimension 2: outcome (case, controls)
  # Dimension 3 (optional): stratifier (e.g. age group)

  # we subset things differently if we have a cube (3D) or square (2D).
  #
  # For the cube, we result in 3 rows because we take the sum of all the strata
  # to create the crude.
  #
  # For the square, we only need to produce one row.
  ndim <- length(dim(x))
  if (ndim == 3L) { 
    A <- x[1, 1, ]
    B <- x[1, 2, ]
    C <- x[2, 1, ]
    D <- x[2, 2, ] 
    res <- c(c(sum(A), A), 
             c(sum(B), B), 
             c(sum(C), C), 
             c(sum(D), D)
            ) 
    nr     <- dim(x)[ndim] + 1L
    rnames <- dimnames(x)[[3]]
  } else {
    A <- x[1, 1]
    B <- x[1, 2]
    C <- x[2, 1]
    D <- x[2, 2] 
    res    <- c(A, B, C, D)
    nr     <- 1L
    rnames <- NULL
  }
  res <- matrix(res, 
                nrow = nr,
                dimnames = list(strata = c("crude", rnames),
                                c(A = "exp_cases",    
                                  B = "unexp_cases",
                                  C = "exp_controls", 
                                  D = "unexp_controls")
                )
  )
  if (ndim == 3) {
    res[if (total) 1:3 else 2:3, ]
  } else {
    res
  }
}

#' create a data frame from a 2x2x2 matrix
#'
#' @param x a 2x2x2 matrix
#'
#' @return a data frame with the important combinations:
#'  - A_exp_cases
#'  - B_unexp_cases
#'  - C_exp_controls
#'  - D_unexp_controls
#'  - total_cases (A + B)
#'  - total_controls (C + D)
#'  - total_exposed (A + C)
#'  - total_unexposed (B + D)
#'  - total (A + B + C + D)
#' @noRd
case_data_frame <- function(x) {

  cmat <- case_matrix(x, total = TRUE)
  A_exp_cases      <- cmat[, "exp_cases"]
  B_unexp_cases    <- cmat[, "unexp_cases"]
  C_exp_controls   <- cmat[, "exp_controls"]
  D_unexp_controls <- cmat[, "unexp_controls"]

  data.frame(
    A_exp_cases      = A_exp_cases,
    B_unexp_cases    = B_unexp_cases,
    C_exp_controls   = C_exp_controls,
    D_unexp_controls = D_unexp_controls,

    total_cases      = A_exp_cases    + B_unexp_cases,
    total_controls   = C_exp_controls + D_unexp_controls,

    total_exposed    = A_exp_cases    + C_exp_controls,
    total_unexposed  = B_unexp_cases  + D_unexp_controls,

    total            = A_exp_cases + B_unexp_cases +
                       C_exp_controls + D_unexp_controls
  )

}



#' get ratio estimates from a 2x2x2 array
#'
#' @param x a 2x2x2 array
#' @param measure one of OR, RR, or IRR
#' @param conf the confidence level
#'
#' @return a matrix with three columns, ratio, lower, and upper
#' @export
#'
#' @examples
#' arr <- c(10, 35, 90, 465, 36, 25, 164, 175)
#' arr <- array(arr, dim = c(2, 2, 2),
#'              dimnames = list(risk = c(TRUE, FALSE),
#'                              outcome = c(TRUE, FALSE),
#'                              old = c(FALSE, TRUE))
#'        )
#' get_ratio_est(arr, "OR") # works
#' get_ratio_est(arr, "RR") # doesn't work
#' get_ratio_est(arr, "IRR") # estimate works
#'
get_ratio_est <- function(x, measure = "OR", conf = 0.95) {


  d <- case_data_frame(arr)

  # Risk ratio:
  # A / (A + B)      exposed cases / all cases
    # ----------- = ------------------------------
  # C / (C + D)   exposed controls / all controls
  ratio   <- switch(measure, 
                    RR = ratio_est(d$A_exp_cases, 
                                   d$total_cases,
                                   d$C_exp_controls, 
                                   d$total_controls,
                                   measure = "RR",
                                   conf = conf),
  # Odds ratio:
  # A / B      exposed cases / unexposed cases
  # ----- = -------------------------------------
  # C / D   exposed controls / unexposed controls

                    OR = ratio_est(d$A_exp_cases, 
                                   d$B_unexp_cases,
                                   d$C_exp_controls, 
                                   d$D_unexp_controls,
                                   measure = "OR",
                                   conf = conf),

  # Incidence Rate Ratio:
  # A / B      exposed cases / exposed person-time
  # ----- = ----------------------------------------
  # C / D   exposed controls / unexposed person-time
                    IRR = ratio_est(d$A_exp_cases, 
                                    d$B_unexp_cases,
                                    d$C_exp_controls, 
                                    d$D_unexp_controls,
                                    measure = "IRR",
                                    conf = conf),

  )
  rownames(ratio) <- rownames(d)
  ratio
  
}

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
