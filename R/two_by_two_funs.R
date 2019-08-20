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

  A_exp_cases      <- cmat[, "exp_cases"]
  B_unexp_cases    <- cmat[, "unexp_cases"]
  C_exp_controls   <- cmat[, "exp_controls"]
  D_unexp_controls <- cmat[, "unexp_controls"]

  total_cases      <- A_exp_cases + B_unexp_cases
  total_controls   <- C_exp_controls + D_unexp_controls
  
  total_exposed    <- A_exp_cases + C_exp_controls
  total_unexposed  <- B_unexp_cases + D_unexp_controls

  if (measure == "OR") {
    # NOTE: The table input for epiR::epi.2by2 calculates ORs wrong, so we have
    # to assume that the table is flipped around
    # DIM 1: outcome (case, control)
    # DIM 2: exposure
    # DIM 3: strata 
    cases_exp     <- x[1, 1, , drop = TRUE] # x[c(1, 5)]
    cases_unexp   <- x[1, 2, , drop = TRUE] # x[c(3, 7)]

    controls_exp   <- x[2, 1, , drop = TRUE] # x[c(2, 6)]
    controls_unexp <- x[2, 2, , drop = TRUE] # x[c(4, 8)]

    data.frame(
      exp_cases      = cases_exp, #x[1, , 1, drop = TRUE], # x[c(1, 3)]
      unexp_cases    = cases_unexp, #x[1, , 2, drop = TRUE], # x[c(5, 7)]
      cases_odds     = cases_exp / cases_unexp,

      exp_controls   = controls_exp, #x[2, , 1, drop = TRUE], # x[c(2, 4)]
      unexp_controls = controls_unexp, # x[2, , 2, drop = TRUE], # x[c(6, 8)]
      controls_odds  = controls_exp / controls_unexp
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
case_matrix <- function(x) {

  # Dimension 1: exposure (exposed, unexposed)
  # Dimension 2: outcome (case, controls)
  # Dimension 3: stratifier (e.g. age group)
  A <- x[1, 1, ]
  B <- x[2, 1, ]
  C <- x[1, 2, ]
  D <- x[2, 2, ] 
  res <- matrix(c(A, B, C, D), 
                nrow = 2,
                dimnames = list(strata = dimnames(x)[[3]],
                                c(A = "exp_cases",    
                                  B = "unexp_cases",
                                  C = "exp_controls", 
                                  D = "unexp_controls")
                )
  )
  res
}
