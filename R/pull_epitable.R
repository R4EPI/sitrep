
get_epitable_ci <- function(epitable, measure = "OR", type = "strata") {

  res <- data.frame(
                    exp    = numeric(1),
                    lower  = numeric(1),
                    upper  = numeric(1),
                    pvalue = numeric(1),
                   )
  var              <- glue::glue("{measure}.{type}.wald")
  chi              <- glue::glue("chisq.{type}")
  res[["exp"]]     <- epitable$massoc[[var]][["exp"]]
  res[["lower"]]   <- epitable$massoc[[var]][["lower"]]
  res[["upper"]]   <- epitable$massoc[[var]][["upper"]]
  res[["p.value"]] <- epitable$massoc[[chi]][["p.value"]]
  res
}

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

    # NOTE: ZNK: This is the part I don't particularly understand. Why are the
    # strata spread across columns here? This is correct in terms of the
    # calculations, but I am really unsure of how to read this. 
    data.frame(
      exp_cases      = x[1, , 1, drop = TRUE], # x[c(1, 3)]
      unexp_cases    = x[1, , 2, drop = TRUE], # x[c(5, 7)]
      cases_odds     = cases_exp / cases_unexp,

      exp_controls   = x[2, , 1, drop = TRUE], # x[c(2, 4)]
      unexp_controls = x[2, , 2, drop = TRUE], # x[c(6, 8)]
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
      exp_cases <-         x[1, 1, , drop = TRUE]
      exp_total <- colSums(x[1,  , , drop = TRUE])
    unexp_cases <-         x[2, 1, , drop = TRUE]
    unexp_total <- colSums(x[2,  , , drop = TRUE])

    data.frame(
      exp_cases   = exp_cases,
      exp_total   = exp_total,
      exp_risk    = (exp_cases / exp_total) * 100,

      unexp_cases = unexp_cases,
      unexp_total = unexp_total,
      unexp_risk  = (unexp_cases / unexp_total) * 100
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
      exp_cases    <- x[1, 1, , drop = TRUE]
      exp_perstime <- x[1, 2, , drop = TRUE]

    unexp_cases    <- x[2, 1, , drop = TRUE]
    unexp_perstime <- x[2, 2, , drop = TRUE]

    data.frame(
      exp_cases       = exp_cases,
      exp_perstime    = exp_perstime,
      exp_incidence   = (exp_cases / exp_perstime) * 100,

      unexp_cases     = unexp_cases,
      unexp_perstime  = unexp_perstime,
      unexp_incidence = (unexp_cases / unexp_perstime) * 100
    )

  } else {
    stop(glue::glue("the measure {measure} is not recognised"), call. = FALSE)
  }
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
