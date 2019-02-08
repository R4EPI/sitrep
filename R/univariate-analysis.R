#' Produce risk ratios or odds ratios
#' @param measure Specify what you would like to calculated, options are "OR" or "RR"
#' default is "OR"
#' @param digits Specify number of decimal places
#' @param verbose Specify whether would like all outputs in table (default is TRUE)
#' Non-verbose output drops group odds/risk calculations as well as p-value
#' @param mergeCI Whether or not to put the confidence intervals in one column (default is FALSE)
#' @param outcome Vector of TRUE/FALSE from your outcome variable of interest
#' @param ... TRUE/FALSE vectors for the exposure variables of interest
#' @references Inspired by Daniel Gardiner,
#' see [github repo](https://github.com/DanielGardiner/UsefulFunctions/blob/efffde624d424d977651ed1a9ee4430cbf2b0d6f/single.variable.analysis.v0.3.R#L12)
#' @export

univariate_analysis <- function(measure = "OR", digits = 3, verbose = TRUE,
                                mergeCI = FALSE, outcome, ...) {
  n <- length(outcome)
  predictors <- list(...)
  predictor_labels <- substitute(list(...))
  predictor_labels <- vapply(predictor_labels[-1L], deparse, character(1L))
  stopifnot(
    length(predictors) > 0L,
    all(vapply(predictors, length, integer(1L)) == n),
    all(vapply(predictors, is.logical, logical(1L)))
  )
  na_rows <- Reduce(function(acc, el) acc | is.na(el), predictors, init = FALSE)
  n_na_rows <- sum(na_rows)

  if (n_na_rows) {
    warning("Removed ", n_na_rows, " rows due to missing values")
  }

  if (measure == "OR") {
    res <- lapply(predictors, function(predictor) {
      table <- epitools::epitable(
        outcome[!na_rows],
        predictor[!na_rows]
      )

      or <- epitools::oddsratio(table, method = "wald")
      midp_pval_or <- round(or$p.value[2L, 1L], digits = digits)

      outtie <- cbind(
        data.frame(
          exp_cases = or$data[2L, 2L],
          unexp_cases = or$data[1L, 2L],
          cases_odds = round(or$data[2L, 2L] / or$data[1L, 2L], digits = digits),
          exp_noncases = or$data[2L, 1L],
          unexp_noncases = or$data[1L, 1L],
          noncases_odds = round(or$data[2L, 1L] / or$data[1L, 1L], digits = digits)
        ),
        t(round(or$measure[2L, ], digits = digits)),
        data.frame(p_value = midp_pval_or)
      )

      if (verbose == FALSE) {
        outtie$cases_odds <- NULL
        outtie$noncases_odds <- NULL
        outtie$p_value <- NULL
      }

      if (mergeCI == TRUE) {
        outtie$ci <- paste0(outtie$lower, " - ", outtie$upper)
        outtie$lower <- NULL
        outtie$upper <- NULL
      }

      outtie
    })
  }

  if (measure == "RR") {
    res <- lapply(predictors, function(predictor) {
      table <- epitools::epitable(
        outcome[!na_rows],
        predictor[!na_rows]
      )

      rr <- epitools::riskratio(table, method = "wald")
      midp_pval_rr <- round(rr$p.value[2L, 1L], digits = digits)

      outtie <- cbind(
        data.frame(
          exp_cases = rr$data[2L, 2L],
          exp = rr$data[2L, 3L],
          exp_AR = round(rr$data[2L, 2L] / rr$data[2L, 3L], digits = digits),
          unexp_cases = rr$data[1L, 2L],
          unexp = rr$data[1L, 3L],
          unexp_AR = round(rr$data[1L, 2L] / rr$data[1L, 3L], digits = digits)
        ),
        t(round(rr$measure[2L, ], digits = digits)),
        data.frame(p_value = midp_pval_rr)
      )

      if (verbose == FALSE) {
        outtie$exp_AR <- NULL
        outtie$unexp_AR <- NULL
        outtie$p_value <- NULL
      }

      if (mergeCI == TRUE) {
        outtie$ci <- paste0(outtie$lower, " - ", outtie$upper)
        outtie$lower <- NULL
        outtie$upper <- NULL
      }
      outtie
    })
  }

  tibble::as_tibble(
    cbind(
    data.frame(exposure = predictor_labels),
    do.call(rbind, res)
    )
  )
}
