#' # Inspired by Daniel Gardiner
#' https://github.com/DanielGardiner/UsefulFunctions/blob/efffde624d424d977651ed1a9ee4430cbf2b0d6f/single.variable.analysis.v0.3.R#L12
#' @export


univariate_analysis <- function (outcome, ...) {
  n <- length(outcome)
  predictors <- list(...)
  predictor_labels <- substitute(list(...))
  predictor_labels <- vapply(predictor_labels[-1L], deparse, character(1L))
  stopifnot(length(predictors) > 0L, 
            all(vapply(predictors, length, integer(1L)) == n),
            all(vapply(predictors, is.logical, logical(1L))))
  na_rows <- Reduce(function (acc, el) acc | is.na(el), predictors, init = FALSE)
  n_na_rows <- sum(na_rows)
  
  if (n_na_rows) {
    warning("Removed ", n_na_rows, " rows due to missing values")
  }
  
  res <- lapply(predictors, function (predictor) {
    table <- epitools::epitable(outcome[!na_rows], 
                                predictor[!na_rows])    
    #odds <- epitools::oddsratio(table, method = "wald") # what method?
    rr <- epitools::riskratio(table, method = "wald")
    midp_pval_rr <- rr$p.value[2L, 1L]
    
    cbind(
      data.frame(
        exp = rr$data[3L, 2L],
        exp_cases = rr$data[2L, 2L],
        exp_AR = round(rr$data[2L, 2L] / rr$data[3L, 2L], 1L),
        unexp = rr$data[1L, 3L],
        unexp_cases = rr$data[1L, 2L],
        unexp_AR = round(rr$data[1L, 2L] / rr$data[1L, 3L], 1L)
      ),
      t(rr$measure[2L, ]),
      data.frame(p_value = midp_pval_rr)
    )
  })
  
  cbind(
    data.frame(exposure = predictor_labels),
    do.call(rbind, res)
  )
}
