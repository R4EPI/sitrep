#' Compute a sample size for surveys
#'
#' @export
sample_size <- function(population_size, expected_prevalence,
                        precision, design_effect, alpha = 0.05) {
  num <- (design_effect * population_size * expected_prevalence * (1 - expected_prevalence))
  denom <- (precision^2 / qnorm(1 - alpha / 2)^2 * (population_size - 1) + expected_prevalence * (1 - expected_prevalence))
  as.integer(round(num / denom))
}

#' Take a sample size if children and convert it to households
#'
#' @export
sample_size_households <- function(sample_size, avg_hh, prop_under_5, frac_6_59, non_response_rate = 0) {
  hh <- sample_size / (avg_hh * prop_under_5 * frac_6_59)
  hh <- (1 + non_response_rate) * hh
  as.integer(ceiling(hh))
}
