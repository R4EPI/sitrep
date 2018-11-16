#' Compute a sample size for a proportion
#'
#'
#'
#' @references
#' This method is based on the work of Kevin M. Sullivan and Andrew G. Dean
#' of OpenEpi.
#'
#' @export
sample_size <- function(population_size, expected_prevalence,
                        precision, design_effect, alpha = 0.05) {
  num <- (design_effect * population_size * expected_prevalence *
            (1 - expected_prevalence))
  denom <- precision^2 / stats::qnorm(1 - alpha / 2)^2 * (population_size - 1) +
              expected_prevalence * (1 - expected_prevalence)
  as.integer(round(num / denom))
}

#' Take a sample size of children and convert it to households
#'
#'
#' @references
#' Sampling Methods and Sample Size Calculation for the SMART Methodology
#' <https://www.humanitarianresponse.info/sites/www.humanitarianresponse.info/files/documents/files/Sampling_Paper_June_2012.pdf>
#'
#' @export
sample_size_households <- function(sample_size, avg_hh,
                                   prop_under_5,
                                   frac_6_59, non_response_rate = 0) {
  hh <- sample_size / (avg_hh * prop_under_5 * frac_6_59)
  hh <- hh / (1 - non_response_rate)
  as.integer(ceiling(hh))
}
