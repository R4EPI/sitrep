#' Rates and Ratios
#'
#' Calculate attack rate, case fatality rate, and mortality rate
#'
#' @param cases,deaths number of cases or deaths in a population.
#' @param population the number of individuals in the population.
#' @param conf_level a number representing the confidence level for which to
#' calculate the confidence interval. Defaults to 0.95, representinc 95%
#' confidence interval.
#' @param multiplier The base by which to multiply the output:
#'  - `multiplier = 1`: ratio between 0 and 1
#'  - `multiplier = 100`: proportion
#'  - `multiplier = 10^4`: x per 10,000 people
#' @param mergeCI Whether or not to put the confidence intervals in one column (default is FALSE)
#' @param digits if `mergeCI = TRUE`, this determines how many digits are printed
#' @export
#' @rdname attack_rate
#' @examples
#' # Attack rates can be calculated with just two numbers
#' print(ar <- attack_rate(10, 50), digits = 4) # 20% attack rate
#' 
#' # print them inline using `fmt_ci_df()`
#' fmt_ci_df(ar)
#'
#' # Alternatively, if you want one column for the CI, use `mergeCI = TRUE`
#' attack_rate(10, 50, mergeCI = TRUE, digits = 2) # 20% attack rate
#'
#' print(cfr <- case_fatality_rate(1, 100), digits = 2) # CFR of 1%
#' fmt_ci_df(cfr)
attack_rate <- function(cases, population, conf_level = 0.95,
                        multiplier = 100, mergeCI = FALSE, digits = 2) {
  res <- proportion(cases, population, multiplier = multiplier, conf_level = conf_level)
  colnames(res) <- c("cases", "population", "ar", "lower", "upper")
  if (mergeCI == TRUE) {
    res <- merge_ci_df(res, digits = digits)
  }
  res
}

#' @rdname attack_rate
#' @export
case_fatality_rate <- function(deaths, population, conf_level = 0.95,
                               multiplier = 100, mergeCI = FALSE, digits = 2) {
  res <- proportion(deaths, population, multiplier = multiplier, conf_level = conf_level)
  colnames(res) <- c("deaths", "population", "cfr", "lower", "upper")
  if (mergeCI == TRUE) {
    res <- merge_ci_df(res, digits = digits)
  }
  res
}

#' @rdname attack_rate
#' @export
mortality_rate <- function(deaths, population, conf_level = 0.95,
                           multiplier = 10^4, mergeCI = FALSE, digits = 2) {
  stopifnot(is.numeric(multiplier), length(multiplier) == 1L, multiplier > 0)
  # as in here https://www.cdc.gov/ophss/csels/dsepd/ss1978/lesson3/section3.html
  res <- proportion(deaths, population, conf_level = conf_level, multiplier = multiplier)
  est_label <- paste0("mortality per ", scales::number(multiplier))
  colnames(res) <- c("deaths", "population", est_label, "lower", "upper")
  if (mergeCI == TRUE) {
    res <- merge_ci_df(res, digits = digits)
  }
  res
}

proportion <- function(x, n, conf_level = 0.95, multiplier = 100) {
  stopifnot(is.numeric(conf_level), conf_level >= 0, conf_level <= 1)
  res <- binom::binom.wilson(x, n, conf.level = conf_level)
  res <- res[, c("x", "n", "mean", "lower", "upper")]
  colnames(res) <- c("x", "n", "prop", "lower", "upper")
  res$prop  <- (x / n) * multiplier
  res$lower <- res$lower * multiplier
  res$upper <- res$upper * multiplier
  res
}

