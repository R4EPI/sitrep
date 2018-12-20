proportion <- function(x, n, conf_level = 0.95, multiplier = 1) {
  stopifnot(is.numeric(conf_level), conf_level >= 0, conf_level <= 1)
  res <- binom::binom.wilson(x, n, conf.level = conf_level)
  res <- res[, c("x", "n", "mean", "lower", "upper")]
  colnames(res) <- c("x", "n", "prop", "lower", "upper")
  res$prop <- (x / n) * multiplier
  res$lower <- res$lower * multiplier
  res$upper <- res$upper * multiplier
  res
}

#' @export
attack_rate <- function(cases, population, conf_level = 0.95) {
  res <- proportion(cases, population, conf_level = conf_level)
  colnames(res) <- c("cases", "population", "ar", "lower", "upper")
  res
}

#' @export
case_fatality_rate <- function(deaths, population, conf_level = 0.95) {
  res <- proportion(deaths, population, conf_level = conf_level)
  colnames(res) <- c("deaths", "population", "cfr", "lower", "upper")
  res
}

#' @export
mortality_rate <- function(deaths, population,
                            conf_level = 0.95, multiplier = 10^4) {
  stopifnot(is.numeric(multiplier), length(multiplier) == 1L, multiplier > 0)
  # as in here https://www.cdc.gov/ophss/csels/dsepd/ss1978/lesson3/section3.html
  res <- proportion(deaths, population, conf_level = conf_level, multiplier = multiplier)
  est_label <- paste0("mortality per ", scales::number(multiplier))
  colnames(res) <- c("deaths", "population", est_label, "lower", "upper")
  res
}
