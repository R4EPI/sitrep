#' Stratifies z-scores by variables in the survey data
#'
#' @param survey_data a data.frame with the survey data
#' @param by a column by which you want to stratify the data
#' @param ... columns having zscore indicators
#'
#' @include helpers.R
#' @export
#' @examples
#' # A crude example, but it works
#' stratify_zscores(mtcars, by = "gear", am, vs)
stratify_zscores <- function(survey_data, by, ...) {
  comput_result <- function(group = pairlist()) {
    res <- survey_data
    res <- dplyr::select(res, !!! rlang::ensyms(...), !! by)
    res <- tidyr::gather(res, "stratum", "flagged", -(!! by))
    res <- dplyr::group_by(res, .data$stratum, !!! group)
    res <- dplyr::summarise(res, n = n(),
              flagged = sum(.data$flagged, na.rm = TRUE),
              rel = .data$flagged / .data$n,
              ci = list(binom::binom.wilson(.data$flagged, .data$n)[, c("lower", "upper")]))
    tidyr::unnest(res)
  }
  dplyr::bind_rows(
    comput_result(),
    comput_result(group = pairlist(as.symbol(by)))
  )
}
