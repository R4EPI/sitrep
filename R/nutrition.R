#' Stratifies z-scores by variables in the survey data
#'
#' @param survey_data a data.frame with the survey data
#' @param by a column by which you want to stratify the data
#' @param ... columns having zscore indicators
#'
#' @include helpers.R
#' @export
stratify_zscores <- function(survey_data, by, ...) {
  comput_result <- function(group = pairlist()) {
    res <- survey_data
    res <- dplyr::select(res, !!! rlang::ensyms(...), !! by)
    res <- tidyr::gather(res, "stratum", "flagged", -(!! by))
    res <- dplyr::group_by(res, .data$stratum, !!! group)
    dplyr::summarise(res, n = n(),
              flagged = sum(.data$flagged, na.rm = TRUE),
              rel = .data$flagged / .data$n,
              !!! splice_df(binom::binom.wilson(.data$flagged, .data$n), "lower", "upper"))
  }
  dplyr::bind_rows(
    comput_result(),
    comput_result(group = pairlist(as.symbol(by)))
  )
}
