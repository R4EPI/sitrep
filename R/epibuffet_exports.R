# These functions have been re-exported from the {epibuffet} package. They were
# previously part of the sitrep package, but have been moved into their own
# separate package for maintenance. By re-exporting them, the user does not 
# see much of a difference. This is different than the {apyramid} package where
# we use the function internally as it has modified syntax.

#' Functions re-expored from {epibuffet}
#'
#' @seealso \pkg{epibuffet} functions:
#'
#'  - [epibuffet::tab_linelist()]: tabulate linelist data
#'  - [epibuffet::tab_survey()]: tabulate survey data 
#'  - [epibuffet::tab_univariate()]: caluclate odds, risk, and incidence risk
#'    ratios for multiple variables from linelist data.
#'  - [epibuffet::data_frame_from_2x2()]: convert a 2x2(x2) table to a data
#'    frame clearly labelling the (un)exposed (non)case combinations and their
#'    totals.
#'
#' @name tab_survey
#' @importFrom epibuffet tab_survey
#' @export 
#' @rdname tab_linelist
"tab_survey"

#' @name tab_linelist
#' @importFrom epibuffet tab_linelist
#' @export
#' @rdname tab_linelist
"tab_linelist"

#' @name tab_univariate
#' @importFrom epibuffet tab_univariate
#' @export
#' @rdname tab_linelist
"tab_univariate"

#' @name data_frame_from_2x2
#' @importFrom epibuffet data_frame_from_2x2
#' @export
#' @rdname tab_linelist
"data_frame_from_2x2"

