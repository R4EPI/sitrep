# These functions have been re-exported from the {epitabulate} package. They were
# previously part of the sitrep package, but have been moved into their own
# separate package for maintenance. By re-exporting them, the user does not
# see much of a difference.

#' Functions re-expored from {epitabulate}
#'
#' @seealso \pkg{epitabulate} functions:
#'
#'  - [epitabulate::tab_linelist()]: tabulate linelist data
#'  - [epitabulate::tab_survey()]: tabulate survey data
#'  - [epitabulate::tab_univariate()]: caluclate odds, risk, and incidence risk
#'    ratios for multiple variables from linelist data.
#'  - [epitabulate::data_frame_from_2x2()]: convert a 2x2(x2) table to a data
#'    frame clearly labelling the (un)exposed (non)case combinations and their
#'    totals.
#'
#' @name tab_survey
#' @importFrom epitabulate tab_survey
#' @export
#' @rdname tab_linelist
"tab_survey"

#' @name tab_linelist
#' @importFrom epitabulate tab_linelist
#' @export
#' @rdname tab_linelist
"tab_linelist"

#' @name tab_univariate
#' @importFrom epitabulate tab_univariate
#' @export
#' @rdname tab_linelist
"tab_univariate"

#' @name data_frame_from_2x2
#' @importFrom epitabulate data_frame_from_2x2
#' @export
#' @rdname tab_linelist
"data_frame_from_2x2"

