# These functions have been re-exported from the {epitabulate} package. They were
# previously part of the sitrep package, but have been moved into their own
# separate package for maintenance. By re-exporting them, the user does not
# see much of a difference.

#' Functions re-exported from {epitabulate}
#'
#' @seealso \pkg{epitabulate} functions:
#'  - [epitabulate::attack_rate()],
#'  - [epitabulate::case_fatality_rate()]
#'  - [epitabulate::case_fatality_rate_df()],
#'  - [epitabulate::mortality_rate()]
#'  - [epitabulate::add_ar()]: a {gtsummary} wrapper for epitabulate::attack_rate()
#'  - [epitabulate::add_cfr()]: a {gtsummary} wrapper for
#'    epitabulate::case_fatality_rate()
#'  - [epitabulate::add_crosstabs()]: a {gtsummary} wrapper to add counts to
#'    a gtsummary::tbl_uvregression()
#'  - [epitabulate::add_mr()]: a {gtsummary} wrapper for epitabulate::mortality_rate()
#'  - [epitabulate::gt_remove_stat()]: a {gtsummary} wrapper to remove variables
#'    from a gtsummary table
#'  - [epitabulate::tab_linelist()]: tabulate linelist data
#'  - [epitabulate::tab_survey()]: tabulate survey data
#'  - [epitabulate::tab_univariate()]: caluclate odds, risk, and incidence risk
#'    ratios for multiple variables from linelist data.
#'  - [epitabulate::data_frame_from_2x2()]: convert a 2x2(x2) table to a data
#'    frame clearly labelling the (un)exposed (non)case combinations and their
#'    totals.


#' @name attack_rate
#' @importFrom epitabulate attack_rate
#' @rdname eptabulate-exports
#' @export
epitabulate::attack_rate

#' @name case_fatality_rate
#' @importFrom epitabulate case_fatality_rate
#' @rdname epitabulate-exports
#' @export
epitabulate::case_fatality_rate

#' @name case_fatality_rate_df
#' @importFrom epitabulate case_fatality_rate_df
#' @rdname epitabulate-exports
#' @export
epitabulate::case_fatality_rate_df

#' @name mortality_rate
#' @importFrom epitabulate mortality_rate
#' @rdname epitabulate-exports
#' @export
epitabulate::mortality_rate

#' @name add_ar
#' @importFrom epitabulate add_ar
#' @rdname add_ar
#' @export
epitabulate::add_ar

#' @name add_cfr
#' @importFrom epitabulate add_cfr
#' @rdname add_ar
#' @export
epitabulate::add_cfr

#' @name add_crosstabs
#' @importFrom epitabulate add_crosstabs
#' @rdname add_crosstabs
#' @export
epitabulate::add_crosstabs

#' @name add_mr
#' @importFrom epitabulate add_mr
#' @rdname add_ar
#' @export
epitabulate::add_mr

#' @name gt_remove_stat
#' @importFrom epitabulate gt_remove_stat
#' @rdname gt_remove_stat
#' @export
epitabulate::gt_remove_stat

#' @name tab_survey
#' @importFrom epitabulate tab_survey
#' @rdname tab_linelist
#' @export
epitabulate::tab_survey

#' @name tab_linelist
#' @importFrom epitabulate tab_linelist
#' @rdname tab_linelist
#' @export
epitabulate::tab_linelist

#' @name tab_univariate
#' @importFrom epitabulate tab_univariate
#' @rdname tab_linelist
#' @export
epitabulate::tab_univariate

#' @name data_frame_from_2x2
#' @importFrom epitabulate data_frame_from_2x2
#' @rdname tab_linelist
#' @export
epitabulate::data_frame_from_2x2
