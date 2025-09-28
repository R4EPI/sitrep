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
#'  - [epitabulate::tbl_cmh()]: a {gtsummary} wrapper to calculate stratified
#'    odds, risk, and incidence risk ratios for multiple variables from linelist data.


#' @name attack_rate
#' @importFrom epitabulate attack_rate
#' @rdname epitabulate-exports
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
#' @rdname epitabulate-exports
#' @export
epitabulate::add_ar

#' @name add_cfr
#' @importFrom epitabulate add_cfr
#' @rdname epitabulate-exports
#' @export
epitabulate::add_cfr

#' @name add_crosstabs
#' @importFrom epitabulate add_crosstabs
#' @rdname epitabulate-exports
#' @export
epitabulate::add_crosstabs

#' @name add_mr
#' @importFrom epitabulate add_mr
#' @rdname epitabulate-exports
#' @export
epitabulate::add_mr

#' @name gt_remove_stat
#' @importFrom epitabulate gt_remove_stat
#' @rdname epitabulate-exports
#' @export
epitabulate::gt_remove_stat

#' @name tbl_cmh
#' @importFrom epitabulate tbl_cmh
#' @rdname epitabulate-exports
#' @export
epitabulate::tbl_cmh

