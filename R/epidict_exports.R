# These functions have been re-exported from the {epidict} package. They were
# previously part of the sitrep package, but have been moved into their own
# separate package for maintenance. By re-exporting them, the user does not 
# see much of a difference. This is different than the {apyramid} package where
# we use the function internally as it has modified syntax.

#' Functions re-expored from {epidict}
#'
#' @seealso Dictionaries: [epidict::msf_dict()], [epidict::msf_dict_survey()]\cr
#'   Generator: [epidict::gen_data()]
#' @name msf_dict_survey
#' @importFrom epidict msf_dict_survey
#' @export 
#' @rdname msf_dict
"msf_dict_survey"

#' @name msf_dict
#' @importFrom epidict msf_dict
#' @export
#' @rdname msf_dict
"msf_dict"

#' @name gen_data
#' @importFrom epidict gen_data
#' @export
#' @rdname msf_dict
"gen_data"

