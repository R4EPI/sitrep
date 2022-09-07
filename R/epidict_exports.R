# These functions have been re-exported from the {epidict} package. They were
# previously part of the sitrep package, but have been moved into their own
# separate package for maintenance. By re-exporting them, the user does not
# see much of a difference.

#' Functions re-expored from {epidict}
#'
#' @seealso Dictionaries: [epidict::msf_dict()], [epidict::msf_dict_survey()]\cr
#'   Renaming: [epidict::msf_dict_rename_helper()]\cr
#'   Generator: [epidict::gen_data()]

#' @name msf_dict
#' @importFrom epidict msf_dict
#' @rdname msf_dict
#' @export
epidict::msf_dict

#' @name msf_dict_survey
#' @importFrom epidict msf_dict_survey
#' @rdname msf_dict
#' @export
epidict::msf_dict_survey

#' @name msf_dict_rename_helper
#' @importFrom epidict msf_dict_rename_helper
#' @rdname msf_dict
#' @export
epidict::msf_dict_rename_helper

#' @name gen_data
#' @importFrom epidict gen_data
#' @rdname msf_dict
#' @export
epidict::gen_data
