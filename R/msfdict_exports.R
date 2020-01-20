# These functions have been re-exported from the {msfdict} package. They were
# previously part of the sitrep package, but have been moved into their own
# separate package for maintenance. By re-exporting them, the user does not 
# see much of a difference. This is different than the {apyramid} package where
# we use the function internally as it has modified syntax.

#' Functions re-expored from {msfdict}
#'
#' @seealso Dictionaries: [msfdict::msf_dict()], [msfdict::msf_dict_survey()]\cr
#'   Generator: [msfdict::gen_data()]
#' @name msf_dict_survey
#' @importFrom msfdict msf_dict_survey
#' @export 
#' @rdname msf_dict
"msf_dict_survey"

#' @name msf_dict
#' @importFrom msfdict msf_dict
#' @export
#' @rdname msf_dict
"msf_dict"

#' @name gen_data
#' @importFrom msfdict gen_data
#' @export
#' @rdname msf_dict
"gen_data"

