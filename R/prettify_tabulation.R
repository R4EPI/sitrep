#' Make the tabulation pretty by uniting the confindence intervals
#'
#' @param y a data frame
#' @param digits number of digits to round to
#' @param null_strata a logical value specifyingif there is a null strata variable
#' @param cod variable of interest
#' @param st stratifying variable
#' @noRd
prettify_tabulation <- function(y, digits = 1, ci_prefix = "") {


  ci <- trimws(sprintf("%s ci", ci_prefix))
  y <- unite_ci(y, ci, dplyr::contains("proportion"), percent = TRUE, digits = digits)

  # convert any NA% proportions to just NA
  y[[ci]] <- dplyr::if_else(grepl("NA%", y[[ci]]), NA_character_, y[[ci]])

  return(y)

}

