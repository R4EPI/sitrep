#' Tabulate survey design objects by a categorical and another stratifying variable
#'
#' @param x a survey design object
#' @param var the bare name of a categorical variable 
#' @param strata a variable to stratify the results by
#' @param pretty if `TRUE`, default, the proportion and CI are merged
#' @param digits if `prittey = FALSE`, this indicates the number of digits used
#'   for proportion and CI
#' @return a tibble
#' @export
#' @examples
#' library(srvyr)
#' library(survey)
#' data(api)
#' 
#' # stratified sample
#' apistrat %>%
#'   as_survey_design(strata = stype, weights = pw) %>%
#'   tabulate_survey(stype, awards)
#'
#' apistrat %>%
#'   as_survey_design(strata = stype, weights = pw) %>%
#'   tabulate_binary_survey(stype, awards, keep = c("Yes", "E"))
#'
#' apistrat %>%
#'   as_survey_design(strata = stype, weights = pw) %>%
#'   tabulate_binary_survey(stype, awards, keep = c("Yes", "E"), invert = TRUE)
tabulate_survey <- function(x, var, strata = NULL, pretty = TRUE, digits = 1) {
  stopifnot(inherits(x, "tbl_svy"))

  cod <- rlang::enquo(var)
  st  <- rlang::enquo(strata)
  null_strata <- is.null(rlang::get_expr(st))
  
  
  x <- if (null_strata) srvyr::group_by(x, !! cod) else srvyr::group_by(x, !!st, !! cod)


  x <- srvyr::summarise(x,
                        n = survey_total(var = "se", na.rm = TRUE),
                        proportion = survey_mean(vartype = "ci", na.rm = TRUE)
                       )
  x$n <- round(x$n)
  x   <- x[!colnames(x) %in% "n_se"]

  if (!pretty) {
    return(x)
  }

  x <- unite_ci(x, "prop", dplyr::starts_with("proportion"), percent = TRUE, digits = digits)

  if (null_strata) {
    return(x)
  }

  x <- dplyr::select(x, !! cod, !! st, "n", "prop")

  x <- tidyr::gather(x, key = "variable", value = "value", -(1:2))
  x <- tidyr::unite(x, "tmp", !! st, "variable", sep = " ")
  x <- tidyr::spread(x, "tmp", "value")
  rename_at(x, dplyr::vars(dplyr::ends_with("prop")), 
            ~function(i) rep("% (95% CI)", length(i)))
}

#' @export
#' @rdname tabulate_survey
#' @param ... binary variables for tabulation
#' @param keep a vector of binary values to keep
#' @param invert if `TRUE`, the kept values are rejected. Defaults to `FALSE`
#'
tabulate_binary_survey <- function(x, ..., keep = NULL, invert = FALSE, pretty = TRUE, digits = 1) {

  stopifnot(inherits(x, "tbl_svy"))
  if (is.null(keep)) {
    stop("Please provide a list of values to keep in the output.")
  }

  vars <- tidyselect::vars_select(colnames(x), ...)
  res <- lapply(vars, function(i) tabulate_survey(x, !! rlang::ensym(i), pretty = pretty, digits = digits))
  for (i in seq_along(res)) {
    names(res[[i]])[1] <- "value"
  }
  suppressWarnings(res <- dplyr::bind_rows(res, .id = "variable"))
  
  res[if (invert) !res$value %in% keep else res$value %in% keep, ]
}
