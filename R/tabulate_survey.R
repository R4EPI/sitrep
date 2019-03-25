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
#' api %>%
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

  x <- srvyr::select(x, !! cod, !!st)
  if (null_strata) {
    x <- srvyr::group_by(x, !! cod) 
    x <- srvyr::mutate(x, dummy = !! cod)
  } else {
    x <- srvyr::group_by(x, !!st, !! cod)
    x <- srvyr::mutate(x, dummy = sprintf("%s %s", !! cod, !!st))
  }
  
  p <- unique(dplyr::pull(srvyr::as_tibble(x), !!quote(dummy)))

  

  y <- srvyr::summarise(x, n = survey_total(var = "se", na.rm = TRUE))
  
  res <- setNames(vector(mode = "list", length = nrow(x)), p)
  
  for (i in p) {
    res[[p]] <- srvyr::summarise(x, 
                                 proportion = srvyr::survey_mean(dummy == i, 
                                                                 proportion = TRUE,
                                                                 var = "ci",
                                                                 na.rm = TRUE)
                                 )
  }
  return(res)

  y$n <- round(y$n)
  y   <- y[!colnames(y) %in% "n_se"]

  if (!pretty) {
    return(y)
  }

  y <- unite_ci(y, "prop", dplyr::starts_with("proportion"), percent = TRUE, digits = digits)

  if (null_strata) {
    return(y)
  }

  y <- dplyr::select(y, !! cod, !! st, "n", "prop")

  y <- tidyr::gather(y, key = "variable", value = "value", -(1:2))
  y <- tidyr::unite(y, "tmp", !! st, "variable", sep = " ")
  y <- tidyr::spread(y, "tmp", "value")
  rename_at(y, dplyr::vars(dplyr::ends_with("prop")),
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
