#' Tabulate survey design objects by a categorical and another stratifying variable
#'
#' @param x a survey design object
#' @param var the bare name of a categorical variable
#' @param strata a variable to stratify the results by
#' @param pretty if `TRUE`, default, the proportion and CI are merged
#' @param digits if `prittey = FALSE`, this indicates the number of digits used
#'   for proportion and CI
#' @param method a method from [survey::svyciprop()] to calculate the confidence
#'   interval. Defaults to "logit"
#' @return a tibble
#' @export
#' @importFrom srvyr survey_total survey_mean
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
tabulate_survey <- function(x, var, strata = NULL, pretty = TRUE, digits = 1, method = "logit") {
  stopifnot(inherits(x, "tbl_svy"))

  cod <- rlang::enquo(var)
  st  <- rlang::enquo(strata)
  null_strata <- is.null(rlang::get_expr(st))

  x <- srvyr::select(x, !! cod, !!st)

  # here we are creating a dummy variable that is either the var or the
  # combination of var and strata so that we can get the right proportions from
  # the survey package in a loop below
  if (null_strata) {
    x <- srvyr::group_by(x, !! cod) 
    x <- srvyr::mutate(x, dummy = !! cod)
  } else {
    # if there is a strata, create a unique, parseable dummy var by inserting
    # the timestamp in between the vars
    tim <- as.character(Sys.time())
    x <- srvyr::group_by(x, !!st, !! cod)
    x <- srvyr::mutate(x, dummy = sprintf("%s %s %s", !! st, tim, !! cod))
  }

  # Calculating the survey total will also give us zero counts 
  y <- srvyr::summarise(x, n = survey_total(vartype = "se", na.rm = TRUE))

  # We can then set up the proportion calculations. Because of issues with using
  # srvyr::survey_mean() on several variables, we have to roll our own.
  y$proportion       <- NA_real_
  y$proportion_lower <- NA_real_
  y$proportion_upper <- NA_real_


  # Here we pull out the relevant values for the proportions
  # first, filter out any rows with missing values
  tx <- dplyr::filter(srvyr::as_tibble(x), !is.na(!! cod))
  # Then pull out the unique dummy variable names
  p <- as.character(unique(dplyr::pull(tx, !!quote(dummy))))
  p <- p[!is.na(p)]
  
  # loop over the values and caclucate CI proportion as needed. If they aren't
  # present, then the proportion remains NA.
  for (i in p) { 
    # Find the row that i corresponds to. It differs if there are strata or not
    if (null_strata) {
      val <- y[[1]] == i
    } else {
      val <- sprintf("%s %s %s", y[[1]], tim, y[[2]]) == i
    }
    if (y$n[val] > 0) {
      # The peanutbutter and paperclips way of getting a proportion: 
      # set a column to contain only the value you desire
      x   <- srvyr::mutate(x, this = i)
      # get the proportion of your target variable that matches the new column
      # et voila!
      tmp <- survey::svyciprop(~I(dummy == this), x, method = method)
      ci  <- attr(tmp, "ci")
      y$proportion[val]       <- tmp[[1]]
      y$proportion_lower[val] <- ci[[1]]
      y$proportion_upper[val] <- ci[[2]]
    }
  }


  y$n <- round(y$n)
  y   <- y[!colnames(y) %in% "n_se"]

  if (!pretty) {
    return(y)
  }

  y <- unite_ci(y, "prop", dplyr::starts_with("proportion"), percent = TRUE, digits = digits)
  
  # convert any NA% proportions to just NA
  y$prop <- dplyr::if_else(grepl("NA%", y$prop), NA_character_, y$prop)

  if (null_strata) {
    return(y)
  }

  y <- dplyr::select(y, !! cod, !! st, "n", "prop")

  y <- tidyr::gather(y, key = "variable", value = "value", -(1:2))
  y <- tidyr::unite(y, "tmp", !! st, "variable", sep = " ")
  y <- tidyr::spread(y, "tmp", "value")
  dplyr::rename_at(y, dplyr::vars(dplyr::ends_with("prop")),
                   ~function(i) rep("% (95% CI)", length(i)))
}

#' @export
#' @rdname tabulate_survey
#' @param ... binary variables for tabulation
#' @param keep a vector of binary values to keep
#' @param invert if `TRUE`, the kept values are rejected. Defaults to `FALSE`
#'
tabulate_binary_survey <- function(x, ..., keep = NULL, invert = FALSE, pretty = TRUE, digits = 1, method = "logit") {

  stopifnot(inherits(x, "tbl_svy"))
  if (is.null(keep)) {
    stop("Please provide a list of values to keep in the output.")
  }

  vars <- tidyselect::vars_select(colnames(x), ...)
  res <- lapply(vars, function(i) tabulate_survey(x, !! rlang::ensym(i), pretty = pretty, digits = digits, method = method))
  for (i in seq_along(res)) {
    names(res[[i]])[1] <- "value"
  }
  suppressWarnings(res <- dplyr::bind_rows(res, .id = "variable"))

  res[if (invert) !res$value %in% keep else res$value %in% keep, ]
}
