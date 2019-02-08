# ALL WIP

#' Compute summary statistics for a list of binary variables
#'
#' @param design a design object from the \code{\link{survey}} package
#' @param variables a character vector of variables present in the design
#'
#' @importFrom stats confint
#' @export
survey_ciprop_foreach_binary_variable <- function(design, variables) {
  dplyr::bind_rows(
    lapply(variables, function(var){
      suvery_ciprob(design, var)
    })
  )
}

#' Compute summary statistics for a single binary variable
#'
#' @param design a design object from the \code{\link{survey}} package
#' @param variable a variable name
#'
#' @importFrom stats confint
#' @export
suvery_ciprob <- function(design, variable) {
  formula <- rlang::new_formula(lhs = NULL, rhs = as.symbol(variable))
  res <- survey::svyciprop(formula, design, method = "logit", na.rm = TRUE)
  CIs <- confint(res)
  dplyr::tibble(n = sum(design$variables[[variable]] == 1, na.rm = TRUE),
         mean = as.numeric(res),
         CI_lower = CIs[[1L]],
         CI_upper = CIs[[2L]])
}

#' Compute stratified summary statistics for a binary variable
#'
#' @param design a design object from the \code{\link{survey}} package
#' @param variable a variable name
#' @param by another variable used for stratification
#'
#' @importFrom stats confint
#' @export
suvery_ciprob_stratified <- function(design, variable, by) {
  var <- rlang::new_formula(lhs = NULL, rhs = as.symbol(variable))
  strat <- rlang::new_formula(lhs = NULL, rhs = as.symbol(by))
  res <- survey::svyby(var, strat, design, survey::svyciprop, method = "logit", na.rm = TRUE)
  CIs <- confint(res)

  dplyr::tibble(
    by = res[[1L]],
    mean = res[[2L]],
    CI_lower = CIs[, 1L],
    CI_upper = CIs[, 2L]
  )
}
