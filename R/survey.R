# ALL WIP, so no proper docs yet.

#' @export
survey_ciprop_foreach_binary_variable <- function(design, variables) {
  dplyr::bind_rows(
    lapply(variables, function(var){
      formula <- rlang::new_formula(lhs = NULL, rhs = as.symbol(var))
      res <- survey::svyciprop(formula, design, method = "logit", na.rm = TRUE)
      CIs <- confint(res)
      dplyr::tibble(variable = var,
             n = sum(design$variables[[var]] == 1, na.rm = TRUE),
             mean = as.numeric(res),
             CI_lower = CIs[[1L]],
             CI_upper = CIs[[2L]])
    })
  )
}

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
