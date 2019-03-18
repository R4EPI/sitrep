#' Create an age group variable
#'
#' @param x Your age variable
#' @param breakers A string. Age category breaks you can define within c().
#' Alternatively use "lower", "upper" and "by" to set these breaks based on a
#' sequence.
#' @param lower A number. The lowest age value you want to consider (default is 0)
#' @param upper A number. The highest age value you want to consider
#' @param by A number. The number of years you want between groups
#' @param separator A character that you want to have between ages in group
#' names. The default is "-" producing e.g. 0-10.
#' @param ceiling A TRUE/FALSE variable. Specify whether you would like the
#' highest value in your breakers, or alternatively the upper value specified,
#' to be the endpoint. This would produce the highest group of "70-80" rather
#' than "80+". The default is FALSE (to produce a group of 80+).
#' @param above.char Only considered when ceiling == FALSE.  A character that
#' you want to have after your highest age group. The default is "+" producing
#' e.g. 80+
#' @export
#' @examples
#'
#' if (require("dplyr")) {
#' set.seed(50)
#' dat <- gen_data("Cholera", n = 100)
#' ages <- dat %>% 
#'   select(starts_with("age")) %>%
#'   mutate(age_years = age_categories(age_years, breakers = c(0, 5, 10, 15, 20))) %>%
#'   mutate(age_months = age_categories(age_months, breakers = c(0, 5, 10, 15, 20))) %>%
#'   mutate(age_days = age_categories(age_days, breakers = c(0, 5, 15))) 
#' 
#' ages %>%
#'   group_age_categories(years = age_years, months = age_months, days = age_days) %>%
#'   pull(age_category) %>%
#'   table()
#' }


age_categories <- function(x, breakers = NA,
                           lower = 0, upper = NA,
                           by = 10,
                           separator = "-",
                           ceiling = FALSE,
                           above.char = "+") {


  # make sure age variable is numeric
  x <- as.numeric(x)

  if (length(breakers) == 1) {
    if (!is.na(breakers)) {
      stop("breakers must be at least three numbers")
    } else {
      breakers <- unique(c(seq(lower, upper, by = by), upper))
    }
  }

  nb <- length(breakers)

  if (ceiling) {
    lower_vals <- breakers[c(-nb, -nb + 1)]
    upper_vals <- breakers[c(-1, -nb)] - 1
    final_val  <- sprintf("%d%s%d", breakers[nb - 1], separator, breakers[nb])
    breakers[nb] <- breakers[nb] + 1L
  } else {
    lower_vals <- breakers[-nb]
    upper_vals <- breakers[-1] - 1
    final_val  <- sprintf("%d%s", breakers[nb], above.char)
    breakers   <- unique(c(breakers, Inf))
  }
  labs <- c(paste(lower_vals, upper_vals, sep = separator), final_val)

  output <- cut(x,
                breaks = breakers,
                right = FALSE,
                include.lowest = FALSE,
                labels = labs
               )

  # return variable with groups
  output
}


#' @param dat a data frame with at least one column defining an age category
#'
#' @param years,months,weeks,days the bare name of the column defining years,
#' months, weeks, or days (or NULL if the column doesn't exist)
#'
#' @param one_column if `TRUE` (default), the categories will be joined into a
#' single column called "age_category" that appends the type of age category
#' used. If `FALSE`, there will be one column with the grouped age categories
#' called "age_category" and a second column indicating age unit called
#' "age_unit".
#'
#' @return a data frame
#'
#' @rdname age_categories
#' @export
#'
group_age_categories <- function(dat, years = NULL, months = NULL, weeks = NULL, days = NULL, one_column = TRUE) {
  da <- rlang::enquo(days)
  we <- rlang::enquo(weeks)
  mo <- rlang::enquo(months)
  ye <- rlang::enquo(years)
  d <- !is.null(rlang::get_expr(da))
  w <- !is.null(rlang::get_expr(we))
  m <- !is.null(rlang::get_expr(mo))
  y <- !is.null(rlang::get_expr(ye))
  stopifnot(d || w || m || y)
  l <- length(d | w | m | y)
  nas <- factor(rep(NA_character_, l))
  da <- if (d) dplyr::pull(dat, !!da) else nas
  we <- if (w) dplyr::pull(dat, !!we) else nas
  mo <- if (m) dplyr::pull(dat, !!mo) else nas
  ye <- if (y) dplyr::pull(dat, !!ye) else nas
  if (one_column) {
    levels(da) <- if (d) paste(levels(da), "days") else levels(da)
    levels(we) <- if (w) paste(levels(we), "weeks") else levels(we)
    levels(mo) <- if (m) paste(levels(mo), "months") else levels(mo)
    levels(ye) <- if (y) paste(levels(ye), "years") else levels(ye)
  } else {
    type = NULL
  }
  res <- dplyr::case_when(
    !is.na(da) ~ as.character(da),
    !is.na(we) ~ as.character(we),
    !is.na(mo) ~ as.character(mo),
    TRUE       ~ as.character(ye)
  )
  
  res <- factor(res, levels = forcats::lvls_union(list(da, we, mo, ye)))

  if (one_column) {
    res <- tibble::add_column(dat, age_category = res)
  } else {
    type <- dplyr::case_when(
      !is.na(da) ~ "days",
      !is.na(we) ~ "weeks",
      !is.na(mo) ~ "months",
      TRUE       ~ "years"
    )
    type <- forcats::fct_drop(factor(type, c("days", "weeks", "months", "years")))
    res <- tibble::add_column(dat, age_category = res, age_unit = type)
  }
  res

}
