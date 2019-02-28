#' Create an age group variable
#' either give distinct breaks or specify vals for sequence
#' @param x Your age variable
#' @param breakers A string. Age category breaks you can define within c(). Alternatively use "lower",
#'  "upper" and "by" to set these breaks based on a sequence.
#' @param lower A number. The lowest age value you want to consider (default is 0)
#' @param upper A number. The highest age value you want to consider
#' @param by A number. The number of years you want between groups
#' @param separator A character that you want to have between ages in group names. The default is "-" producing e.g. 0-10.
#' @param ceiling A TRUE/FALSE variable. Specify whether you would like the highest value in your breakers, or alternatively
#' the upper value specified, to be the endpoint. This would produce the highest group of "70-80" rather than "80+".
#' The default is FALSE (to produce a group of 80+).
#' @param above.char Only considered when ceiling == FALSE.
#' A character that you want to have after your highest age group. The default is "+" producing e.g. 80+
#' @export


age_categories <- function(x, breakers = NA,
                           lower = 0, upper = NA,
                           by = 10,
                           separator = "-",
                           ceiling = FALSE,
                           above.char = "+") {


  # make sure age variable is numeric
  x <- as.numeric(x)
  right <- include.lowest <- ceiling

  if (length(breakers) == 1) {
    if (!is.na(breakers)) {
      stop("breakers must be at least three numbers")
    } else {
      breakers <- unique(seq(lower, upper, by = by), max(x, na.rm = TRUE))
    }
  }

  nb <- length(breakers)

  if (ceiling) {
    lower_vals <- breakers[c(-nb, -nb + 1)]
    upper_vals <- breakers[c(-1, -nb)] - 1
    final_val  <- sprintf("%d%s%d", breakers[nb - 1], separator, breakers[nb])
  } else {
    lower_vals <- breakers[-nb]
    upper_vals <- breakers[-1] - 1
    final_val  <- sprintf("%d%s", breakers[nb], above.char)
  }
  labs <- c(paste(lower_vals, upper_vals, sep = separator), final_val)

  # If there is no ceiling, Inf needs to be part of the breaks
  if (!ceiling) {
    breakers <- unique(c(breakers, Inf))
  }
  output <- cut(x, 
                breaks = breakers,
                right = right, 
                include.lowest = right,
                labels = labs
               )

  # return variable with groups
  output
}
