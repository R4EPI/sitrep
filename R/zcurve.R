#' Create a curve comparing observed Z-scores to the WHO standard.
#'
#' @param x a data frame
#' @param zscore bare name of a numeric vector containing computed zscores
#' @return a ggplot2 object that is customisable via the ggplot2 package.
#' @export
#' @examples
#' library("ggplot2")
#' set.seed(9)
#' dat <- data.frame(observed = rnorm(204) + runif(1),
#'                   skewed   = rnorm(204) + runif(1, 0.5)
#'                  ) # slightly skewed
#' zcurve(dat, observed) +
#'   labs(title = "Weight-for-Height Z-scores") +
#'   theme_classic()
#'
#' zcurve(dat, skewed) +
#'   labs(title = "Weight-for-Height Z-scores") +
#'   theme_classic()
#'
#' # Including different groups to facet
#' dat <- data.frame(
#'   observed = c(rnorm(204) + runif(1), rnorm(204) + runif(1, 0.5)),
#'   groups   = rep(c("A", "B"), each = 204),
#'   treat    = sample(c('up', 'down'), 408, replace = TRUE)
#'                  )
#' zcurve(dat, observed) +
#'   facet_grid(treat~groups)
zcurve <- function(x, zscore) {

  if (!is.data.frame(x)) {
    stop("x must be a data frame")
  }

  zsc <- tidyselect::vars_pull(names(x), !! rlang::enquo(zscore))

  if (!is.numeric(x[[zsc]])) {
    stop("zscore must be a numeric variable")
  }

  ggplot(x) +
    stat_density(aes(x = !! rlang::enquo(zscore), color = "Observed"), size = 1,
                 geom = "line") +
    stat_function(fun     = stats::dnorm,
                  args    = list(mean = 0, sd = 1), size = 1,
                  mapping = aes(color = "WHO standard")
                 ) +
    scale_color_manual("",
                       values = c("Observed" = "red",
                                  "WHO standard" = "dark grey")
    ) +
    scale_x_continuous(limits = c(-6, 6), expand = expand_scale()) +
    scale_y_continuous(labels = scales::percent_format(), expand = expand_scale(add = 0.02)) +
    labs(
         x = "Z-score",
         y = sprintf("Proportion of children\n(n = %d)", nrow(x))
    )

}
