#' Create a curve comparing observed Z-scores to the WHO standard.
#'
#' @param zscore a numeric vector containing computed zscores
#' @return a ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot aes stat_function geom_density scale_x_continuous labs
#' @examples
#' dat <- rnorm(204) + runif(1) # slightly skewed
#' zcurve(dat)
zcurve <- function(zscore) {

  stopifnot(is.numeric(zscore))
  dat <- data.frame(observed = zscore)
  obs <- quote(observed)
  ggplot(dat) +
    geom_density(aes(x = !!obs, color = "observed")) +
    stat_function(fun = stats::dnorm, 
                  args = list(mean = 0, sd = 1),
                  mapping = aes(color = "WHO standard")
                 ) +
    scale_color_manual("",
                       values = c("observed" = "red",
                                  "WHO standard" = "black") 
    ) +
    scale_x_continuous(limits = c(-6, 6)) + 
    labs(
         x = "Z-score",
         y = sprintf("%% of cases\n(n = %d)", length(zscore))
    )
  
}
