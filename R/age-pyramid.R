#' Plot an age group
#'
#' @import ggplot2
#' @export
plot_age_pyramid <- function(data) {
  stopifnot(is.data.frame(data), c("age_group", "sex") %in% colnames(data))
  ggplot(data) +
    aes(x = age_group, y = n, fill = sex) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(limits = c(-40, 40), breaks = seq(-40, 40, 10), label = abs(seq(-40, 40, 10)))
}
