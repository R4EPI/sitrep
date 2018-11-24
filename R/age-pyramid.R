#' Plot an age group
#'
#' @import ggplot2
#' @export
plot_age_pyramid <- function(data) {
  stopifnot(is.data.frame(data), c("age_group", "sex") %in% colnames(data))
  plot_data <- dplyr::group_by(data, .data$age_group, .data$sex)
  plot_data <- dplyr::summarise(plot_data, n = n())
  sex_levels <- unique(data[["sex"]])
  stopifnot(length(sex_levels) >= 1L, length(sex_levels) <= 2L)
  plot_data[["n"]] <- ifelse(plot_data[["sex"]] == sex_levels[[1L]], -1L, 1L) * plot_data[["n"]]
  ggplot(plot_data) +
    aes(x = age_group, y = n, fill = sex) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(limits = c(-40, 40), breaks = seq(-40, 40, 10), label = abs(seq(-40, 40, 10)))
}
