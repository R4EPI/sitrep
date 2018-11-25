#' Plot an age group
#'
#' @import ggplot2
#' @export
plot_age_pyramid <- function(data) {
  stopifnot(is.data.frame(data), c("age_group", "sex") %in% colnames(data))
  plot_data <- dplyr::group_by(data, .data$age_group, .data$sex)
  plot_data <- dplyr::summarise(plot_data, n = n())
  max_n <- max(plot_data[["n"]])
  stopifnot(is.finite(max_n), max_n > 0)
  step_size <- ceiling(max_n / 5)
  sex_levels <- unique(data[["sex"]])
  stopifnot(length(sex_levels) >= 1L, length(sex_levels) <= 2L)
  plot_data[["n"]] <- ifelse(plot_data[["sex"]] == sex_levels[[1L]], -1L, 1L) * plot_data[["n"]]
  ggplot(plot_data) +
    aes(x = age_group, y = n, fill = sex) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = incidence::incidence_pal1(length(sex_levels))) +
    scale_y_continuous(limits = c(-max_n, max_n), breaks = seq(-max_n, max_n, step_size),
                       label = abs(seq(-max_n, max_n, step_size)))
}
