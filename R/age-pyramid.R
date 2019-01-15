#' Plot an age group
#'
#' @param data a data frame containing an age group column and a bivariate column 
#'   defining data to create either sides of the pyramid (defaults to "sex")
#' @param age_group the name of a column in the data frame that defines the age
#'   group. Defaults to "age_group"
#' @param split_by the name of a column in the data frame that defines the 
#'   the bivariate column. Defaults to "sex"
#' @import ggplot2
#' @export
#' @examples
#'
#' set.seed(2018-01-15)
#' ages <- cut(sample(80, 150, replace = TRUE),
#'             breaks = c(0, 5, 10, 30, 90), right = FALSE)
#' sex  <- sample(c("Female", "Male"), 150, replace = TRUE)
#' ill  <- sample(0:1, 150, replace = TRUE)
#' dat  <- data.frame(AGE = ages, sex = sex, ill = ill, stringsAsFactors = FALSE)
#' print(ap   <- plot_age_pyramid(dat, age_group = "AGE"))
#' print(ap   <- plot_age_pyramid(dat, age_group = "AGE", split_by = "ill"))
plot_age_pyramid <- function(data, age_group = "age_group", split_by = "sex") {
  stopifnot(is.data.frame(data), c(age_group, split_by) %in% colnames(data))
  data[[split_by]] <- as.character(data[[split_by]])
  ag <- rlang::sym(age_group)
  sb <- rlang::sym(split_by)
  plot_data <- tidyr::complete(data, !!ag) # make sure all factors are represented
  plot_data <- dplyr::group_by(plot_data, !!ag, !!sb)
  plot_data <- dplyr::summarise(plot_data, n = n())
  max_n <- max(plot_data[["n"]])
  stopifnot(is.finite(max_n), max_n > 0)
  step_size <- ceiling(max_n / 5)
  sex_levels <- unique(data[[split_by]])
  stopifnot(length(sex_levels) >= 1L, length(sex_levels) <= 2L)
  plot_data[["n"]] <- ifelse(plot_data[[split_by]] == sex_levels[[1L]], -1L, 1L) * plot_data[["n"]]
  the_breaks <- seq(0, max_n, step_size)
  the_breaks <- c(-rev(the_breaks[-1]), the_breaks)
  ggplot(plot_data) +
    aes(x = !!ag, y = !!quote(n), fill = !!sb) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = incidence::incidence_pal1(length(sex_levels))) +
    scale_y_continuous(limits = c(-max_n, max_n),
                       breaks = the_breaks,
                       label = abs(the_breaks))
}
