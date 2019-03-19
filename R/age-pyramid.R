#' Plot a population pyramid (age-sex) from a dataframe.
#'
#' @param data Your dataframe (e.g. linelist)
#' @param age_group the name of a column in the data frame that defines the age
#'   group categories. Defaults to "age_group"
#' @param split_by the name of a column in the data frame that defines the
#'   the bivariate column. Defaults to "sex"
#' @param stack_by the name of the column in the data frame to use for shading
#'   the bars
#' @param proportional If `TRUE`, bars will represent proportions of cases out
#'   of the entire population. Otherwise (`FALSE`, default), bars represent
#'   case counts
#' @param na.rm  If `TRUE`, this removes NA counts from the age groups. Defaults
#'   to `TRUE`.
#' @param vertical_lines If you would like to add dashed vertical lines to help
#' visual interpretation of numbers. Default is to not show (`FALSE`),
#' to turn on write `TRUE`.
#' @param horizontal lines If `TRUE` (default), horizontal dashed lines will
#'   appear behind the bars of the pyramid
#'
#' @note if `split_by` and `stack_by` are not the same, The values of `spit_by`
#'   will show up as labels at the top of the pyramid.
#' @import ggplot2
#' @importFrom scales percent
#' @export
#' @examples
#' library(ggplot2)
#'
#' set.seed(2018-01-15)
#' ages <- cut(sample(80, 150, replace = TRUE),
#'             breaks = c(0, 5, 10, 30, 90), right = FALSE)
#' sex  <- sample(c("Female", "Male"), 150, replace = TRUE)
#' ill  <- sample(c("case", "non-case"), 150, replace = TRUE)
#' dat  <- data.frame(AGE = ages, sex = sex, ill = ill, stringsAsFactors = FALSE)
#'
#' # Create the age pyramid, stratifying by sex
#' print(ap   <- plot_age_pyramid(dat, age_group = "AGE"))
#'
#' # Remove NA categories with na.rm = TRUE
#' dat2 <- dat
#' dat2[1, 1] <- NA
#' dat2[2, 2] <- NA
#' dat2[3, 3] <- NA
#' print(ap   <- plot_age_pyramid(dat2, age_group = "AGE"))
#' print(ap   <- plot_age_pyramid(dat2, age_group = "AGE", na.rm = TRUE))
#'
#' # Stratify by case definition and customize with ggplot2
#' ap   <- plot_age_pyramid(dat, age_group = "AGE", split_by = "ill") +
#'   theme_bw(base_size = 16) +
#'   labs(title = "Age groups by case definition")
#' print(ap)
#'
#' # Stratify by multiple factors
#' ap <- plot_age_pyramid(dat,
#'                        age_group = "AGE",
#'                        split_by = "sex",
#'                        stack_by = "ill",
#'                        vertical_lines = TRUE) +
#'   labs(title = "Age groups by case definition and sex")
#' print(ap)
#' 
#' # Display proportions
#' ap <- plot_age_pyramid(dat,
#'                        age_group = "AGE",
#'                        split_by = "sex",
#'                        stack_by = "ill",
#'                        proportional = TRUE,
#'                        vertical_lines = TRUE) +
#'   labs(title = "Age groups by case definition and sex")
#' print(ap)
plot_age_pyramid <- function(data, age_group = "age_group", split_by = "sex",
                             stack_by = split_by, proportional = FALSE, na.rm = FALSE,
                             vertical_lines = FALSE, horizontal_lines = TRUE) {
  is_df <- is.data.frame(data)
  is_svy <- inherits(data, "tbl_svy")
  impt_columns <- (is_df || is_svy) &&  c(age_group, split_by, stack_by) %in% colnames(data)
  if (!impt_columns) {
    if (is_df || is_svy) {
      sprintf("The columns %s, %s, and %s were not found in %s",
              age_group, split_by, stack_by, deparse(substitute(data)))
    } else { 
      sprintf("%s must be a data frame or %s object",
              deparse(substitute(data)))
    }
  }
  ag <- rlang::sym(age_group)
  sb <- rlang::sym(split_by)
  st <- rlang::sym(stack_by)
  if (is_df) {
    if (!is.character(data[[split_by]]) || !is.factor(data[[split_by]])) {
      data[[split_by]] <- as.character(data[[split_by]])
    }
    if (!is.character(data[[stack_by]]) || !is.factor(data[[stack_by]])) {
      data[[stack_by]] <- as.character(data[[stack_by]])
    }
    if (anyNA(data[[split_by]]) || anyNA(data[[stack_by]])) {
      nas <- is.na(data[[split_by]]) | is.na(data[[stack_by]])
      warning(sprintf("removing %d observations with missing values between the %s and %s columns.",
                      sum(nas), split_by, stack_by))
      data <- data[!nas, , drop = FALSE]
    }
    if (na.rm) {
      nas <- is.na(data[[age_group]])
      warning(sprintf("removing %d observations with missing values from the %s column.",
                      sum(nas), age_group))
      data <- data[!nas, , drop = FALSE]
    } else {
      data[[age_group]] <- forcats::fct_explicit_na(data[[age_group]])
    }
    plot_data <- tidyr::complete(data, !!ag) # make sure all factors are represented
    plot_data <- dplyr::group_by(plot_data, !!ag, !!sb, !!st)
    plot_data <- dplyr::summarise(plot_data, n = dplyr::n())
    plot_data <- dplyr::ungroup(plot_data)
  } else {
    plot_data <- srvyr::group_by(data, !!ag, !!sb, !!st)
    plot_data <- srvyr::summarise(plot_data,
                                  n = srvyr::survey_total(vartype = "ci", level = 0.95))

  }
  if (proportional) {
    plot_data$n <- plot_data$n / sum(plot_data$n, na.rm = TRUE)
  }
  # find the maximum x axis position
  max_n <- dplyr::group_by(plot_data, !!ag, !!sb)
  max_n <- dplyr::summarise(max_n, n = sum(abs(!!quote(n))))
  max_n <- max(max_n[["n"]])
  # make sure the x axis is a multiple of ten
  if (proportional) {
    max_n <- ceiling(max_n * 100)
    max_n <- max_n + if (max_n %% 10 == 0) 0 else (10 - max_n %% 10)
    step_size <- if (max_n > 25) 0.1 else if (max_n > 15) 0.05 else 0.01
    max_n <- max_n / 100
    lab_fun <- function(i) scales::percent(abs(i))
    y_lab <- "proportion"
  } else {
    max_n <- max_n + if (max_n %% 10 == 0) 0 else (10 - max_n %% 10)
    step_size <- ceiling(max_n / 5)
    lab_fun <- abs
    y_lab <- "counts"
  }
  stopifnot(is.finite(max_n), max_n > 0)

  age_levels <- levels(plot_data[[age_group]])
  max_age_group <- age_levels[length(age_levels)]
  sex_levels <- unique(plot_data[[split_by]])
  stk_levels <- unique(plot_data[[stack_by]])

  stopifnot(length(sex_levels) >= 1L, length(sex_levels) <= 2L)

  plot_data[["n"]] <- ifelse(plot_data[[split_by]] == sex_levels[[1L]], -1L, 1L) * plot_data[["n"]]
  the_breaks <- seq(0, max_n, step_size)
  the_breaks <- c(-rev(the_breaks[-1]), the_breaks)

  pyramid <- ggplot(plot_data) +
    aes(x = !!ag, y = !!quote(n), group = !!sb, fill = !!st) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = incidence::incidence_pal1(length(stk_levels))) +
    scale_y_continuous(limits = c(-max_n, max_n),
                       breaks = the_breaks,
                       labels = lab_fun) +
theme_classic() +
theme(axis.line.y.left = element_blank()) +
labs(y = y_lab)

  if (vertical_lines == TRUE) {
    pyramid <- pyramid +
      geom_hline(yintercept = c(seq(-max_n, max_n, step_size)), linetype = "dotted", colour = "grey")
  }

if (horizontal_lines == TRUE) {
  pyramid <- pyramid + theme(panel.grid.major.y = element_line(linetype = 2))
}
pyramid <- pyramid +
  geom_hline(yintercept = 0) # add vertical line

if (stack_by != split_by) {
  pyramid <- pyramid +
    annotate(geom = "label",
             x = max_age_group,
             y = -step_size,
             vjust = 0.5,
             hjust = 1,
             label = sex_levels[[1]]) +
annotate(geom = "label",
         x = max_age_group,
         y = step_size,
         vjust = 0.5,
         hjust = 0,
         label = sex_levels[[2]])
}
pyramid
}
