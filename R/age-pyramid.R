#' Plot a population pyramid (age-sex) from a dataframe.
#'
#' @param data Your dataframe (e.g. linelist)
#' @param age_group the name of a column in the data frame that defines the age
#'   group categories. Defaults to "age_group"
#' @param split_by the name of a column in the data frame that defines the
#'   the bivariate column. Defaults to "sex". See NOTE
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
#' @param horizontal_lines If `TRUE` (default), horizontal dashed lines will
#'   appear behind the bars of the pyramid
#' @param pyramid if `TRUE`, then binary `split_by` variables will result in 
#'   a population pyramid (non-binary variables cannot form a pyramid). If 
#'   `FALSE`, a pyramid will not form.
#'
#' @note If the `split_by` variable is bivariate (e.g. an indicator for
#' pregnancy), then the result will show up as a pyramid, otherwise, it will be
#' presented as a facetted barplot with with empty bars in the background
#' indicating the range of the un-facetted data set. Values of `spit_by` will
#' show up as labels at top of each facet.
#'#'#'
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
#' gender <- sex
#' gender[sample(5)] <- "NB"
#' ill  <- sample(c("case", "non-case"), 150, replace = TRUE)
#' dat  <- data.frame(AGE              = ages,
#'                    sex              = sex,
#'                    gender           = gender,
#'                    ill              = ill,
#'                    stringsAsFactors = FALSE)
#'
#' # Create the age pyramid, stratifying by sex
#' print(ap   <- plot_age_pyramid(dat, age_group = AGE))
#' 
#' # Create the age pyramid, stratifying by gender, which can include non-binary
#' print(apg  <- plot_age_pyramid(dat, age_group = AGE, split_by = gender))
#'
#' # Remove NA categories with na.rm = TRUE
#' dat2 <- dat
#' dat2[1, 1] <- NA
#' dat2[2, 2] <- NA
#' dat2[3, 3] <- NA
#' print(ap   <- plot_age_pyramid(dat2, age_group = AGE))
#' print(ap   <- plot_age_pyramid(dat2, age_group = AGE, na.rm = TRUE))
#'
#' # Stratify by case definition and customize with ggplot2
#' ap   <- plot_age_pyramid(dat, age_group = AGE, split_by = ill) +
#'   theme_bw(base_size = 16) +
#'   labs(title = "Age groups by case definition")
#' print(ap)
#'
#' # Stratify by multiple factors
#' ap <- plot_age_pyramid(dat,
#'                        age_group = AGE,
#'                        split_by = sex,
#'                        stack_by = ill,
#'                        vertical_lines = TRUE) +
#'   labs(title = "Age groups by case definition and sex")
#' print(ap)
#' 
#' # Display proportions
#' ap <- plot_age_pyramid(dat,
#'                        age_group = AGE,
#'                        split_by = sex,
#'                        stack_by = ill,
#'                        proportional = TRUE,
#'                        vertical_lines = TRUE) +
#'   labs(title = "Age groups by case definition and sex")
#' print(ap)
#' 
#' # empty group levels will still be displayed
#' dat3 <- dat2
#' dat3[dat$AGE == "[0,5)", "sex"] <- NA
#' plot_age_pyramid(dat3, age_group = AGE) 
plot_age_pyramid <- function(data, age_group = "age_group", split_by = "sex",
                             stack_by = split_by, proportional = FALSE, na.rm = FALSE,
                             vertical_lines = FALSE, horizontal_lines = TRUE,
                             pyramid = TRUE) {
  
  is_df     <- is.data.frame(data)
  is_svy    <- inherits(data, "tbl_svy")
  age_group <- tidyselect::vars_select(colnames(data), !! enquo(age_group))
  split_by  <- tidyselect::vars_select(colnames(data), !! enquo(split_by))
  stack_by  <- tidyselect::vars_select(colnames(data), !! enquo(stack_by))

  if (!is_df && !is_svy) {
    msg <- sprintf("%s must be a data frame or  object", deparse(substitute(data)))
    stop(msg)
  }

  ag        <- rlang::sym(age_group)
  sb        <- rlang::sym(split_by)
  st        <- rlang::sym(stack_by)
  plot_data <- count_age_categories(data,
                                    age_group, 
                                    split_by, 
                                    stack_by, 
                                    proportional, 
                                    na.rm)

  age_levels    <- levels(plot_data[[age_group]])
  max_age_group <- age_levels[length(age_levels)]
  sex_levels    <- unique(plot_data[[split_by]])
  sex_levels    <- sex_levels[!is.na(sex_levels)]
  stk_levels    <- unique(plot_data[[stack_by]])
  stopifnot(length(sex_levels) >= 1L)#, length(sex_levels) <= 2L)
  sex_measured_binary <- pyramid && length(sex_levels) == 2L

  if (sex_measured_binary) {
  # find the maximum x axis position
    max_n <- dplyr::group_by(plot_data, !!ag, !!sb, .drop = FALSE)
    max_n <- dplyr::summarise(max_n, n = sum(abs(!!quote(n))))
    max_n <- max(max_n[["n"]])
  } else {
    maxdata <- dplyr::group_by(plot_data, !! ag, .drop = FALSE)
    maxdata <- dplyr::tally(maxdata, wt = !! quote(n))
    max_n   <- max(maxdata$n, na.rm = TRUE)
  }

  # make sure the x axis is a multiple of ten
  if (proportional) {
    max_n     <- ceiling(max_n * 100)
    max_n     <- max_n + if (max_n %% 10 == 0) 0 else (10 - max_n %% 10)
    step_size <- if (max_n > 25) 0.1 else if (max_n > 15) 0.05 else 0.01
    max_n     <- max_n / 100
    lab_fun   <- function(i) scales::percent(abs(i))
    y_lab     <- "proportion"
  } else {
    max_n     <- max_n + if (max_n %% 10 == 0) 0 else (10 - max_n %% 10)
    step_size <- ceiling(max_n / 5)
    lab_fun   <- abs
    y_lab     <- "counts"
  }

  the_breaks    <- seq(0, max_n, step_size)
  the_breaks    <- c(-rev(the_breaks[-1]), the_breaks)
  stopifnot(is.finite(max_n), max_n > 0)


  if (sex_measured_binary) {
    plot_data[["n"]] <- ifelse(plot_data[[split_by]] == sex_levels[[1L]], -1L, 1L) * plot_data[["n"]]
  }

  pyramid <- ggplot(plot_data) +
    aes(x = !!ag, y = !!quote(n)) 

  if (!sex_measured_binary) {
    pyramid <- pyramid + geom_col(color = "grey20", fill = "grey80", alpha = 0.5, data = maxdata)
  }
  pyramid <- pyramid + 
    geom_col(aes(group = !!sb, fill = !!st), color = "grey20") +
    coord_flip() +
    scale_fill_manual(values  = incidence::incidence_pal1(length(stk_levels))) +
    scale_y_continuous(limits = if (sex_measured_binary) c(-max_n, max_n) else c(0, max_n),
                       breaks = the_breaks,
                       labels = lab_fun) +
    scale_x_discrete(drop = FALSE) + 
    theme_classic() +
    theme(axis.line.y.left = element_blank()) +
    labs(y = y_lab)

  if (!sex_measured_binary) {
    pyramid <- pyramid + facet_wrap(split_by)
  }
  if (vertical_lines == TRUE) {
    pyramid <- pyramid +
      geom_hline(yintercept = c(seq(-max_n, max_n, step_size)), linetype = "dotted", colour = "grey")
  }

  if (horizontal_lines == TRUE) {
    pyramid <- pyramid + theme(panel.grid.major.y = element_line(linetype = 2))
  }
  pyramid <- pyramid +
    geom_hline(yintercept = 0) # add vertical line

  if (sex_measured_binary && stack_by != split_by) {
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


# This will count the age categories for us. I've pulled it out of the plot-age
count_age_categories <- function(data, age_group, split_by, stack_by, proportional, na.rm) {

  ag <- rlang::sym(age_group)
  sb <- rlang::sym(split_by)
  st <- rlang::sym(stack_by)

  if (is.data.frame(data)) {
    sbv <- data[[split_by]]
    stv <- data[[stack_by]]
    if (!is.character(sbv) || !is.factor(sbv)) {
      data[[split_by]] <- as.character(sbv)
    }
    if (!is.character(stv) || !is.factor(stv)) {
      data[[stack_by]] <- as.character(stv)
    }
    if (anyNA(sbv) || anyNA(stv)) {
      nas <- is.na(sbv) | is.na(stv)
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
    # plot_data <- tidyr::complete(data, !!ag) # make sure all factors are represented
    plot_data <- dplyr::group_by(data, !!ag, !!sb, !!st, .drop = FALSE)
    plot_data <- dplyr::summarise(plot_data, n = dplyr::n())
    plot_data <- dplyr::ungroup(plot_data)
  } else {
    plot_data <- srvyr::group_by(data, !!ag, !!sb, !!st, .drop = FALSE)
    plot_data <- srvyr::summarise(plot_data,
                                  n = srvyr::survey_total(vartype = "ci", level = 0.95))

  }
  # Remove any missing values
  to_delete <- is.na(plot_data[[split_by]]) & is.na(plot_data[[stack_by]]) & plot_data[["n"]] == 0
  plot_data <- plot_data[!to_delete, , drop = FALSE]

  if (proportional) {
    plot_data$n <- plot_data$n / sum(plot_data$n, na.rm = TRUE)
  }

  plot_data

}
