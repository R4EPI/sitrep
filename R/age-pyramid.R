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
#' @param show_halfway When `TRUE` (default), a dashed vertical line will be
#'   added to each of the age bars showing the halfway point for the
#'   un-stratified age group. When `FALSE`, no halfway point is marked.
#' @param vertical_lines If you would like to add dashed vertical lines to help
#' visual interpretation of numbers. Default is to not show (`FALSE`),
#' to turn on write `TRUE`.
#' @param horizontal_lines If `TRUE` (default), horizontal dashed lines will
#'   appear behind the bars of the pyramid
#' @param pyramid if `TRUE`, then binary `split_by` variables will result in 
#'   a population pyramid (non-binary variables cannot form a pyramid). If 
#'   `FALSE`, a pyramid will not form.
#' @param pal a color palette function or vector of colors to be passed to
#'   [ggplot2::scale_fill_manual()] defaults to the "Geyser" palette from
#'   [hcl.pals()].
#'
#' @note If the `split_by` variable is bivariate (e.g. an indicator for
#' pregnancy), then the result will show up as a pyramid, otherwise, it will be
#' presented as a facetted barplot with with empty bars in the background
#' indicating the range of the un-facetted data set. Values of `spit_by` will
#' show up as labels at top of each facet.
#'
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
#'                    sex              = factor(sex, c("Male", "Female")),
#'                    gender           = factor(gender, c("Male", "NB", "Female")),
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
                             show_halfway = TRUE, vertical_lines = FALSE, 
                             horizontal_lines = TRUE, pyramid = TRUE, 
                             pal = function(n) hcl.colors(n, "Geyser")) {
  
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

  # Count the plot data --------------------------------------------------------
  plot_data <- count_age_categories(data,
                                    age_group, 
                                    split_by, 
                                    stack_by, 
                                    proportional, 
                                    na.rm)

  # gathering the levels for each of the elements ------------------------------ 
  age_levels    <- levels(plot_data[[age_group]])
  max_age_group <- age_levels[length(age_levels)]

  # Splitting levels without missing data
  split_levels  <- plot_data[[split_by]]
  split_levels  <- if (is.factor(split_levels)) levels(split_levels) else unique(split_levels)
  split_levels  <- split_levels[!is.na(split_levels)]

  # Stacking levels assuming there is no missing data
  stk_levels    <- plot_data[[stack_by]]
  stk_levels    <- if (is.factor(stk_levels)) levels(stk_levels) else unique(stk_levels)

  stopifnot(length(split_levels) >= 1L)

  # Switch between pyramid and non-pyramid shape -------------------------------
  # This will only result in a pyramid if the user specifies so AND the split
  # levels is binary.
  split_measured_binary <- pyramid && length(split_levels) == 2L

  if (split_measured_binary) {
    maxdata <- dplyr::group_by(plot_data, !!ag, !!sb, .drop = FALSE)
  } else {
    maxdata <- dplyr::group_by(plot_data, !!ag, .drop = FALSE)
  }

  # find the maximum x axis position
  maxdata <- dplyr::tally(maxdata, wt = !! quote(n))
  max_n   <- max(abs(maxdata[["n"]]), na.rm = TRUE)

  # make sure the x axis is a multiple of ten. This took a lot of fiddling
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

  stopifnot(is.finite(max_n), max_n > 0)

  # Make sure the breaks are correct for the plot size
  the_breaks <- seq(0, max_n, step_size)
  the_breaks <- if (split_measured_binary) c(-rev(the_breaks[-1]), the_breaks) else the_breaks


  if (split_measured_binary) {
    # If the user has a binary level and wants to plot the data in a pyramid,
    # then we need to make the counts for the primary level negative so that
    # they appear to go to the left on the plot.
    plot_data[["n"]] <- ifelse(plot_data[[split_by]] == split_levels[[1L]], -1L, 1L) * plot_data[["n"]]
    maxdata[["d"]]   <- ifelse(maxdata[[split_by]] == split_levels[[1L]], -1L, 0L) * maxdata[["n"]]
    # If we are labelling the center, then we can get it by summing the values over the age groups
    maxdata          <- dplyr::summarise(maxdata, center = sum(!! quote(d)) + sum(!! quote(n)) / 2)
  } else {
    maxdata[["center"]] <- maxdata[["n"]] / 2
  }

  # Create base plot -----------------------------------------------------------
  pyramid <- ggplot(plot_data, aes(x = !!ag, y = !!quote(n)))
  pal     <- if (is.function(pal)) pal(length(stk_levels)) else pal

  if (!split_measured_binary) { 
    # add the background layer if the split is not binary
    maxdata[["zzzzz_alpha"]] <- 0.5
    pyramid <- pyramid + 
      geom_col(aes(alpha = !! quote(zzzzz_alpha)), fill = "grey80", color = "grey20", data = maxdata)
  }

  # Add bars, scales, and themes -----------------------------------------------
  pyramid <- pyramid + 
    geom_col(aes(group = !!sb, fill = !!st), color = "grey20") +
    coord_flip() +
    scale_fill_manual(values  = pal) +
    scale_y_continuous(limits = if (split_measured_binary) c(-max_n, max_n) else c(0, max_n),
                       breaks = the_breaks,
                       labels = lab_fun) +
    scale_x_discrete(drop = FALSE) + # note: drop = FALSE important to avoid missing age groups
    theme_classic() +
    theme(axis.line.y.left = element_blank()) +
    labs(y = y_lab)

  if (!split_measured_binary) {
    # Wrap the categories if the split is not binary
    pyramid <- pyramid + 
      facet_wrap(split_by) +
      scale_alpha_continuous(guide = guide_legend(label = FALSE)) +
      labs(alpha = "Total")
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

  if (show_halfway) {
    maxdata           <- dplyr::arrange(maxdata, !! ag)
    maxdata[['x']]    <- seq_along(maxdata[[age_group]]) - 0.25
    maxdata[['xend']] <- maxdata[['x']] + 0.5
    maxdata[['halfway']] <- 'dashed' 
    pyramid <- pyramid + 
      geom_segment(aes(x        = !! quote(x),
                       xend     = !! quote(xend),
                       y        = !! quote(center),
                       yend     = !! quote(center),
                       linetype = !! quote(halfway)),
                   color = "grey20",
                   data  = maxdata) +
      scale_linetype_identity(guide = guide_legend(label = FALSE))
    
  }

  if (split_measured_binary && stack_by != split_by) {
    # If the split is binary and we have both stacked and split data, then we
    # need to label the groups. We do so by adding a label annotation
    pyramid <- pyramid +
      annotate(geom  = "label",
               x     = max_age_group,
               y     = -step_size,
               vjust = 0.5,
               hjust = 1,
               label = split_levels[[1]]) +
      annotate(geom  = "label",
               x     = max_age_group,
               y     = step_size,
               vjust = 0.5,
               hjust = 0,
               label = split_levels[[2]])
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
    if (is.factor(sbv)) {
      plot_data[[split_by]] <- factor(plot_data[[split_by]], levels(sbv))
    }
    if (is.factor(stv)) {
      plot_data[[stack_by]] <- factor(plot_data[[stack_by]], levels(stv))
    }
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
