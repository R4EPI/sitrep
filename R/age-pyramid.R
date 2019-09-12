#' Plot a population pyramid (age-sex) from a dataframe.
#'
#' @param data Your dataframe (e.g. linelist)
#'
#' @param age_group the name of a column in the data frame that defines the age
#'   group categories. Defaults to "age_group"
#'
#' @param split_by the name of a column in the data frame that defines the
#'   the bivariate column. Defaults to "sex". See NOTE
#'
#' @param stack_by the name of the column in the data frame to use for shading
#'   the bars
#'
#' @param proportional If `TRUE`, bars will represent proportions of cases out
#'   of the entire population. Otherwise (`FALSE`, default), bars represent
#'   case counts
#'
#' @param na.rm  If `TRUE`, this removes NA counts from the age groups. Defaults
#'   to `TRUE`.
#'
#' @param show_halfway When `TRUE` (default), a dashed vertical line will be
#'   added to each of the age bars showing the halfway point for the
#'   un-stratified age group. When `FALSE`, no halfway point is marked.
#'
#' @param compare_group sdflkjsdlfkjsldkjfl
#'
#' @param vertical_lines If you would like to add dashed vertical lines to help
#' visual interpretation of numbers. Default is to not show (`FALSE`),
#' to turn on write `TRUE`.
#'
#' @param horizontal_lines If `TRUE` (default), horizontal dashed lines will
#'   appear behind the bars of the pyramid
#'
#' @param pyramid if `TRUE`, then binary `split_by` variables will result in 
#'   a population pyramid (non-binary variables cannot form a pyramid). If 
#'   `FALSE`, a pyramid will not form.
#'
#' @param pal a color palette function or vector of colors to be passed to
#'   [ggplot2::scale_fill_manual()] defaults to the first "qual" palette from
#'   [ggplot2::scale_fill_brewer()].
#'
#' @note If the `split_by` variable is bivariate (e.g. an indicator for
#' pregnancy), then the result will show up as a pyramid, otherwise, it will be
#' presented as a facetted barplot with with empty bars in the background
#' indicating the range of the un-facetted data set. Values of `spit_by` will
#' show up as labels at top of each facet.
#'
#' @import ggplot2
#' @importFrom scales percent
#' @importFrom rlang !! enquo .data :=
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
#' plot_age_pyramid(dat3, age_group = AGE, na.rm = TRUE) 
#' plot_age_pyramid(dat3, age_group = AGE) 
plot_age_pyramid <- function(data, age_group = "age_group", split_by = "sex",
                             stack_by = split_by, proportional = FALSE, na.rm = FALSE,
                             compare_group = 1,
                             show_halfway = TRUE, vertical_lines = FALSE, 
                             horizontal_lines = TRUE, pyramid = TRUE, 
                             pal = NULL) {
  
  is_df     <- is.data.frame(data)
  is_svy    <- inherits(data, "tbl_svy")
  age_group <- tidyselect::vars_select(colnames(data), !! enquo(age_group))
  split_by  <- tidyselect::vars_select(colnames(data), !! enquo(split_by))
  stack_by  <- tidyselect::vars_select(colnames(data), !! enquo(stack_by))

  if (!is_df && !is_svy) {
    msg <- sprintf("%s must be a data frame or  object", deparse(substitute(data)))
    stop(msg)
  }

  group <- rlang::sym(age_group)
  split <- rlang::sym(split_by)
  fill  <- rlang::sym(stack_by)
  cfill <- rlang::sym(glue::glue("__{as.integer(Sys.time())}__{stack_by}"))

  # Count the plot data --------------------------------------------------------
  plot_data <- count_age_categories(data,
                                    age_group, 
                                    split_by, 
                                    stack_by, 
                                    proportional, 
                                    na.rm)
  max_n <- max(plot_data[["n"]])
  # gathering the levels for each of the elements ------------------------------ 
  age_levels    <- levels(plot_data[[age_group]])
  max_age_group <- age_levels[length(age_levels)]

  # Splitting levels without missing data
  split_levels  <- plot_data[[split_by]]
  split_levels  <- if (is.factor(split_levels)) levels(split_levels) else unique(split_levels)
  split_levels  <- split_levels[!is.na(split_levels)]

  if (is.numeric(compare_group)) {
    compare_group <- split_levels[compare_group]
  } 
  if (!compare_group %in% split_levels) {
    slevels <- glue::glue_collapse(split_levels, sep = ", ", last = ", or ")
    stop(glue::glue("`compare_group` ({compare_group}) must be one of {slevels}"))
  }

  # Stacking levels assuming there is no missing data
  stk_levels    <- plot_data[[stack_by]]
  stk_levels    <- if (is.factor(stk_levels)) levels(stk_levels) else unique(stk_levels)

  stopifnot(length(split_levels) >= 1L)

  # Switch between pyramid and non-pyramid shape -------------------------------
  # This will only result in a pyramid if the user specifies so AND the split
  # levels is binary.
  split_measured_binary <- pyramid && length(split_levels) == 2L

  midpoint <- function(u, l) u - (abs(u - l) / 2L)

  background <- dplyr::group_by(plot_data, !!group)
  background <- dplyr::mutate(background, N = sum(.data$n))
  background <- dplyr::filter(background, !!split == compare_group)
  background <- dplyr::mutate(background,
                              upper = .data$N - sum(.data$n),
                              lower = -1L     * .data$n)


  compare_bg <- dplyr::select(background, 
                              !!group, 
                              !!split, 
                              !!fill, 
                              n = .data$lower)
  compare_bg <- dplyr::mutate(compare_bg, !!cfill := !!fill)
  compare_bg <- dplyr::rename_at(compare_bg, vars(split_by), toupper)

  plot_bg <- tidyr::gather(background, key = "part", value = "n", 
                           .data$upper, .data$lower)
  plot_bg <- dplyr::select(plot_bg, -!!split)
  plot_bg <- dplyr::distinct(plot_bg, !!group, .data$n, .keep_all = TRUE)
  plot_bg[["zzzz_alpha"]] <- "Total"

  max_n <- dplyr::group_by(plot_data, !!group, !!split)
  max_n <- dplyr::summarise(max_n, n = sum(.data$n))
  max_n <- max(abs(max_n[["n"]]))

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

  cmpr_labeller <- function(x, cmpr = compare_group) {
    glue::glue("{cmpr} vs {x}")
  }

  plot_data <- dplyr::filter(plot_data, !!split != compare_group)
  pyramid <- ggplot(plot_data, aes(x = !!group, y = .data$n)) 
  
  if (!split_measured_binary) {
    pyramid <- pyramid + 
      geom_col(data = plot_bg, mapping = aes(alpha = .data$zzzz_alpha), 
               fill = "grey80", color = "grey20", position = "stack") +
      facet_wrap(split, labeller = as_labeller(cmpr_labeller)) +
      scale_alpha_manual(values = 0.5, 
                         guide = guide_legend(title = NULL, order = 3)) 
  }

  pyramid <- pyramid + 
    geom_col(mapping = aes(fill = !!fill), 
             position = "stack", color = "grey20") +
    geom_col(data = compare_bg, mapping = aes(fill = !!cfill), 
             color = "grey20", position = "stack", show.legend = FALSE) +
    geom_hline(yintercept = 0) +
    coord_flip()

  if (show_halfway) {
    maxdata <- dplyr::arrange(background, !!group)
    maxdata <- dplyr::group_by(maxdata, !!group)
    maxdata <- dplyr::summarise(maxdata, 
                                upper = max(.data$upper), 
                                lower = sum(.data$lower))
    maxdata <- dplyr::mutate(maxdata, center = midpoint(.data$upper, .data$lower))
    maxdata <- dplyr::select(maxdata, !!group, .data$center)
    maxdata[['x']]       <- seq_along(maxdata[[age_group]]) - 0.25
    maxdata[['xend']]    <- maxdata[['x']] + 0.5
    maxdata[['halfway']] <- 'midpoint'
    pyramid <- pyramid + 
      geom_segment(aes(x        = .data$x,
                       xend     = .data$xend,
                       y        = .data$center,
                       yend     = .data$center,
                       linetype = .data$halfway),
                   color     = "grey20",
                   key_glyph = "vpath", # NOTE: key_glyph is only part of ggplot2 >= 2.3.0; this will warn otherwise
                   data      = maxdata) +
      scale_linetype_manual(values = 'dashed', guide = guide_legend(title = NULL, order = 2))
    
  }
  if (is.null(pal)) {
    pyramid <- pyramid + scale_fill_brewer(type = "qual", guide = guide_legend(order = 1)) 
  } else {
    pyramid <- pyramid + scale_fill_manual(values = pal, guide = guide_legend(order = 1))
  }

  if (!split_measured_binary || split_by != stack_by) {

    pd              <- dplyr::distinct(plot_data, .data[[split_by]])
    pd[[age_group]] <- max_age_group
    pd$n            <- step_size

    pyramid <- pyramid +
      geom_label(data = pd, 
                 mapping = aes(label = .data[[split_by]]),
                 vjust = 0.5,
                 label.padding = grid::unit(0.5, "lines"),
                 hjust = 0) +
      annotate(geom  = "label",
               x     = max_age_group,
               y     = -step_size,
               vjust = 0.5,
               hjust = 1,
               label.padding = grid::unit(0.5, "lines"),
               label = compare_group) 
  }

  the_breaks <- seq(0, max_n, step_size)
  the_breaks <- c(-rev(the_breaks[-1]), the_breaks)

  pyramid +
    scale_y_continuous(limits = c(-max_n, max_n),
                       breaks = the_breaks,
                       labels = lab_fun) +
    scale_x_discrete(drop = FALSE) # note: drop = FALSE important to avoid missing age groups

}


# This will count the age categories for us. I've pulled it out of the plot-age
count_age_categories <- function(data, age_group, split_by, stack_by, 
                                 proportional = FALSE, na.rm = FALSE) {

  ag <- rlang::sym(age_group)
  sb <- rlang::sym(split_by)
  st <- rlang::sym(stack_by)

  if (is.data.frame(data)) {
    data <- dplyr::select(data, !!ag, !!sb, !!st)
    if (na.rm) {
      missing <- dplyr::mutate_at(data,
                                  .vars = vars(!!ag, !!sb, !!st),
                                  .funs = is.na)
      sumissing <- colSums(missing)
      if (any(sumissing > 0)) {
        nmiss <- glue::glue(" {sumissing} values from {names(sumissing)} ")
        nmiss <- glue::glue_collapse(nmiss, sep = ",", last = "and")
        missing <- missing[[age_group]] | missing[[split_by]] | missing[[stack_by]]
        msg     <- glue::glue("{sum(missing)} missing rows were removed ({nmiss}).")
        warning(msg)
        data    <- data[!missing, , drop = FALSE]
      } else {
        # 🤷
      }
    } else {
      data <- dplyr::mutate_at(data,
                               .vars = vars(!!ag, !!sb, !!st),
                               .funs = forcats::fct_explicit_na, "Missing")
    }
    plot_data <- dplyr::count(data, !!ag, !!sb, !!st, .drop = FALSE)
  } else {
    plot_data <- srvyr::group_by(data, !!ag, !!sb, !!st, .drop = FALSE)
    plot_data <- srvyr::summarise(plot_data,
                                  n = srvyr::survey_total(vartype = "ci", level = 0.95))

  }
  if (proportional) {
    plot_data$n <- plot_data$n / sum(plot_data$n, na.rm = TRUE)
  }

  plot_data

}
