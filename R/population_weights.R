#' Add a column of weights to a data frame
#'
#' @param x a data frame of survey data
#' @param p a data frame containing pouplation data for groups in `...`
#' @param ... shared grouping columns across both `x` and `p`. These are used
#'   to match the weights to the correct subset of the pouplation.
#' @param population the column in `p` that defines the pouplation numbers
#' @param weight the name of the new column to store the weights. Defaults to
#'   "weight".
#' @param weight_ID the name of the new ID column to be created. Defaults to
#'   "weight_ID"
#' @author Zhian N. Kamvar Alex Spina
#' @export
add_weights <- function(x, p, ..., population = population, weight = weight, weight_ID = weight_ID) {

  .dots      <- rlang::enquos(...)
  population <- rlang::enquo(population)
  weight_ID  <- rlang::enquo(weight_ID)
  weight     <- rlang::enquo(weight)

  # create a merger ID by age group and sex
  p <- tidyr::unite(p, "ID", !!! .dots)

  # get study sample counts
  counts <- dplyr::count(x, !!! .dots)
  counts <- tidyr::unite(counts, "ID", !!! .dots)
  counts <- dplyr::select(counts, .data$ID, .data$n)

  # merge counts to population data
  p <- dplyr::left_join(p, counts, by = "ID")

  # create weight variable
  p <- dplyr::mutate(p, !! weight := !! population / .data$n)
  p <- dplyr::select(p, .data$ID, !! weight)


  # merge to study sample
  merge_by <- "ID"
  names(merge_by) <- as.character(rlang::expr(weight_ID))
  d <- tidyr::unite(x, !! weight_ID, !!! .dots, remove = FALSE)
  d <- dplyr::left_join(d, p, by = merge_by)
  d


}
