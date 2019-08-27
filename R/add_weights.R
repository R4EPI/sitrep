#' Add a column of stratified survey weights to a data frame.
#' For use in surveys where you took a sample population out of a larger
#' source population, with a simple-random or stratified survey design.
#'
#' Creates weight based on dividing stratified population counts from the source
#' population by surveyed counts in the sample population.
#'
#' @param x a data frame of survey data
#' @param p a data frame containing pouplation data for groups in `...`
#' @param ... shared grouping columns across both `x` and `p`. These are used
#'   to match the weights to the correct subset of the pouplation.
#' @param population the column in `p` that defines the pouplation numbers
#' @param surv_weight the name of the new column to store the weights. Defaults to
#'   "surv_weight".
#' @param surv_weight_ID the name of the new ID column to be created. Defaults to
#'   "surv_weight_ID"
#' @author Zhian N. Kamvar Alex Spina Lukas Richter
#' @export
#'
#' @examples
#'
#' # define a fake dataset of survey data
#' # including household and individual information
#' x <- tibble::tribble(
#'   ~cluster, ~household_id, ~eligibile_n, ~surveyed_n, ~individual_id, ~age_grp, ~sex, ~outcome,
#'   "Village A",             1,            6,           4,              1,   "0-10",  "Male",        "Y",
#'   "Village A",             1,            6,           4,              2,  "20-30",  "Female",      "Y",
#'   "Village A",             1,            6,           4,              3,  "30-40",  "Male",        "N",
#'   "Village A",             1,            6,           4,              4,  "50-60",  "Female",      "N",
#'   "Village A",             2,            6,           4,              4,  "50-60",  "Female",      "N",
#'   "Village B",             2,            3,           3,              1,  "20-30",  "Male",        "N",
#'   "Village B",             2,            3,           3,              2,  "50-60",  "Female",      "N",
#'   "Village B",             2,            3,           3,              3,  "30-40",  "Female",      "Y"
#' )
#'
#' # define a fake population data set
#' # including age group, sex, counts and proportions
#' p <- sitrep::gen_population(total = 10000,
#'  groups = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60"),
#'  proportions = c(0.1, 0.2, 0.3, 0.4, 0.2, 0.1)) %>%
#'  # make sure col names match survey dataset
#'  dplyr::rename(age_grp = groups,
#'  sex = strata,
#'  population = n)
#'
#' # add weights to a stratified simple random sample
#' # weight based on age group and sex
#' add_weights(x, p = p, age_grp, sex,
#' population = population)
#'


add_weights <- function(x, p, ..., population = population, surv_weight = surv_weight, surv_weight_ID = surv_weight_ID) {

  .dots      <- rlang::enquos(...)
  population <- rlang::enquo(population)
  surv_weight_ID  <- rlang::enquo(surv_weight_ID)
  surv_weight     <- rlang::enquo(surv_weight)

  # create a merger ID by age group and sex
  p <- tidyr::unite(p, "ID", !!! .dots)

  # get study sample counts
  counts <- dplyr::count(x, !!! .dots)
  counts <- tidyr::unite(counts, "ID", !!! .dots)
  counts <- dplyr::select(counts, .data$ID, .data$n)

  # merge counts to population data
  p <- dplyr::left_join(p, counts, by = "ID")

  # create weight variable
  p <- dplyr::mutate(p, !! surv_weight := !! population / .data$n)
  p <- dplyr::select(p, .data$ID, !! surv_weight)


  # merge to study sample
  merge_by <- "ID"
  names(merge_by) <- as.character(rlang::expr(surv_weight_ID))
  d <- tidyr::unite(x, !! surv_weight_ID, !!! .dots, remove = FALSE)
  d <- dplyr::left_join(d, p, by = merge_by)
  d


}
