#' Add a column of survey weights to a data frame.
#' For use in surveys where you took a sample population out of a larger
#' source population.
#'
#' NOTES TO OURSELVES:
#' We want to have two options for weighting:
#'
#' - Strata: What we already have in previous version.
#'           Just want to weight each person based on the source population.
#'           E.g. if we want to stratify our analysis based on age and sex in two
#'           different camps. Then we need the population breakdown by age and sex
#'           in those two camps. Then the function just creates a multiplier,
#'           by divding the number from the source population by the number in
#'           the sample population.
#'
#' - Cluster: This is more complex. Cluster sampling involves first choosing a
#'           set of cluster from a list of all clusters (normally villages in an area),
#'           then within those villages choosing houses (theres several methods for this),
#'           and finally at the household level you either interview one person
#'           or everyone in that household.
#'           Your population dataset, instead of being population counts as in the
#'           above strata example, is a list of all clusters in the area with
#'           the number of houses within each.
#'           Your survey dataset is collected with two levels, one for household
#'           and one for individual. But those two levels are merged, so for
#'           each individual there are two variables included: number of people in
#'           household, and number of people surveyed.
#'           To calculate the weight for each individual, you multiply the inverses
#'           (i.e. 1 over) the probability of: a cluster being chosen,
#'           a household being chosen in that cluster, an individual being chosen
#'           in their household.
#'
#'
#' @param x a data frame of survey data
#' @param p a data frame containing popuplation data for groups in `...`
#' @param c a data frame containing a list of clusters and the number of
#' households in each. Default is NULL as the default `method` is "stratified".
#' @param ... shared grouping columns across both `x` and `p`. These are used
#'   to match the weights to the correct subset of the pouplation.
#' @param population the column in `p` that defines the pouplation numbers
#' @param cluster_c the column in `c` that lists all possible clusters.
#' Ignored if `method` is not "cluster" or if `ignore_cluster`
#' is TRUE.
#' @param household_c the column in `c` that lists the number of households per cluster.
#' Ignored if `method` is not "cluster" or if `ignore_household`
#' is TRUE.
#' @param cluster_x the column in `x` that indicates which cluster rows belong to
#' Ignored if `method` is not "cluster" or if `ignore_cluster`
#' is TRUE.
#' @param household_x the column in `x` that indicates a unique household identifier.
#' Ignored if `method` is not "cluster or if `ignore_household` is TRUE.
#' @param individuals_eligible_x the column in `x` which specifies the number of people
#' eligible for being interviewed in that household. (e.g. the total number of children)
#' Default is NULL as the default `method` is "stratified".
#' @param individuals_interviewed_x the column in `x` which specifies the number of people
#' actually interviewed in that household. (if not available, can also simply be
#' a `household_x` - in this case the function will calculate counts).
#' Default is NULL as the default `method` is "stratified".
#' @param method what type of survey method you would like to use. Options are
#' "stratified" (default) and "cluster".
#' Stratified: takes variables grouped population counts from `p` and divides
#' the corresponding groups by counts of variables in `...` in `x`, to create a weight.
#' Cluster: Will multiply the inverse chances of a cluster being selected, a household
#' being selected within a cluster, and an individual being selected within a household.
#' As follows:
#' unique(cluster_c) / unique(cluster_x) *
#' unique(household_c) / unique(household_x) *
#' individuals_eligible_x / individuals_interviewed_x
#' In the case where ignore_cluster and ignore_household are TRUE, this will simply be:
#' 1 * 1 * individuals_eligible_x / inidivudals_interviewed_x
#' @param ignore_cluster If TRUE, set the weight for clusters to be 1. This assumes that
#' your sample was taken in a way which is a close approximation of a simple random sample.
#' Ignores inputs from `cluster_c` as well as `cluster_x`.
#' Default is TRUE, as the default `method` is "cluster".
#' @param ignore_household If TRUE, set the weight for households to be 1. This assumes that
#' your sample of households was takenin a way which is a close approximation
#' of a simple random sample. Ignores inputs from `household_c` and `household_x`
#' @param surv_weight the name of the new column to store the weights. Defaults to
#'   "surv_weight".
#' @param surv_weight_ID the name of the new ID column to be created. Defaults to
#'   "surv_weight_ID"
#' @author Zhian N. Kamvar Alex Spina Lukas Richter
#' @export


add_weights <- function(x, p, c = NULL, ... , population, cluster_c, household_c,
            cluster_x, household_x, individuals_eligible_x = NULL, individuals_interviewed_x = NULL,
            method = "stratified", ignore_cluster = TRUE, ignore_household = TRUE,
            surv_weight = "surv_weight", surv_weight_ID =  "surv_weight_ID") {

  # stratified method is straight forward - lifted directly from old function version
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

