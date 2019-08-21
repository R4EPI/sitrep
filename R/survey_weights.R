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
#'           number of clusters from a list of all clusters (e.g. pick a few villages
#'           out of all villages in an area),
#'           then within those villages choosing houses (theres several methods for this),
#'           and finally at the household level you either interview one person
#'           or everyone in that household.
#'           So the population no longer matters,
#'           instead of being population counts as in the above stratified example,
#'           you have a list of all clusters in the area with the number of houses
#'           within each cluster.
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
#' 
#' @param p a data frame containing popuplation data for groups in `...`
#' 
#' @param cz a data frame containing a list of clusters and the number of
#'   households in each. Default is NULL as the default `method` is "stratified".
#' 
#' @param ... shared grouping columns across both `x` and `p`. These are used
#'   to match the weights to the correct subset of the pouplation.
#'   Used only when `method` is "stratified".
#' 
#' @param population the column in `p` that defines the pouplation numbers
#' 
#' @param cluster_cz the column in `cz` that lists all possible clusters.
#'   Ignored if `method` is not "cluster" or if `ignore_cluster` is TRUE.
#' 
#' @param household_cz the column in `cz` that lists the number of households per cluster.
#'   Ignored if `method` is not "cluster" or if `ignore_household` is TRUE.
#' 
#' @param cluster_x the column in `x` that indicates which cluster rows belong to.
#'   Ignored if `method` is not "cluster" or if `ignore_cluster` is TRUE.
#' 
#' @param household_x the column in `x` that indicates a unique household identifier.
#'   Ignored if `method` is not "cluster or if `ignore_household` is TRUE.
#' 
#' @param individuals_eligible_x the column in `x` which specifies the number of people
#'   eligible for being interviewed in that household. (e.g. the total number of children)
#' 
#' @param individuals_interviewed_x the column in `x` which specifies the number of people
#'   actually interviewed in that household.
#' 
#' @param method what type of survey method you would like to use. Options are
#'   "stratified" (default) and "cluster".
#'   - Stratified: takes variables grouped population counts from `p` and divides
#'     the corresponding groups by counts of variables in `...` in `x`, to create a weight.
#'   - Cluster: Will multiply the inverse chances of a cluster being selected, a household
#'     being selected within a cluster, and an individual being selected within a household.
#' As follows:
#' unique(cluster_cz) / unique(cluster_x) *
#' unique(household_cz) / unique(household_x) *
#' individuals_eligible_x / individuals_interviewed_x
#' In the case where ignore_cluster and ignore_household are TRUE, this will simply be:
#' 1 * 1 * individuals_eligible_x / inidivudals_interviewed_x
#' 
#' @param ignore_cluster If TRUE, set the weight for clusters to be 1. This
#'   assumes that your sample was taken in a way which is a close approximation
#'   of a simple random sample.  Ignores inputs from `cluster_cz` as well as
#'   `cluster_x`.  Default is TRUE, as the default `method` is "stratified".
#' 
#' @param ignore_household If TRUE, set the weight for households to be 1. This
#'   assumes that your sample of households was takenin a way which is a close
#'   approximation of a simple random sample. Ignores inputs from `household_cz`
#'   and `household_x`.  Default is TRUE, as the default `method` is "stratified".
#' 
#' @param surv_weight the name of the new column to store the weights. Defaults to
#'   "surv_weight".
#' 
#' @param surv_weight_ID the name of the new ID column to be created. Defaults to
#'   "surv_weight_ID"
#' 
#' @author Zhian N. Kamvar Alex Spina Lukas Richter
#' @export
#' @examples
#'
#'
#' # define datasets
#' x <- tibble::tribble(
#'   ~cluster, ~household_id, ~eligibile_n, ~surveyed_n, ~individual_id, ~age_grp, ~sex, ~outcome,
#'   "Village A",             1,            6,           4,              1,   "0-10",  "M",      "Y",
#'   "Village A",             1,            6,           4,              2,  "20-30",  "F",      "Y",
#'   "Village A",             1,            6,           4,              3,  "30-40",  "M",      "N",
#'   "Village A",             1,            6,           4,              4,  "50-60",  "F",      "N",
#'   "Village A",             2,            6,           4,              4,  "50-60",  "F",      "N",
#'   "Village B",             2,            3,           3,              1,  "20-30",  "M",      "N",
#'   "Village B",             2,            3,           3,              2,  "50-60",  "F",      "N",
#'   "Village B",             2,            3,           3,              3,  "30-40",  "F",      "Y"
#' )
#'
#'
#' cz <- tibble::tribble(
#'   ~cluster, ~n_houses,
#'   "Village A",        23,
#'   "Village B",        42,
#'   "Village C",        56,
#'   "Village D",        38
#' )
#'


add_weights <- function(x, p, cz = NULL, ... , population,
                        cluster_cz, household_cz,
                        cluster_x, household_x, individuals_eligible_x, individuals_interviewed_x,
                        method = "stratified",
                        ignore_cluster = TRUE, ignore_household = TRUE,
                        surv_weight = "surv_weight", surv_weight_ID =  "surv_weight_ID") {

  # stratified method is straight forward - lifted directly from old function version
  if (method == "stratified") {
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
  }  else {

    # cluster method

    # define vars
    clus_id_cz <- tidyselect::vars_select(colnames(cz), {{cluster_cz}})
    hh_id_cz   <- tidyselect::vars_select(colnames(cz), {{household_cz}})
    clus_id_x <- tidyselect::vars_select(colnames(x), {{cluster_x}})
    hh_id_x   <- tidyselect::vars_select(colnames(x), {{household_x}})
    indiv_eli   <- tidyselect::vars_select(colnames(x), {{individuals_eligible_x}})
    indiv_surv   <- tidyselect::vars_select(colnames(x), {{individuals_interviewed_x}})


    if (ignore_cluster) {
      cluster_chance <- 1
    } else {
      ## chance of a cluster being selected
      # the total number of clusters in the area
      n_clusters_total  <- summarise(cz, nclus_tot = sum(!duplicated(clus_id_cz)))
      # the number of clusters that were surveyed
      n_clusters_chosen <- summarise(x, nclus = sum(!duplicated(clus_id_x)))
      # inverse chance of a cluster being chosen
      cluster_chance <- as.numeric(n_clusters_total / n_clusters_chosen)
    }

    if (ignore_household) {
      c$hh_chance <- 1
    } else {
      ## chance of a household being selected in each cluster
      # number of households chosen in each cluster
      n_households_chosen <- summarise(
        group_by(x, cluster_id_x),
        n_hh = sum(!duplicated(hh_id_x))
        )

      # join counts of households surveyed to list of clusters
      cz <- left_join(cz, n_households_chosen, by = c(clus_id_cz = clus_id_x))

      # inverse chance of a household being chosen in each cluster
      cz <- mutate(cz, hh_chance = hh_id_cz / n_hh)
    }


    # multiply cluster chance and household chance
    cz$clus_hh_chance <- cz$hh_chance * czluster_chance



    ## chance of an individual being selected in their household
    # create a subset - so we dont merge too many vars in later
    temp <- select(x, clus_id_x,
                   hh_id_x,
                   indiv_eli,
                   indiv_surv)

    # inverse chance of individual being chosen in their household
    temp <- mutate(temp, indiv_chance = indiv_eli / indiv_surv)

    # add in the cluster_household chance
    temp <- left_join(temp, cz, by = c(clus_id_x = clus_id_cz))

    # create final weight by multiplying indivdual chance by cluster_household chance
    temp[[surv_weight]] <- temp$indiv_chance * temp$clus_hh_chance

    # create a survey weight id by merging cluster and household id
    temp <- tidyr::unite(temp, {{surv_weight_ID}}, clus_id_x, hh_id_x)

    # only keep the weight and the weight id
    temp <- select(temp, {{surv_weight}}, {{surv_weight_ID}})

    # bind weights on to original dataset
    d <- bind_cols(x, temp)

    # return new dataset with weights
    d
  }


}

