#' Add a column of cluster survey weights to a data frame.
#' For use in surveys where you took a sample population out of a larger
#' source population, with a cluster survey design.
#'
#' Will multiply the inverse chances of a cluster being selected, a household
#'     being selected within a cluster, and an individual being selected within a household.
#' As follows:
#' (unique(cluster_cz) / unique(cluster_x)) *
#' (unique(household_cz) / unique(household_x)) *
#' (individuals_eligible_x / individuals_interviewed_x)
#'
#' In the case where ignore_cluster and ignore_household are TRUE, this will simply be:
#' 1 * 1 * individuals_eligible_x / inidivudals_interviewed_x
#'
#' @param x a data frame of survey data
#'
#' @param cz a data frame containing a list of clusters and the number of
#'   households in each. Default is NULL as the default `method` is "stratified".
#'
#' @param cluster_cz the column in `cz` that lists all possible clusters.
#'   Ignored if `ignore_cluster` is TRUE.
#'
#' @param household_cz the column in `cz` that lists the number of households per cluster.
#'   Ignored if `ignore_household` is TRUE.
#'
#' @param cluster_x the column in `x` that indicates which cluster rows belong to.
#'   Ignored if `ignore_cluster` is TRUE.
#'
#' @param household_x the column in `x` that indicates a unique household identifier.
#'   Ignored if `ignore_household` is TRUE.
#'
#' @param individuals_eligible_x the column in `x` which specifies the number of people
#'   eligible for being interviewed in that household. (e.g. the total number of children)
#'
#' @param individuals_interviewed_x the column in `x` which specifies the number of people
#'   actually interviewed in that household.
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
#'
#' # define a fake dataset of cluster listings
#' # including cluster names and number of households
#' cz <- tibble::tribble(
#'   ~cluster, ~n_houses,
#'   "Village A",        23,
#'   "Village B",        42,
#'   "Village C",        56,
#'   "Village D",        38
#' )
#'
#'
#' # add weights to a cluster sample
#' # include weights for cluster, household and individual levels
#' add_weights_cluster(x, cz = cz,
#' cluster_cz = cluster, household_cz = n_houses,
#' cluster_x = cluster, household_x = household_id,
#' individuals_eligible_x = eligibile_n, individuals_interviewed_x = surveyed_n,
#' ignore_cluster = FALSE, ignore_household = FALSE)
#'
#'
#' # add weights to a cluster sample
#' # ignore weights for cluster and household level (set equal to 1)
#' # only include weights at individual level
#' add_weights_cluster(x, cz = cz,
#' cluster_cz = cluster, household_cz = n_houses,
#' cluster_x = cluster, household_x = household_id,
#' individuals_eligible_x = eligibile_n, individuals_interviewed_x = surveyed_n,
#' ignore_cluster = TRUE, ignore_household = TRUE)
#'


add_weights_cluster <- function(x, cz = NULL,
                        cluster_cz, household_cz,
                        cluster_x, household_x, individuals_eligible_x, individuals_interviewed_x,
                        ignore_cluster = TRUE, ignore_household = TRUE,
                        surv_weight = "surv_weight", surv_weight_ID =  "surv_weight_ID") {

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
      n_clusters_total  <- length(unique(cz[[clus_id_cz]]))
      # the number of clusters that were surveyed
      n_clusters_chosen <- length(unique(x[[clus_id_x]]))
      # inverse chance of a cluster being chosen
      cluster_chance <- n_clusters_total / n_clusters_chosen
    }

    if (ignore_household) {
      cz$hh_chance <- 1
    } else {
      ## chance of a household being selected in each cluster
      # number of households chosen in each cluster
      n_households_chosen <- dplyr::summarise(
        dplyr::group_by(x, {{cluster_x}}),
        n_hh = sum(!duplicated({{household_x}}))
        )

      # join counts of households surveyed to list of clusters
      cz <- dplyr::left_join(cz, n_households_chosen, by = setNames(clus_id_x, clus_id_cz))

      # inverse chance of a household being chosen in each cluster
      cz[["hh_chance"]] <- cz[[hh_id_cz]] / cz[["n_hh"]]
    }


    # multiply cluster chance and household chance
    cz$clus_hh_chance <- cz$hh_chance * cluster_chance



    ## chance of an individual being selected in their household
    # create a subset - so we dont merge too many vars in later
    temp <- dplyr::select(x, clus_id_x,
                   hh_id_x,
                   indiv_eli,
                   indiv_surv)

    # inverse chance of individual being chosen in their household
    temp[["indiv_chance"]] <- temp[[indiv_eli]] / temp[[indiv_surv]]

    # add in the cluster_household chance
    temp <- dplyr::left_join(temp, cz, by = setNames(clus_id_cz, clus_id_x))

    # create final weight by multiplying indivdual chance by cluster_household chance
    temp[[surv_weight]] <- temp$indiv_chance * temp$clus_hh_chance

    # create a survey weight id by merging cluster and household id
    temp <- tidyr::unite(temp, {{surv_weight_ID}}, clus_id_x, hh_id_x)

    # only keep the weight and the weight id
    temp <- dplyr::select(temp, {{surv_weight}}, {{surv_weight_ID}})

    # bind weights on to original dataset
    d <- dplyr::bind_cols(x, temp)

    # return new dataset with weights
    d

}

